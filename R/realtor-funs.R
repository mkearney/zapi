

## search realtor
search_realtor <- function(p = 1,
                           price = 1e5:6e5,
                           sqft = 1000:6000,
                           bed = 2,
                           bath = 1,
                           basement = TRUE) {
  if (basement) {
    bsmt <- "/features-bmg2"
  } else {
    bsmt <- ""
  }
  op <- getOption("scipen")
  on.exit(options(scipen = op), add = TRUE)
  options(scipen = 6)

  plo <- min(price)
  if (length(price) == 1) {
    phi <- ""
  } else {
    phi <- max(price)
  }
  slo <- min(sqft)
  if (length(sqft) == 1) {
    shi <- ""
  } else {
    shi <- max(sqft)
  }
  url <- "https://www.realtor.com/realestateandhomes-search/Columbia_MO/" %PP%
    "beds-{bed}/baths-{bath}/type-single-family-home/price-{plo}-{phi}/" %PP%
    "sqft-{slo}-{shi}/age-0+/sby-6/pg-{p}{bsmt}?" %PP%
    "pos=38.853345,-92.461548,38.96288,-92.347994,13"
  tryCatch(webble(url), error = function(e) NULL)
}

unq <- function(x) unique(unlist(x, use.names = FALSE))

gjd <- function(x) {
  j <- wibble::parse_json(x)
  kp <- dapr::vap_lgl(j, ~ all(c("address", "st", "unitname") %in% names(.x)))
  if (sum(kp) == 0) return(NULL)
  d <- j[[which(kp)[1]]]
  tibble::as_tibble(d)
}
lap_time <- function(., .f, ..., time = 0.5) {
  if (is.language(.f)) {
    .f <- .f[[2]]
  } else {
    .f <- call(as.character(substitute(.f)), quote(.x))
  }
  .o <- vector("list", length(.))
  for (i in seq_along(.)) {
    .o[[i]] <- eval(.f, list(.x = .[[i]]))
    if (is.null(.o[[i]])) break
    if (i < length(.)) {
      tfse::print_complete(i)
      Sys.sleep(time)
    }
  }
  .o
}
get_homes <- function(.x) {
  .x %>%
    rvest::html_nodes(".component_property-card") %>%
    rvest::html_attr("data-url") %>%
    tfse::na_omit() %>%
    sprintf("https://www.realtor.com/realestateandhomes-detail%s", .) %>%
    unique()
}

x_mat <- function(.x) {
  .x <- .x[, !names(.x) %in% c("zpid", "adress1", "pred", grep("zest|pred", names(.x), value = TRUE))]
  chr <- dapr::vap_lgl(.x, is.character)
  lg <- dapr::vap_lgl(.x, ~ all(tfse::na_omit(.x) %in% c("TRUE", "FALSE")))
  .x[lg & chr] <- dapr::lap(.x[lg & chr], as.logical)
  lg <- dapr::vap_lgl(.x, is.logical)
  .x[lg] <- dapr::lap(.x[lg], as.integer)
  .x[] <- dapr::lap(.x, fct_num)
  .x <- .x[dapr::vap_lgl(.x, ~ is.numeric(.x) | is.factor(.x) | is.logical(.x))]
  kp <- unlist(lapply(.x, function(y) var(as.numeric(y), na.rm = TRUE))) > 0
  .x <- .x[, kp]
  .x[grep("id$", names(.x))] <- lap(.x[grep("id$", names(.x))], as.character)
  .x[grep("id$", names(.x))] <- lap(.x[grep("id$", names(.x))], ~ {
    ifelse(is.na(.x), "NA", .x)
  })
  .x <- model.matrix(listingprice ~ ., .x)
  .x <- as.matrix(.x)
  ## , "pldp250K_500K,85K,250K,300K", "pldp250K_500K,85K,250K"
  .x <- .x[, !colnames(.x) %in% c("listingprice")]
  .x
}
pred_mat <- function(.x) {
  .x <- x_mat(.x)
  uq_cols(.x)
}
nd_mat <- function(.x) {
  x_mat(.x)
}
fct_num <- function(.x) {
  if (all(is.na(.x))) {
    return(as.character(.x))
  }
  if (!is.character(.x)) {
    return(.x)
  }
  .x[is.na(.x)] <- "NA"
  if (tfse::n_uq(.x) == 2) {
    .x <- .x == unique(.x)[1]
  } else if (tfse::n_uq(.x) > 2 && tfse::n_uq(.x) < (.8 * length(.x))) {
    .x <- factor(.x)
  }
  .x
}

uq_cols <- function(x) {
  r <- suppressWarnings(cor(x, use = "pairwise.complete.obs"))
  for (i in seq_len(ncol(r))) {
    if (i == 1) next
    di <- which(r[[i]] == 1 & !is.na(r[[i]]))
    di <- nin(di, i)
    if (length(di) > 0) {
      r <- r[, -di]
    }
    if (i >= ncol(r)) break
  }
  x <- x[, colnames(r)]
  x[, apply(x, 2, var, na.rm = TRUE) > 0]
}


uq_rows <- function(x) {
  r <- suppressWarnings(cor(t(x), use = "pairwise.complete.obs"))
  rows <- apply(r, 1, function(.x) sort(abs(.x), decreasing = TRUE)[2] < 1.0)
  x[rows, ]
}
as_tbl <- function(.x) tbltools::bind_rows_data(lapply(.x, gjd), fill = TRUE)
safe_webble <- function(.x) tryCatch(webble(.x), error = function(e) NULL)

is_numish <- function(x) {
  if (!is.character(x)) return(FALSE)
  if (!all(grepl("^$|^-?(\\,|\\.|\\d)+$", x))) return(FALSE)
  if (all(grepl("^$|^\\d+$", x)) && max(nchar(x), na.rm = TRUE) > 9) {
    return(FALSE)
  }
  TRUE
}
as_num <- function(x) {
  x <- gsub("\\,", "", x)
  if (all(grepl("^-?\\d+$", x))) {
    suppressWarnings(as.integer(x))
  } else {
    suppressWarnings(as.numeric(x))
  }
}



## get zillow ID
get_zpid <- function(address, zip) {
  z <- tibble::tibble(
    address = address,
    zipldp = zip,
    zpid = NA_character_
  )
  address <- trim_ws(gsub("_|-|,", " ", address))
  url <- "http://www.zillow.com/webservice/GetSearchResults.htm?zws-id=" %P%
    Sys.getenv("ZILLOW_PAT") %P%
    "&address=" %P% URLencode(address, reserved = TRUE) %P%
    "&citystatezip=" %P% zip
  res <- httr::GET(url)
  res <- httr::content(res, encoding = "UTF-8")
  if (zapi_limit_warning(res)) {
    cat("Limit warning received!", fill = TRUE)
    while (zapi_limit_warning(res)) {
      cat("Sleeping 60 seconds...", fill = TRUE)
      Sys.sleep(60)
      res <- httr::GET(url)
      res <- httr::content(res, encoding = "UTF-8")
    }
  }
  zpid <- tryCatch({rvest::html_text(
    rvest::html_node(res, "zpid"))},
    error = function(e) NA_character_)
  z$zpid <- zpid
  z
}

## get Zestimate
get_zest <- function(z) {
  z <- tibble::tibble(
    address = z$address,
    zipldp = z$zipldp,
    zpid = z$zpid,
    lat = NA_real_,
    lng = NA_real_,
    zest = NA_real_,
    zest_update = as.Date(NA_character_),
    zest_pctl = NA_real_,
    localrealestate_links = NA_character_,
    localrealestate_zindexValue = NA_character_
  )
  if (is.na(z$zpid)) {
    return(z)
  }
  url <- "http://www.zillow.com/webservice/GetZestimate.htm?zws-id=" %P%
    Sys.getenv("ZILLOW_PAT") %P%
    "&zpid=" %P% z$zpid
  res <- httr::GET(url)
  res <- httr::content(res, encoding = "UTF-8")
  if (zapi_limit_warning(res)) {
    cat("Limit warning received!", fill = TRUE)
    while (zapi_limit_warning(res)) {
      cat("Sleeping 60 seconds...", fill = TRUE)
      Sys.sleep(60)
      res <- httr::GET(url)
      res <- httr::content(res, encoding = "UTF-8")
    }
  }
  as_numeric <- function(x) if (length(x) == 0) NA_real_ else as.numeric(x)
  as_date <- function(x, format) {
    if (length(x) == 0) {
      as.Date(NA_character_)
    } else {
      as.Date(x, format = format)
    }
  }
  r <- tryCatch(xml2::as_list(res),
    error = function(e) NULL)
  r <- r[["zestimate"]][["response"]]
  if (is.null(r)) return(z)
  as_a_tbl <- function(x) {
    x <- dapr::lap(x, ~ {
      if (length(.x) == 0) {
        return(NA)
      }
      while (is.recursive(.x) || length(.x) > 1) {
        .x <- .x[[1]]
        if (length(.x) == 0) {
          return(NA)
        }
      }
      .x
    })
    names(x) <- gsub("-| ", "_", names(x))
    tibble::as_tibble(x)
  }
  if (length(r$localRealEstate$region) > 0) {
    names(r$localRealEstate$region) <- paste0("localrealestate_", names(r$localRealEstate$region))
    localrealestate <- as_a_tbl(r$localRealEstate$region)
  } else {
    localrealestate <- data.frame()
  }
  if (length(localrealestate[["localrealestate_zindexValue"]]) == 1 &&
      !is.na(localrealestate[["localrealestate_zindexValue"]])) {
    z$localrealestate_zindexValue <- localrealestate$localrealestate_zindexValue
  }
  if (length(localrealestate[["localrealestate_links"]]) == 1 &&
      !is.na(localrealestate[["localrealestate_links"]])) {
    z$localrealestate_links <- localrealestate$localrealestate_links
  }

  z$lat <- tryCatch(as_numeric(
    r$address$latitude[[1]][[1]]),
    error = function(e) NA_real_)
  z$lng <- tryCatch(as_numeric(
    r$address$longitude[[1]][1]),
    error = function(e) NA_real_)
  z$zest <- tryCatch(as_numeric(
    r$zestimate$amount[[1]][1]),
    error = function(e) NA_real_)
  z$zest_update <- tryCatch(as_date(
    r$zestimate[[2]][[1]][1],
    format = "%m/%d/%Y"),
    error = function(e) as.Date(NA_character_))
  z$zest_pctl <- tryCatch(as_numeric(
    r$zestimate$percentile[[1]][1]),
    error = function(e) NA_real_)
  z
}

zapi_limit_warning <- function(x) {
  grepl("true", rvest::html_node(x, "limit-warning"), ignore.case = TRUE)
}

zestimate <- function(address, zip) {
  zpid <- get_zpid(address, zip)
  get_zest(zpid)
}

zestimates <- function(d) {
  zest <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    zpid <- get_zpid(d$address[i], d$zipldp[i])
    zest[[i]] <- get_zest(zpid)
    w <- getOption("width")
    if (w > 100) w <- 100
    w <- w - nchar(d$address[i]) - 11
    if (w < 1) w <- 1
    sp <- paste(rep("=", w), collapse = "")
    tfse::print_complete(d$address[i] %P% " " %P% sp %P%
        " [" %P% sprintf("%03d", i) %P%
        "/" %P% sprintf("%03d", nrow(d)) %P% "]")
  }
  dplyr::bind_rows(zest)
}

replace_with_mean <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    x[is.na(x)] <- "NA"
    return(x)
  }
  if (inherits(x, "Date")) {
    m <- mean(as.numeric(x), na.rm = TRUE)
    m <- as.Date(m, origin = "1970-01-01")
    x[is.na(x)] <- m
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    m <- mean(as.numeric(x), na.rm = TRUE)
    m <- as.POSIXct(m, origin = "1970-01-01")
    x[is.na(x)] <- m
    return(x)
  }
  if (!is.numeric(x) || all(!is.na(x))) return(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

is_tf <- function(x) {
  if (!is.character(x)) return(FALSE)
  all(is.na(x) | x %in% c("TRUE", "FALSE"))
}


all_names <- function(x) UseMethod("all_names")

all_names.default <- function(x) {
  NULL
}

all_names.data.frame <- function(x) {
  if (nrow(x) == 0) {
    return(NULL)
  }
  rec <- vap_lgl(x, is.recursive)
  if (sum(rec) == 0) {
    return(names(x))
  }
  c(
    names(x[!rec]),
    unlist(lap(x[rec], all_names))
  )
}

all_names.list <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  rec <- vap_lgl(x, is.recursive)
  if (sum(rec) == 0) {
    return(names(x))
  }
  c(
    names(x[!rec]),
    unlist(lap(x[rec], all_names))
  )
}


zillow_data_frame <- function(x) {
  tibble::tibble(
    zpid = nodes(x, "zpid"),
    homedetails = nodes(x, "links homedetails"),
    graphsanddata = nodes(x, "links graphsanddata"),
    mapthishome = nodes(x, "links mapthishome"),
    comparables = nodes(x, "links comparables"),
    street = nodes(x, "address street"),
    zipcode = nodes(x, "address zipcode"),
    city = nodes(x, "address city"),
    state = nodes(x, "address state"),
    latitude = as.numeric(nodes(x, "address latitude")),
    longitude = as.numeric(nodes(x, "address longitude")),
    zest  = as.numeric(nodes(x, "zestimate amount")),
    zest_date  = as.Date(nodes(x, "zestimate last-updated"), format = "%m/%d/%Y", origin = "1970-01-01"),
    zest_low  = nodes(x, "zestimate valuationRange low"),
    zest_high  = nodes(x, "zestimate valuationRange high"),
    zest_pct  = as.numeric(nodes(x, "zestimate percentile")),
    #rent_zest = nodes(x, "rentzestimate amount"),
    #rent_zest_date = as.Date(nodes(x, "rentzestimate last-updated"), format = "%m/%d/%Y", origin = "1970-01-01"),
    local_value = as.numeric(gsub(",", "", nodes(x, "localRealEstate region zindexValue"))),
    local_region = nodes(x, "localRealEstate region links overview"),
    fips = nodes(x, "FIPScounty"),
    use_code = nodes(x, "useCode"),
    tax_year = as.integer(nodes(x, "taxAssessmentYear")),
    tax = as.numeric(nodes(x, "taxAssessment")),
    year_built = as.numeric(nodes(x, "yearBuilt")),
    lot_sqft = as.numeric(nodes(x, "lotSizeSqFt")),
    sqft = as.numeric(nodes(x, "finishedSqFt")),
    bath = as.numeric(nodes(x, "bathrooms")),
    bed = as.numeric(nodes(x, "bedrooms")),
    rooms = as.numeric(nodes(x, "totalRooms"))
  )
}

get_deep_search_result <- function(address) {
  address <- URLencode(gsub("_", " ", address))
  zws <- Sys.getenv("ZILLOW_PAT")
  csz <- URLencode("Columbia MO 65203")
  "https://www.zillow.com/webservice/GetDeepSearchResults.htm?" %PP%
    "zws-id={zws}&address={address}&citystatezip={csz}" %>%
    httr::GET() -> r
  httr::warn_for_status(r)
  r <- httr::content(r, encoding = "UTF-8")
  tryCatch(zillow_data_frame(r),
    error = function(e) NULL)
}

get_deep_search_results <- function(address) {
  d <- dapr::lap(address, get_deep_search_result)
  tryCatch(dplyr::bind_rows(d),
    error = function(e) d)
}

nodes <- function(x, .n) {
  x <- rvest::html_text(rvest::html_nodes(x, .n), trim = TRUE)
  if (length(x) == 0) return(NA_character_)
  x
}
