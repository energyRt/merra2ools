#' Spatial clustering based on correlation or other metrics.
#'
#' @param x `data.frame` (merra subset) with location and time identifiers, and a time-series variable to cluster.
#' @param varname name of column with data to be used to cluster locations.
#' @param locid name of column of location identifiers.
#' @param time name of column with time dimension
#' @param locid_info (optional) `data.frame` or `sf` object with weights and/or spatial groups (regions) of location identifiers.
#' @param weight (optional) name of column with (positive) weights in `locid_info`, used in calculating weighted `mean` and `sd` metrics.
#' @param group (optional) name of column with group-names of locations (such as regions). If provided, clustering will be made for each group separately.
#' @param k (optional) integer vector of number of clusters to test. By default (`NULL`) clustering process start from `1` to the number of locations and terminates when `max_loss` condition is met.
#' @param max_loss maximum loss of variation (standard deviation) of clustered variable, measured as `1 - sd(clustered_variable) / sd(original_variable)`. Default value is `0.05`, meaning up to `5` percent of variability of original, non-clustered variable is allowed to be lost by clustering.  
#' @param verbose logical, should the clustering process be reported, TRUE by default.  
#' @param distance character name of a selected distance measure to use `TSdist::KMedoids`. Default metrics is `cor` - Pearson's correlation between the time series variable in different locations. Alternative, allowed methasures: `"euclidean", "manhattan", "minkowski", "infnorm", "ccor", "sts", "dtw", "keogh_lb", "edr", "erp", "lcss", "fourier", "tquest", "dissimfull", "dissimapprox", "acf", "pacf", "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", "ar.pic", "cdm", "cid", "cor", "cort", "wav", "int.per", "per", "mindist.sax", "ncd", "pred", "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet"`.
#' @param ... additional parameters to pass to `TSdist::KMedoids`, might be required for some distance measures.
#' @param cores integer number of processor cores to use, currently ignored.
#'
#' @return `data.frame` with alternative number of clusters with columns:
#' \describe{
#'   \item{k}{Number of clusters}
#'   \item{N}{Total number of time series}
#'   \item{locid}{location identifier in `merra2ools` datasets}
#'   \item{"group"}{(if provided) column with locid-groups}
#'   \item{cluster}{cluster number in every `k`-group}
#'   \item{weight}{weight of the cluster in the `k`-group}
#'   \item{sd_N}{standard deviation of the whole sample of (N) time-series}
#'   \item{sd_k}{standard deviation of clustered time series with `k` clusters}
#'   \item{sd_loss}{loss of standard deviation as result of clusterisation, for each `k`}
#'  } 
#' @export
#'
#' @examples
#' # see "Cluster locations" in "Get started" 
#' 
cluster_locid <- function(
    x, varname, locid = "locid", time = "UTC",
    locid_info = NULL, weight = NULL, group = NULL,
    k = c(1:20, 25, 30, 40, 50, 75, 100, 150, 200, 300, 500, 1000, 1e4),
    max_loss = .05,
    distance = "cor", 
    cores = 1, 
    plot = FALSE, 
    verbose = TRUE,
    ...) {
  # browser()
  require("TSdist", quietly = T)
  if (is.null(locid_info)) {
    lid <- unique(x[[locid]]) %>% sort()
    stopifnot(is.null(group))
    stopifnot(is.null(weight))
    locid_info <- data.table(
      lid = lid
    )
    locid_info[[locid]] <- locid_info$lid
    locid_info$lid <- NULL
  }
  
  if (is.null(group)) {
    .regs <- "ALL"
  } else {
    .regs <- unique(locid_info[[group]]) %>% sort()
  }
  # return(.regs)
  
  id_class <- class(x[[locid]])
  ii <- x[[locid]] %in% locid_info[[locid]]
  d <- x %>%
    select(any_of(c(group, locid, time, varname))) %>%
    filter(ii)
  rm(ii)
  # filter_at(vars(locid), any_vars(. %in% locid_info[[locid]]))
  # d <- d[,]
  ll <- list() # list by region
  for (r in .regs) {
    cc <- list() # list of alternative clusters of r - region
    # browser()
    if (is.null(group)) {
      id_r <- locid_info
      dd <- d
    } else {
      id_r <- locid_info[locid_info[[group]] %in% r, ]
      ii <- d[[locid]] %in% id_r[[locid]]
      dd <- filter(d, ii)
      rm(ii)
    }
    # browser()
    dd <- dd %>%
      pivot_wider(
        names_from = all_of(locid),
        names_prefix = "id_",
        values_from = all_of(varname)
      ) %>%
      as.data.table() %>%
      data.table::as.xts.data.table()
    if (verbose) {
      message(
        if_else(is.null(group),
                "group (", paste0(group, " (")
        ),
        which(.regs == r), "/", length(.regs), "): ", r,
        ", locations: ", ncol(dd), ", obs.: ", nrow(dd)
      )
    }
    if (nrow(dd) == 0) next
    # aggregate `id_r` by `locid`
    # browser()
    if (!is.null(locid_info)) {
      w <- id_r %>%
        sf::st_drop_geometry() %>%
        select(any_of(c(locid, weight))) %>%
        unique()
    } else {
      w <- data.table(id = unique(d[[locid]]))
      names(w) <- locid
    }
    
    # browser()
    if (!is.null(weight)) {
      w <- w %>%
        rename(w = all_of(weight)) %>%
        group_by(across(any_of(c(locid, group)))) %>%
        summarise(
          w = sum(as.numeric(w), na.rm = T),
          .groups = "drop"
        )
    } else {
      w$w <- 1L
    }
    
    stopifnot(!any(w$w < 0)) # check weights
    w <- filter(w, w > 0) # weighted_* methods don't take zeros
    if (nrow(w) == 0) {
      if (verbose) cat("No observations (zero-weights for all locations in the group)\n")
      next
    }
    
    # browser()
    sd_N <- d %>%
      right_join(sf::st_drop_geometry(w), by = locid)
    # nobs <- sd_N[[varname]] * sd_N$w
    sd_N <- datawizard::weighted_sd(sd_N[[varname]], sd_N$w,
                                       remove_na = T
    )
    if (is.na(sd_N)) {
      if (verbose) cat("The data with applied weight has zero observations\n")
      next
    }
    # browser()
    K <- 1:ncol(dd)
    if (!is.null(k)) K <- unique(c(K[K %in% k], ncol(dd)))
    K <- K[K <= nrow(d)]
    # K[K >= max(1, min(k)) & K <= min(ncol(dd), max(k))]
    
    for (i in K) {
      # browser()
      if (verbose) cat("k-clusters (k-max): ", i, " (", max(K), ")", sep = "")
      if (i < ncol(dd)) {
        k_i <- TSdist::KMedoids(dd, i, distance = distance, ...)
      } else {
        k_i <- factor(names(dd)) %>% as.integer()
      }
      
      cl <- data.table(
        group = r,
        k = i,
        N = ncol(dd), # number of locations (max possible k)
        locid = as(str_replace(names(dd), "^id_", ""), id_class),
        cluster = k_i
      ) %>%
        dplyr::right_join(w, by = locid) %>%
        as.data.table()
      
      if (!is.null(group)) {
        nms <- names(cl)
        nms[nms == "group"] <- group
        names(cl) <- nms
        rm(nms)
      }
      
      d_i <- d %>%
        right_join(sf::st_drop_geometry(cl), by = locid) %>%
        group_by(across(all_of(c(time, group, "cluster")))) %>%
        rename(value = all_of(varname)) %>%
        summarise(
          value = datawizard::weighted_mean(value, as.numeric(w), remove_na = T),
          # value = weighted.mean(value, w, na.rm = T),
          w = sum(as.numeric(w), na.rm = T),
          .groups = "drop"
        )
      sd_i <- datawizard::weighted_sd(d_i[["value"]], as.numeric(d_i[["w"]]),
                                      remove_na = T
      )
      # cat(", sd_k:", sd_N, sd_i, nrow(d_i), 1 - sd_i/sd_N, "\n")
      if (verbose) {
        cat(", sd_k: ", sd_i, ", sd_loss: ",
            100 * round(1 - sd_i / sd_N, 5), "%\n",
            sep = ""
        )
      }
      # browser()
      # cc[[i]] <- list(
      #   cl = cl,
      #   sd_k = sd_i,
      #   sd_loss = 1 - sd_i / sd_N
      # )
      
      cl[, sd_N := sd_N][, sd_k := sd_i][, sd_loss := 1 - sd_i / sd_N]
      if (plot && inherits(locid_info, "sf")) {
        # if (r == "...") browser()
        rf_cl_sf <- locid_info |> 
          filter(get(group) %in% {{r}}) |>
          left_join(cl, by = c(locid, group)) |>
          st_make_valid() |>
          filter(!is.na(st_dimension(geometry))) 
        # (optional) merge geometries for the plot
        #  %>%
        #   group_by(across(any_of(c(group, "cluster")))) |>
        #   summarise(geometry = st_union(geometry), .groups = "drop") |>
        #   st_make_valid() |>
        #   filter(!is.na(st_dimension(geometry)))
        
        # use discrete palette for smaller number of clusters
        if (i <= 12) rf_cl_sf <- mutate(rf_cl_sf, cluster = as.factor(cluster))
        
        plot(rf_cl_sf["cluster"], 
             main = paste0("group: ", r, "; clusters: ", i, " (", cl$N[1],
                           "); sd_loss: ", round(100 * cl$sd_loss[1], 3), "%"),
             nbreaks = i)
      }
      
      # cl[, sd_N := sd_N][, sd_k := sd_i][, sd_loss := 1 - sd_i / sd_N]
      # cl[, max_loss := max_loss]
      cc[[i]] <- cl; rm(cl)
      
      if (1 - sd_i / sd_N <= max_loss) {
        break
      }
    } # K (cluster) loop
    # browser()
    
    cc <- rbindlist(cc)
    
    if (F) { # sort clusters
      d_cl <- group_by(d_i, cluster, across(any_of(group))) %>%
        summarise(
          value = datawizard::weighted_mean(value, as.numeric(w), remove_na = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(value)) %>%
        mutate(sorted = 1:n()) %>%
        select(-value)
      
      cl <- left_join(cl, d_cl, by = c("cluster", group)) %>%
        mutate(cluster = as.integer(sorted)) %>%
        select(-sorted)
    }
    
    # browser()
    ll[[r]] <- as.data.table(cc)
    # ll[[r]] <- list(
    #   cl = cc,
    #   sd_N = sd_N,
    #   sd_min = min(sapply(cc, function(x) x$sd_k), na.rm = T),
    #   max_loss = max_loss
    # )
  } # .regs loop
  # browser()
  tb <- data.table::rbindlist(ll, use.names = T)
  nms <- colnames(tb)
  if (is.null(group)) {
    tb[["group"]] <- NULL
    nms <- colnames(tb)
    # } else {
    # nms[nms == "group"] <- group
  }
  if (!is.null(weight)) nms[nms == "w"] <- weight else nms[nms == "w"] <- "weight"
  colnames(tb) <- nms
  return(tb)
}


# cluster_locid_old <- function(x, varname, locid = "locid", time = "UTC", 
#                           locid_info = NULL, weight = NULL, group = NULL,
#                           k = NULL, max_loss = .05, verbose = TRUE, 
#                           distance = "cor", ...) {
#   # browser()
#   require("TSdist", quietly = T)
#   if (is.null(locid_info)) {
#     lid <- unique(x[[locid]]) %>% sort()
#     stopifnot(is.null(group))
#     stopifnot(is.null(weight))
#     locid_info <- tibble(
#       lid = lid
#     )
#     locid_info[[locid]] <- locid_info$lid; locid_info$lid <- NULL
#   }
#   
#   if (is.null(group)) {
#     .regs <- "ALL"
#   } else {
#     .regs <- unique(locid_info[[group]]) %>% sort()
#   }
#   # return(.regs)
#   
#   id_class <- class(x[[locid]])
#   ii <- x[[locid]] %in% locid_info[[locid]]
#   d <- x %>% 
#     select(any_of(c(group, locid, time, varname))) %>%
#     filter(ii)
#   rm(ii)
#   # filter_at(vars(locid), any_vars(. %in% locid_info[[locid]]))
#   # d <- d[,]
#   ll <- list()
#   for (r in .regs) {
#     # browser()
#     if (is.null(group)) {
#       id_r <- locid_info
#       dd <- d
#     } else {
#       id_r <- locid_info[locid_info[[group]] %in% r,]
#       ii <- d[[locid]] %in% id_r[[locid]]
#       dd <- filter(d, ii); rm(ii)
#     }
#     # browser()
#     dd <- dd %>%
#       pivot_wider(names_from = all_of(locid), 
#                   names_prefix  = "id_",
#                   values_from = all_of(varname)) %>%
#       as.data.table() %>%
#       data.table::as.xts.data.table()
#     if (verbose) message(if_else(is.null(group), 
#                                  "group (", paste0(group, " (")),
#                          which(.regs == r), "/", length(.regs), "): ", r,
#                          ", locations: ", ncol(dd), ", obs.: ", nrow(dd))
#     if (nrow(dd) == 0) next
#     # aggregate `id_r` by `locid`
#     # browser()
#     if (!is.null(locid_info)) {
#       w <- id_r %>%
#         sf::st_drop_geometry() %>%
#         select(any_of(c(locid, weight))) %>%
#         unique()
#     } else {
#       w = data.table(id = unique(d[[locid]]))
#       names(w) <- locid
#     }
#     
#     # browser()
#     if (!is.null(weight)) {
#       w <- w %>%
#         rename(w = all_of(weight)) %>%
#         group_by(across(any_of(c(locid, group)))) %>%
#         summarise(
#           w = sum(as.numeric(w), na.rm = T),
#           .groups = "drop"
#         )
#     } else {
#       w$w <- 1L
#     }
#     
#     stopifnot(!any(w$w < 0)) # check weights
#     w <- filter(w, w > 0) # weighted_* methods don't take zeros
#     if (nrow(w) == 0) {
#       if (verbose) cat("No observations (zero-weights for all locations in the group)\n")
#       next
#     }
#     
#     # browser()
#     sd_N <- d %>%
#       right_join(sf::st_drop_geometry(w), by = locid)
#     # nobs <- sd_N[[varname]] * sd_N$w
#     sd_N <- datawizard::weighted_sd(sd_N[[varname]], sd_N$w, 
#                                        remove_na = T)
#     if (is.na(sd_N)) {
#       if (verbose) cat("The data with applied weight has zero observations\n")
#       next
#     }
#     K <- 1:ncol(dd)
#     if (!is.null(k)) K <- K[K %in% k]
#     K <- K[K <= nrow(d)]
#     # K[K >= max(1, min(k)) & K <= min(ncol(dd), max(k))]
#     
#     for (i in K) {
#       # browser()
#       if (verbose) cat("N clusters: ", i, " (", max(K),")", sep = "")
#       if (i < ncol(dd)) {
#         k_i <- TSdist::KMedoids(dd, i, distance = distance, ...)
#       } else {
#         k_i <- factor(names(dd)) %>% as.integer()
#       }
#       
#       cl = tibble(
#         group = r,
#         locid = as(str_replace(names(dd), "^id_", ""), id_class),
#         cluster = k_i
#       ) %>%
#         right_join(w, by = locid)
#       if (!is.null(group)) {
#         nms <- names(cl)
#         nms[nms == "group"] <- group
#         names(cl) <- nms; rm(nms)
#       }
#       
#       d_i <- d %>%
#         right_join(sf::st_drop_geometry(cl), by = locid) %>%
#         group_by(across(all_of(c(time, group, "cluster")))) %>%
#         rename(value = all_of(varname)) %>%
#         summarise(
#           value = datawizard::weighted_mean(value, as.numeric(w), remove_na = T),
#           # value = weighted.mean(value, w, na.rm = T),
#           w = sum(as.numeric(w), na.rm = T),
#           .groups = "drop"
#         )
#       sd_i <- datawizard::weighted_sd(d_i[["value"]], as.numeric(d_i[["w"]]),
#                                       remove_na = T)
#       # cat(", sd_k:", sd_N, sd_i, nrow(d_i), 1 - sd_i/sd_N, "\n")
#       if (verbose) cat(", sd_k: ", sd_i, ", sd_loss: " , 
#                        100 * round(1 - sd_i/sd_N, 5), "%\n", sep = "")
#       # browser()
#       if (1 - sd_i/sd_N <= max_loss) {break}
#       
#     }
#     # browser()
#     d_cl <- group_by(d_i, cluster, across(any_of(group))) %>% 
#       summarise(
#         value = datawizard::weighted_mean(value, as.numeric(w), remove_na = TRUE),
#         .groups = "drop"
#       ) %>%
#       arrange(desc(value)) %>%
#       mutate(sorted = 1:n()) %>%
#       select(-value)
#     
#     cl <- left_join(cl, d_cl, by = c("cluster", group)) %>%
#       mutate(cluster = as.integer(sorted)) %>%
#       select(-sorted)
#     
#     # browser()
#     ll[[r]] <- as.data.table(cl)
#   }
#   ll <- data.table::rbindlist(ll, use.names = T)
#   nms <- colnames(ll)
#   if (is.null(group)) {
#     ll[["group"]] <- NULL
#     nms <- colnames(ll)
#     # } else {
#     # nms[nms == "group"] <- group
#   }
#   if (!is.null(weight)) nms[nms == "w"] <- weight else nms[nms == "w"] <- "weight"
#   colnames(ll) <- nms
#   return(ll)
# }