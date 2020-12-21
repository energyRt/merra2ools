
#' Quick plots of wind speed and its dynamics
#'
#' @param merra 
#' @param name 
#' @param mult 
#' @param limits 
#' @param legend.position 
#' @param datetime.position 
#' @param datetime.format 
#'
#' @return
#' @export
#'
#' @examples
wind_plot <- function(merra, name = "W50M", mult = 1, 
                      limits = c(0, 35), 
                      palette = "RdYlBu", direction = -1,
                      legend.position = c(0.91, 0.05),
                      legend.name = "m/s",
                      datetime.position = c(150, 87),
                      datetime.format = "%Y-%b-%d, %Hh %Z"
                      # gif = FALSE 
                      # gif.file = "merra_wind.gif",
                      # gif.nmax = 24, fps = 12,
                      # gif.width = 1920/3, gif.height = 1080/3
                      ) {
  if (!is.data.frame(merra)) stop("'merra' must be a data.frame with MERRA-2 subset")
  if (is.null(merra[[name]])) stop("Column '", name, "' is not found")
  if (is.null(merra[["lon"]]) | is.null(merra[["lat"]])) {
    if (!is.null(merra[["locid"]])) {
      warning("Adding 'lon' and  'lat' from 'locid'")
      lid <- select(merra2ools::locid, 1:3)
      merra <- right_join(lid, merra)
    } else {
      stop("Data doesn't have lat, lon or lod_id")
    }
  }
  gif = FALSE 
  # browser()
  merra$mps <- merra[[name]]*mult
  merra$mps[merra$mps < limits[1]] <- limits[1]
  merra$mps[merra$mps > limits[2]] <- limits[2]
  dat_time <- unique(merra$datetime) %>% sort()
  if (length(dat_time) > 1) {
    if (!gif) {
      warning("Multiple datetime values found, using the earliest one: ", dat_time[1])
      ii <- merra$datetime == dat_time[1]
    } else {
      ii <- merra$datetime %in% dat_time[1:gif_nmax]
    }
    merra <- merra[ii,]
  }
  
  merra <- select(merra, lon, lat, datetime, mps)
  merra$label <- format(merra$datetime, datetime.format)
  
  pp <-
    ggplot(merra, aes(lon, lat)) + 
    geom_raster(aes(fill = mps), alpha = 1, interpolate = F, 
                show.legend = !is.null(legend.position)) +
    scale_fill_distiller(palette = palette, direction = direction, 
                         limits = limits, name = legend.name) +
    theme_void() +
    # theme(panel.grid = element_blank(), panel.border = element_blank()) +
    scale_x_continuous(expand = c(0.00, 0.00)) +
    scale_y_continuous(expand = c(0.00, 0.00)) +
    theme(legend.position = legend.position,
          legend.direction = "horizontal", legend.box = "horizontal",
          # plot.title = element_text(hjust = 0.5), 
          # plot.subtitle = element_text(hjust = 0.5),
          # legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.box.background = element_rect(size = 0.0, 
                                               colour = NA, 
                                               fill = alpha('white', .2)),
          legend.margin = margin(3, 3, 3, 3),
          legend.title = element_text(vjust = .8))

  # if (gif) {
    # # library(gganimate)
    # pp <- pp + 
    #   gganimate::transition_states(states = label, transition_length = 0) +
    #   geom_label(
    #     # data = data.frame( x = 153, y = 87, label = merra$label[1]), 
    #     aes(x = 153, y = 87, label = '{closest_state}'), label.size = 0, color = "black",
    #     fill = "white", alpha = 0.2)
    # pp <- animate(p, renderer = gifski_renderer(), fps = fps, 
    #               nframes = length(unique(merra$datetime)), 
    #               width = 1024, height = 1024*1080/1920)

  # } else {
    # browser()
    if (!is.null(datetime.position) & all(!is.na(datetime.position))) {
      pp <- pp + 
        geom_label(
          data = data.frame(x = datetime.position[1], 
                            y = datetime.position[2],
                            label = merra$label[1]),
          aes(x = x, y = y, label = label), label.size = 0, color = "black",
          fill = "white", alpha = 0.2)
    }
    return(pp)
  # }
}

if (F) {
  wind_plot(dat2, "w50", 1)
  wind_plot(dat2, "w200", 1)
  wind_plot(dat2, "w50", 1, limits = c(0, 30))
  wind_plot(dat2, "w50", 1, limits = c(0, 50))
  wind_plot(dat2, "w50", 1, datetime.position = NULL, legend.position = NULL)
  # a <- wind_plot(dat2, "w50", 1, gif = T, gif_nmax = 48)
}


#' Quick GIF plots
#'
#' @param merra 
#' @param name 
#' @param mult 
#' @param filename 
#' @param nmax 
#' @param fps 
#' @param gif.width 
#' @param gif.height 
#' @param limits 
#' @param legend.position 
#' @param datetime.position 
#' @param datetime.format 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
wind_GIF <- function(merra, name = "W50M", mult = 1, 
                     filename = "merra_wind.gif", 
                     nmax = 48,
                     fps = 24,
                     gif.width = 1920/3, gif.height = 1080/3,
                     limits = c(0, 35), 
                     palette = "RdYlBu", direction = -1,
                     legend.position = c(0.91, 0.05),
                     legend.name = "m/s",
                     datetime.position = c(149, 87),
                     datetime.format = "%Y-%b-%d, %Hh %Z",
                     verbose = TRUE) {
  # browser()
  # dr <- dirname(filename)
  # if (!dir.exists(dr)) dir.create(dr, recursive = TRUE)
  frames <- unique(merra$datetime) %>% sort()
  nframes <- min(length(frames), nmax)
  # browser()
  animation::saveGIF({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      ii <- merra$datetime == frames[i]
      a <- wind_plot(merra[ii,], name = name, mult = mult,
                     limits = limits, palette = palette, direction = direction,
                     legend.name = legend.name,
                     legend.position = legend.position,
                     datetime.position = datetime.position,
                     datetime.format = datetime.format)
      if (i == nframes) {
        if (verbose) cat(" -> creating GIF\n")
      } else {
        if (verbose) cat(rep("\b", nchar(nframes) + 1), sep = "")
      }
      print(a)
    }
    }, 
    interval = 1/fps, ani.width = gif.width, ani.height = gif.height, 
    movie.name = filename
  )

}

if (F) {
  wind_GIF(dat2, "w10", 1, fps = 24, filename = "merra_wind_10m_24fps.gif")
  wind_GIF(dat2, "w50", 1, fps = 24, filename = "merra_wind_50m_24fps.gif")
  wind_GIF(dat2, "w50", 1, fps = 12, filename = "merra_wind_50m_12fps.gif")
  wind_GIF(dat2, "w100", 1, fps = 12, filename = "merra_wind_100m_12fps.gif")
  wind_GIF(dat2, "w200", 1, fps = 6, filename = "merra_wind_200m_6fps.gif")
  
}
