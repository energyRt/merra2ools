
#' Quick plots of wind speed and its dynamics
#'
#' @param x 
#' @param name 
#' @param scale 
#' @param limits 
#' @param legend.position 
#' @param datetime.position 
#' @param datetime.format 
#' @param intercept 
#' @param palette 
#' @param direction 
#' @param legend.name 
#' @param expand.x 
#' @param expand.y 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot_merra <- function(x, 
                       name = "W50M", scale = 1, intercept = 0,
                      limits = c(0, 35), 
                      palette = "RdYlBu", direction = -1,
                      legend.position = c(0.91, 0.05),
                      legend.name = "m/s",
                      datetime.position = c(150, 87),
                      datetime.format = "%Y-%b-%d, %Hh %Z",
                      expand.x = c(0., 0.),
                      expand.y = c(0., 0.),
                      # gif = FALSE 
                      # gif.file = "merra_wind.gif",
                      # gif.nmax = 24, fps = 12,
                      # gif.width = 1920/3, gif.height = 1080/3
                      ...
                      ) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame with MERRA-2 subset")
  if (is.null(x[[name]])) stop("Column '", name, "' is not found")
  if (is.null(x[["lon"]]) | is.null(x[["lat"]])) {
    if (!is.null(x[["locid"]])) {
      warning("Adding 'lon' and  'lat' for each 'locid'")
      lid <- select(merra2ools::locid, 1:3)
      x <- right_join(lid, x)
    } else {
      stop("Data doesn't have lat, lon or lod_id")
    }
  }
  gif = FALSE 
  # browser()
  x$mps <- x[[name]]*scale + intercept
  x$mps[x$mps < limits[1]] <- limits[1]
  x$mps[x$mps > limits[2]] <- limits[2]
  dat_time <- unique(x$datetime) %>% sort()
  if (length(dat_time) > 1) {
    if (!gif) {
      warning("Multiple datetime values found, using the earliest one: ", dat_time[1])
      ii <- x$datetime == dat_time[1]
    } else {
      ii <- x$datetime %in% dat_time[1:gif_nmax]
    }
    x <- x[ii,]
  }
  
  x <- select(x, lon, lat, datetime, mps)
  x$label <- format(x$datetime, datetime.format)
  
  pp <-
    ggplot(x, aes(lon, lat)) + 
    geom_raster(aes(fill = mps), alpha = 1, interpolate = F, 
                show.legend = !is.null(legend.position)) +
    scale_fill_distiller(palette = palette, direction = direction,
                         limits = limits, name = legend.name) +
    # scale_fill_viridis_c(direction = direction, option = "inferno",
                         # limits = limits, name = legend.name) +
    theme_void() +
    # theme(panel.grid = element_blank(), panel.border = element_blank()) +
    scale_x_continuous(expand = expand.x) +
    scale_y_continuous(expand = expand.y) +
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
    #     # data = data.frame( x = 153, y = 87, label = x$label[1]), 
    #     aes(x = 153, y = 87, label = '{closest_state}'), label.size = 0, color = "black",
    #     fill = "white", alpha = 0.2)
    # pp <- animate(p, renderer = gifski_renderer(), fps = fps, 
    #               nframes = length(unique(x$datetime)), 
    #               width = 1024, height = 1024*1080/1920)

  # } else {
    # browser()
    if (!(is.null(datetime.position) || is.na(datetime.position))) {
    # } else {
      pp <- pp + 
        geom_label(
          data = data.frame(x = datetime.position[1], 
                            y = datetime.position[2],
                            label = x$label[1]),
          aes(x = x, y = y, label = label), label.size = 0, color = "black",
          fill = "white", alpha = 0.2)
    }
    return(pp)
  # }
}

if (F) {
  plot_merra(dat2, "w50", 1)
  plot_merra(dat2, "w200", 1)
  plot_merra(dat2, "w50", 1, limits = c(0, 30))
  plot_merra(dat2, "w50", 1, limits = c(0, 50))
  plot_merra(dat2, "w50", 1, datetime.position = NULL, legend.position = NULL)
  # a <- plot_merra(dat2, "w50", 1, gif = T, gif_nmax = 48)
}


#' Quick GIF plots
#'
#' @param x 
#' @param filename 
#' @param nmax 
#' @param fps 
#' @param gif.width 
#' @param gif.height 
#' @param verbose 
#' @param FUN 
#' @param dirname 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
gif_merra <- function(x, 
                      # name = "W10M",
                      FUN = "plot_merra",
                     #  name = "W50M", scale = 1, 
                     nmax = 24*12,
                     fps = 12,
                     gif.width = 576, gif.height = 360,
                     filename = NULL,
                     dirname = ".",
                     # limits = c(0, 35), 
                     # palette = "RdYlBu", direction = -1,
                     # legend.position = c(0.9, 0.05),
                     # legend.name = "m/s",
                     # datetime.position = c(145, 87),
                     # datetime.format = "%Y-%b-%d, %Hh %Z",
                     verbose = getOption("merra2.verbose"),
                     ...
                      ) {
  # browser()
  # dirname <- dirname(filename)
  args <- list(...)
  if (is.null(filename)) {
    name <- args$name
    filename <- paste0(
      "merra_", name, "_", 
      round(gif.width), "x", round(gif.height),
      "_", round(fps), "fps.gif")
  }
  # args$name <- name
  # for (a in names(args)) {
  #   query[a] <- args[a]
  # }
  if (!dir.exists(dirname)) dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
  homedir <- getwd()
  on.exit(setwd(homedir))
  setwd(dirname)
  filename <- basename(filename)
  frames <- unique(x$datetime) %>% sort()
  nframes <- min(length(frames), nmax)
  # browser()
  animation::saveGIF({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      ii <- x$datetime == frames[i]
      args$x <- x[ii,]
      a <- do.call(FUN, args)
      # a <- plot_merra(x[ii,], name = name, scale = scale,
      #                limits = limits, palette = palette, direction = direction,
      #                legend.name = legend.name,
      #                legend.position = legend.position,
      #                datetime.position = datetime.position,
      #                datetime.format = datetime.format)
      
      
      if (i == nframes) {
        if (verbose) cat(" -> creating GIF\n")
      } else {
        if (verbose) cat(rep("\b", nchar(nframes) + 1), sep = "")
      }
      if (!is.null(a)) print(a)
    }
    }, 
    interval = 1/fps, ani.width = gif.width, ani.height = gif.height, 
    movie.name = filename
  )
  return(file.path(dirname, filename))
}


ffmpeg_merra <- function(x, name = "W50M", scale = 1, 
                      nmax = 24*12,
                      fps = 8,
                      width = 576, height = 360,
                      filename = paste0("merra_", name, ".mp4"), 
                      limits = c(0, 35), 
                      palette = "RdYlBu", direction = -1,
                      legend.position = c(0.91, 0.05),
                      legend.name = "m/s",
                      datetime.position = c(149, 87),
                      datetime.format = "%Y-%b-%d, %Hh %Z",
                      verbose = getOption("merra2.verbose")) {
  # browser()
  dr <- dirname(filename)
  if (!dir.exists(dr)) dir.create(dr, recursive = TRUE)
  frames <- unique(x$datetime) %>% sort()
  nframes <- min(length(frames), nmax)
  # browser()
  animation::saveVideo({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      ii <- x$datetime == frames[i]
      a <- plot_merra(x[ii,], name = name, scale = scale,
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
  video.name = filename,
  interval = 1/fps,
  ani.width = width, ani.height = height, 
  
  )

}


if (F) {
  x <- add_lonlat(merra2_mar)
  # gif_merra(x, "W10M", 1, fps = 12, filename = "images/merra_wind_10m_12fps.gif")
  gif_merra(x, "W10M", 1, fps = 10, dirname = "gif")
  ffmpeg_merra(x, "W10M", 1, fps = 4, filename = "merra_ffmpeg.gif")
  ffmpeg_merra(x, "W10M", 1, fps = 12, filename = "merra_ffmpeg4.mp4")
  gif_merra(dat2, "w50", 1, fps = 24, filename = "merra_wind_50m_24fps.gif")
  gif_merra(dat2, "w50", 1, fps = 12, filename = "merra_wind_50m_12fps.gif")
  gif_merra(dat2, "w100", 1, fps = 12, filename = "merra_wind_100m_12fps.gif")
  gif_merra(dat2, "w200", 1, fps = 6, filename = "merra_wind_200m_6fps.gif")
  
}


plot_SWGND <- function(x, name = "SWGDN", scale = 1, 
                      limits = c(0, 1300), 
                      palette = "RdYlBu", direction = -1,
                      na.value = "grey50",
                      legend.position = c(0.91, 0.05),
                      legend.name = "m/s",
                      datetime.position = c(150, 87),
                      datetime.format = "%Y-%b-%d, %Hh %Z"
                      # gif = FALSE 
                      # gif.file = "merra_wind.gif",
                      # gif.nmax = 24, fps = 12,
                      # gif.width = 1920/3, gif.height = 1080/3
) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame with MERRA-2 subset")
  if (is.null(x[[name]])) stop("Column '", name, "' is not found")
  if (is.null(x[["lon"]]) | is.null(x[["lat"]])) {
    if (!is.null(x[["locid"]])) {
      warning("Adding 'lon' and  'lat' from 'locid'")
      lid <- select(merra2ools::locid, 1:3)
      x <- right_join(lid, x)
    } else {
      stop("Data doesn't have lat, lon or lod_id")
    }
  }
  gif = FALSE 
  # browser()
  x$mps <- x[[name]]*scale
  x$mps[x$mps < limits[1]] <- limits[1]
  x$mps[x$mps > limits[2]] <- limits[2]
  dat_time <- unique(x$datetime) %>% sort()
  if (length(dat_time) > 1) {
    if (!gif) {
      warning("Multiple datetime values found, using the earliest one: ", dat_time[1])
      ii <- x$datetime == dat_time[1]
    } else {
      ii <- x$datetime %in% dat_time[1:gif_nmax]
    }
    x <- x[ii,]
  }
  
  x <- select(x, lon, lat, datetime, mps)
  x$label <- format(x$datetime, datetime.format)
  
  pp <-
    ggplot(x, aes(lon, lat)) + 
    geom_tile(aes(fill = mps), alpha = 1, interpolate = F, 
                show.legend = !is.null(legend.position)) +
    scale_fill_distiller(palette = palette, direction = direction, 
                         limits = limits, name = legend.name, na.value = na.value) +
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
  #     # data = data.frame( x = 153, y = 87, label = x$label[1]), 
  #     aes(x = 153, y = 87, label = '{closest_state}'), label.size = 0, color = "black",
  #     fill = "white", alpha = 0.2)
  # pp <- animate(p, renderer = gifski_renderer(), fps = fps, 
  #               nframes = length(unique(x$datetime)), 
  #               width = 1024, height = 1024*1080/1920)
  
  # } else {
  # browser()
  if (!is.null(datetime.position) & all(!is.na(datetime.position))) {
    pp <- pp + 
      geom_label(
        data = data.frame(x = datetime.position[1], 
                          y = datetime.position[2],
                          label = x$label[1]),
        aes(x = x, y = y, label = label), label.size = 0, color = "black",
        fill = "white", alpha = 0.2)
  }
  return(pp)
  # }
}

if (F) {
  # plot_SWGND(x)
  plot_SWGND(x, scale = 1/1e3, limits = c(0, 1.2),
             palette = "Oranges", legend.name = "kW/m2")
  
  gif_merra(x, name = "SWGDN", scale = 1/1e3, limits = c(0, 1.2),
           filename = "merra_SWGDN.gif", #palette = "Oranges", 
           legend.name = "kW/m2", fps = 4)

  plot_SWGND(y, name = "zenith", scale = 1, limits = c(0, 90),
             direction = 1,
             # palette = "Oranges", 
             legend.name = "")

  plot_SWGND(y, name = "azimuth", scale = 1, limits = c(0, 360),
             direction = -1,
             # palette = "Oranges", 
             legend.name = "")
  
  y$azimuth[y$zenith > 90] <- 0
  plot_SWGND(y, name = "azimuth", scale = 1, limits = c(0, 360),
             direction = -1, palette = "Spectral",
             legend.name = "")

  gif_merra(y, name = "zenith", scale = 1, limits = c(0, 90),
           filename = "merra_zenith.gif", direction = 1, 
           legend.name = "", fps = 4)

  gif_merra(y, name = "azimuth", scale = 1, limits = c(0, 360),
           filename = "merra_azimuth_daytime.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)

  plot_SWGND(z, name = "POA.fx", scale = 1, limits = c(0, 2000),
             direction = -1, palette = "Spectral",
             legend.name = "")

  plot_SWGND(z, name = "POA.hz", scale = 1, limits = c(0, 2000),
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  # Albedo
  summary(y$ALBEDO)
  
  plot_SWGND(y, name = "ALBEDO", scale = 1, limits = c(0, 1),
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  gif_merra(y, name = "ALBEDO", scale = 1, limits = c(0, 1),
           filename = "merra_ALBEDO.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)
  
  # Irradiance
  # z <- solar_irradiance(y, ALL = T)
  z
  summary(z$SWGDN)
  summary(z$DNI)
  summary(z$DHI)
  summary(z$clearness_index)
  summary(z$diffuse_fraction)
  ii <- sample(1:nrow(z), 1e4)
  plot(z$clearness_index[ii], z$diffuse_fraction[ii])
  
  plot_SWGND(x, scale = 1/1e3, limits = c(0, 1.2),
             palette = "Oranges", legend.name = "kW/m2")
  
  z
  # Extraterrestrial irradiance
  summary(z$ext_irrad)
  summary(z$ext_irrad[z$datetime == z$datetime[1]])
  plot_SWGND(z, name = "ext_irrad", scale = 1, limits = ..,
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  summary(z$clearness_index[z$datetime == z$datetime[1]])
  plot_SWGND(z, name = "clearness_index", scale = 1, 
             limits = c(0, 1),
             # limits = range(z$clearness_index[z$datetime == z$datetime[1]])/20,
             direction = -1, palette = "Spectral",
             legend.name = "")
  gif_merra(z, name = "clearness_index", scale = 1, limits = c(0, 1),
           filename = "merra_clearness_index.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)
  
  summary(z$diffuse_fraction)
  plot_SWGND(z, name = "diffuse_fraction", scale = 1, limits = c(0, 1),
             direction = -1, palette = "Spectral",
             legend.name = "")
  gif_merra(z, name = "diffuse_fraction", scale = 1, limits = c(0, 1),
           filename = "merra_diffuse_fraction.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)
  
  summary(z$DNI)
  plot_SWGND(z, name = "DNI", scale = 1/1e3, limits = c(0, 1.1),
             direction = -1, palette = "Spectral",
             legend.name = "kW/m2")
  gif_merra(z, name = "DNI", scale = 1/1e3, limits = c(0, 1.1),
           filename = "merra_DNI.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "kW/m2", fps = 4)
  
  summary(z$DHI)
  plot_SWGND(z, name = "DHI", scale = 1/1e3, limits = c(0, 1.1),
             direction = -1, palette = "Spectral",
             legend.name = "kW/m2")
  
  gif_merra(z, name = "DHI", scale = 1/1e3, limits = c(0, 1.1),
           filename = "merra_DHI.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "kW/m2", fps = 4)
  

  # AOI
  # z$array.azimuth.fx <- 180 # Northern
  # 
  # z$AOI.fx <- angle_of_incidence(azimuth = z$azimuth, zenith = z$zenith,
  #                             array.tilt = z$lat, 
  #                             array.azimuth = z$array.azimuth.fx 
  #                             # AOI.max = pi/2, return_rad = TRUE
  #                             )
  range(z$AOI.fx, na.rm = T)
  summary(is.na(z$AOI.fx))
  summary(z$AOI.fx * 180 / pi)
  plot_SWGND(z, name = "AOI.fx", scale = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")

  plot_SWGND(z, name = "AOI.hz", scale = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.tl", scale = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.vt", scale = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.dl", scale = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  gif_merra(z, name = "AOI.fx", scale = 1, limits = c(0, pi/2),
           filename = "merra_angle_of_incidence_90.gif", 
           direction = 1, palette = "Spectral",
           legend.name = "rad", fps = 4)
  
  ii <- !is.na(z$AOI.dl)
  ii <- sample(which(ii), 1e3)
  head(ii)
  select(z[ii,], lon, lat, zenith, azimuth, starts_with("AOI"))
  select(z[ii,], lon, lat, zenith, azimuth, starts_with("array.tilt"))
  select(z[ii,], lon, lat, zenith, azimuth, starts_with("array.azimuth"))
  
  plot(z$AOI.dl[ii], z$AOI.fx[ii])
  
  # correction needed?
  # y$array.azimuth.fx[y$lat < 0] <- 0
  # plot_SWGND(y, name = "AOI", scale = 1, limits = c(0, pi),
  #            direction = 1, palette = "Spectral",
  #            legend.name = "")
  # gif_merra(y, name = "AOI", scale = 1, limits = c(0, 3),
  #          filename = "merra_angle_of_incidence_South0_North180.gif", 
  #          direction = 1, palette = "Spectral",
  #          legend.name = "0:pi", fps = 4)
  
  # POA.fx
  # y$diffuse_fraction <- diffuse_fraction(
  #   yday = y$yday, zenith = y$zenith, SWGDN = y$SWGDN)
  # summary(y$diffuse_fraction)
  # z$POA.fx <-
  #   (z$daytime) * (z$AOI.fx < pi / 2) * z$SWGDN * (1 - z$diffuse_fraction) /
  #   cospi(z$zenith / 180) * cos(z$AOI.fx) +
  #   z$SWGDN * z$diffuse_fraction * (1 + cospi(z$lat / 180)) / 2 +
  #   z$SWGDN * z$ALBEDO * (1 - cospi(z$lat / 180)) / 2
  
  summary(z$POA.fx)
  summary(z$POA.fx[z$zenith < 87])
  summary(z$POA.fx[z$AOI.fx < pi / 2 - .1])

  plot_SWGND(z, name = "POA.fx", scale = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2")

  plot_SWGND(z, name = "POA.hz", scale = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2")

  plot_SWGND(z, name = "POA.tl", scale = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  #+ coord_map("mollweide")
  
  plot_SWGND(z, name = "POA.vt", scale = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  
  plot_SWGND(z, name = "POA.dl", scale = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  
  gif_merra(z, name = "POA.fx", scale = 1, limits = c(0, 3000),
           filename = "POA.fx.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "W/m2", fps = 4)
  
    
}
