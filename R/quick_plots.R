
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
                     nmax = 24*12,
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


plot_SWGND <- function(merra, name = "SWGDN", mult = 1, 
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
  # plot_SWGND(x)
  plot_SWGND(x, mult = 1/1e3, limits = c(0, 1.2),
             palette = "Oranges", legend.name = "kW/m2")
  
  wind_GIF(x, name = "SWGDN", mult = 1/1e3, limits = c(0, 1.2),
           filename = "merra_SWGDN.gif", #palette = "Oranges", 
           legend.name = "kW/m2", fps = 4)

  plot_SWGND(y, name = "zenith", mult = 1, limits = c(0, 90),
             direction = 1,
             # palette = "Oranges", 
             legend.name = "")

  plot_SWGND(y, name = "azimuth", mult = 1, limits = c(0, 360),
             direction = -1,
             # palette = "Oranges", 
             legend.name = "")
  
  y$azimuth[y$zenith > 90] <- 0
  plot_SWGND(y, name = "azimuth", mult = 1, limits = c(0, 360),
             direction = -1, palette = "Spectral",
             legend.name = "")

  wind_GIF(y, name = "zenith", mult = 1, limits = c(0, 90),
           filename = "merra_zenith.gif", direction = 1, 
           legend.name = "", fps = 4)

  wind_GIF(y, name = "azimuth", mult = 1, limits = c(0, 360),
           filename = "merra_azimuth_daytime.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)

  plot_SWGND(z, name = "POA.fx", mult = 1, limits = c(0, 2000),
             direction = -1, palette = "Spectral",
             legend.name = "")

  plot_SWGND(z, name = "POA.hz", mult = 1, limits = c(0, 2000),
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  # Albedo
  summary(y$ALBEDO)
  
  plot_SWGND(y, name = "ALBEDO", mult = 1, limits = c(0, 1),
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  wind_GIF(y, name = "ALBEDO", mult = 1, limits = c(0, 1),
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
  
  plot_SWGND(x, mult = 1/1e3, limits = c(0, 1.2),
             palette = "Oranges", legend.name = "kW/m2")
  
  z
  # Extraterrestrial irradiance
  summary(z$ext_irrad)
  summary(z$ext_irrad[z$datetime == z$datetime[1]])
  plot_SWGND(z, name = "ext_irrad", mult = 1, limits = ..,
             direction = -1, palette = "Spectral",
             legend.name = "")
  
  summary(z$clearness_index[z$datetime == z$datetime[1]])
  plot_SWGND(z, name = "clearness_index", mult = 1, 
             limits = c(0, 1),
             # limits = range(z$clearness_index[z$datetime == z$datetime[1]])/20,
             direction = -1, palette = "Spectral",
             legend.name = "")
  wind_GIF(z, name = "clearness_index", mult = 1, limits = c(0, 1),
           filename = "merra_clearness_index.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)
  
  summary(z$diffuse_fraction)
  plot_SWGND(z, name = "diffuse_fraction", mult = 1, limits = c(0, 1),
             direction = -1, palette = "Spectral",
             legend.name = "")
  wind_GIF(z, name = "diffuse_fraction", mult = 1, limits = c(0, 1),
           filename = "merra_diffuse_fraction.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "", fps = 4)
  
  summary(z$DNI)
  plot_SWGND(z, name = "DNI", mult = 1/1e3, limits = c(0, 1.1),
             direction = -1, palette = "Spectral",
             legend.name = "kW/m2")
  wind_GIF(z, name = "DNI", mult = 1/1e3, limits = c(0, 1.1),
           filename = "merra_DNI.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "kW/m2", fps = 4)
  
  summary(z$DHI)
  plot_SWGND(z, name = "DHI", mult = 1/1e3, limits = c(0, 1.1),
             direction = -1, palette = "Spectral",
             legend.name = "kW/m2")
  
  wind_GIF(z, name = "DHI", mult = 1/1e3, limits = c(0, 1.1),
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
  plot_SWGND(z, name = "AOI.fx", mult = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")

  plot_SWGND(z, name = "AOI.hz", mult = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.tl", mult = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.vt", mult = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  plot_SWGND(z, name = "AOI.dl", mult = 1, limits = c(0, pi/2),
             direction = 1, palette = "Spectral",
             legend.name = "rad")
  
  wind_GIF(z, name = "AOI.fx", mult = 1, limits = c(0, pi/2),
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
  # plot_SWGND(y, name = "AOI", mult = 1, limits = c(0, pi),
  #            direction = 1, palette = "Spectral",
  #            legend.name = "")
  # wind_GIF(y, name = "AOI", mult = 1, limits = c(0, 3),
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

  plot_SWGND(z, name = "POA.fx", mult = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2")

  plot_SWGND(z, name = "POA.hz", mult = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2")

  plot_SWGND(z, name = "POA.tl", mult = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  #+ coord_map("mollweide")
  
  plot_SWGND(z, name = "POA.vt", mult = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  
  plot_SWGND(z, name = "POA.dl", mult = 1/1e3, limits = c(0, 1),
             direction = -1, palette = "Spectral", 
             legend.position = c(0.9, 0.05),
             legend.name = "kW/m2") 
  
  wind_GIF(z, name = "POA.fx", mult = 1, limits = c(0, 3000),
           filename = "POA.fx.gif", 
           direction = -1, palette = "Spectral",
           legend.name = "W/m2", fps = 4)
  
    
}
