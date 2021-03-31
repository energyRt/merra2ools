#' Quick plots of wind speed and its dynamics
#'
#' @param x 
#' @param name 
#' @param scale 
#' @param limits 
#' @param legend.position 
#' @param timestamp.position 
#' @param timestamp.format 
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
#' NA
plot_merra <- function(x, 
                       name = "W50M", scale = 1, intercept = 0,
                      limits = c(0, 35), 
                      palette = "RdYlBu", direction = -1,
                      na.value = "#040434",
                      # na.value = "#070B34",
                      legend.position = c(0.91, 0.05),
                      legend.name = "",
                      timestamp.variable = "UTC",
                      timestamp.format = "%Y-%b-%d, %Hh %Z",
                      timestamp.position = c(150, 87),
                      expand.x = c(0., 0.),
                      expand.y = c(0., 0.),
                      map.border = "lightgrey",
                      map.border.size = .25,
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
      stop("Data doesn't have lat, lon or locid")
    }
  }
  gif = FALSE 
  x$tmp_var <- x[[name]] * scale + intercept
  x$tmp_var[x$tmp_var < limits[1]] <- limits[1]
  x$tmp_var[x$tmp_var > limits[2]] <- limits[2]
  if (!is.null(x[[timestamp.variable]])) {
    dat_time <- unique(x[[timestamp.variable]]) %>% sort()
  } else {
    warning("`timestamp.variable` is not found")
    dat_time <- NULL
    timestamp.position <- NULL
  }
  if (length(dat_time) > 1) {
    if (!gif) {
      warning("Multiple timestamp values found, using the earliest one: ", dat_time[1])
      ii <- x[[timestamp.variable]] == dat_time[1]
    } else if (!is.null(dat_time)) {
      ii <- x[[timestamp.variable]] %in% dat_time[1:gif_nmax]
    } else {
      ii <- rep(TRUE, nrow(x))
      x[[timestamp.variable]] <- NA
    }
    x <- x[ii,]
    dat_time <- dat_time[1]
  }

  if (!is.null(dat_time)) {
    x <- select(x, lon, lat, dplyr::all_of(timestamp.variable), tmp_var)
    timestamp.stamp <- format(dat_time, timestamp.format)
  } else {
    x <- select(x, lon, lat, tmp_var)
  }
  
  pp <-
    ggplot(x, aes(lon, lat)) + 
    geom_raster(aes(fill = tmp_var), alpha = 1, interpolate = F, 
                show.legend = !is.null(legend.position)) +
    scale_fill_distiller(palette = palette, direction = direction,
                         limits = limits, name = legend.name, 
                         na.value = na.value) +
    # scale_fill_viridis_c(direction = direction, option = "inferno",
                         # limits = limits, name = legend.name) +
    theme_void() +
    # theme(panel.grid = element_blank(), panel.border = element_blank()) +
    scale_x_continuous(expand = expand.x, limits = c(-180, 180)) +
    scale_y_continuous(expand = expand.y, limits = c(-90, 90)) +
    theme(legend.position = legend.position,
          legend.direction = "horizontal", legend.box = "horizontal",
          # plot.title = element_text(hjust = 0.5), 
          # plot.subtitle = element_text(hjust = 0.5),
          # legend.background = element_rect(fill = alpha('white', 0.5)),
          legend.box.background = element_rect(size = 0.0, 
                                               colour = NA, 
                                               fill = alpha('white', .25)),
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
    #               nframes = length(unique(x$timestamp)), 
    #               width = 1024, height = 1024*1080/1920)

  # } else {
    # browser()
  # browser()
  if (!(is.null(map.border) || is.na(map.border))) {
    world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
    pp <- pp + geom_sf(data = world, color = map.border, 
                       fill = NA, inherit.aes = F, size = map.border.size)
  }
  if (!(is.null(timestamp.position) || is.na(timestamp.position))) {
    # } else {
    pp <- pp + 
      geom_label(
        data = data.frame(x = timestamp.position[1], 
                          y = timestamp.position[2],
                          label = timestamp.stamp),
        aes(x = x, y = y, label = label), label.size = 0, color = "black",
        fill = "white", alpha = 0.7)
  }
  return(pp)
  # }
}

if (F) {
  plot_merra(dat2, "w50", 1)
  plot_merra(dat2, "w200", 1)
  plot_merra(dat2, "w50", 1, limits = c(0, 30))
  plot_merra(dat2, "w50", 1, limits = c(0, 50))
  plot_merra(dat2, "w50", 1, timestamp.position = NULL, legend.position = NULL)
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
#' NA
gif_merra <- function(x, 
                      FUN = "plot_merra",
                      timestamp.variable = "UTC",
                      nmax = 24*12,
                      fps = 12,
                      gif.width = 576, gif.height = 360,
                      filename = NULL,
                      filename.prefix = "merra_",
                      dirname = ".",
                      verbose = getOption("merra2.verbose"),
                      ...) {
  # dirname <- dirname(filename)
  # browser()
  x <- as.data.table(x)
  arg <- list(...)
  arg[["timestamp.variable"]] <- timestamp.variable
  if (is.null(filename)) {
    name <- arg$name
    filename <- paste0(
      filename.prefix, 
      round(gif.width), "x", round(gif.height),
      "_", round(fps), "fps.gif")
  }
  # arg$name <- name
  # for (a in names(arg)) {
  #   query[a] <- arg[a]
  # }
  if (!dir.exists(dirname)) dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
  homedir <- getwd()
  on.exit(setwd(homedir))
  setwd(dirname)
  filename <- basename(filename)
  frames <- unique(x[[timestamp.variable]]) %>% sort()
  nframes <- min(length(frames), nmax)
  # browser()
  animation::saveGIF({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      ii <- x[[timestamp.variable]] == frames[i]
      arg$x <- x[ii,]
      a <- do.call(FUN, arg, quote = FALSE)
      # a <- rlang::exec(.fn = FUN, !!!arg)
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
                      timestamp.position = c(149, 87),
                      timestamp.format = "%Y-%b-%d, %Hh %Z",
                      verbose = getOption("merra2.verbose")) {
  browser()
  x <- as.data.table(x)
  dr <- dirname(filename)
  if (!dir.exists(dr)) dir.create(dr, recursive = TRUE)
  frames <- unique(x$timestamp) %>% sort()
  nframes <- min(length(frames), nmax)
  # browser()
  animation::saveVideo({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      ii <- x$timestamp == frames[i]
      a <- plot_merra(x[ii,], name = name, scale = scale,
                      limits = limits, palette = palette, direction = direction,
                      legend.name = legend.name,
                      legend.position = legend.position,
                      timestamp.position = timestamp.position,
                      timestamp.format = timestamp.format)
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

