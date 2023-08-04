trajCluster_ms <- function(traj, method = "Euclid", n.cluster = 5,
                        type = "default",
                        cols = "Set1", split.after = FALSE, map.fill = TRUE,
                        map.cols = "grey40", map.alpha = 0.4,
                        projection = "lambert",
                        parameters = c(51, 51), orientation = c(90, 0, 0),
                        by.type = FALSE, origin = TRUE, plot = TRUE, ...) {
  
  # silence R check
  freq <- hour.inc <- default <- NULL
  
  if (tolower(method) == "euclid") {
    method <- "distEuclid"
  } else {
    method <- "distAngle"
  }
  
  # remove any missing lat/lon
  traj <- filter(traj, !is.na(lat), !is.na(lon))
  
  # check to see if all back trajectories are the same length
  traj <- group_by(traj, date) %>%
    mutate(traj_len = length(date))
  
  if (length(unique(traj$traj_len)) > 1) {
    warning("Trajectory lengths differ, using most common length.")
    ux <- unique(traj$traj_len)
    nmax <- ux[which.max(tabulate(match(traj$traj_len, ux)))]
    traj <- ungroup(traj) %>%
      filter(traj_len == nmax)
  }
  
  Args <- list(...)
  
  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(
    
    fontsize = current.font
  ))
  
  ## label controls
  Args$plot.type <- if ("plot.type" %in% names(Args)) {
    Args$plot.type
  } else {
    Args$plot.type <- "l"
  }
  Args$lwd <- if ("lwd" %in% names(Args)) {
    Args$lwd
  } else {
    Args$lwd <- 4
  }
  
  if ("fontsize" %in% names(Args)) {
    trellis.par.set(fontsize = list(text = Args$fontsize))
  }
  
  calcTraj <- function(traj) {
    
    ## make sure ordered correctly
    traj <- traj[order(traj$date, traj$hour.inc), ]
    
    ## length of back trajectories
    traj <- group_by(traj, date) %>%
      mutate(len = length(date))
    
    ## find length of back trajectories
    ## 96-hour back trajectories with origin: length should be 97
    n <- max(abs(traj$hour.inc)) + 1
    
    traj <- subset(traj, len == n)
    len <- nrow(traj) / n
    
    ## lat/lon input matrices
    x <- matrix(traj$lon, nrow = n)
    y <- matrix(traj$lat, nrow = n)
    
    z <- matrix(0, nrow = n, ncol = len)
    res <- matrix(0, nrow = len, ncol = len)
    
    if (method == "distEuclid") {
      res <- .Call("distEuclid", x, y, res)
    }
    
    if (method == "distAngle") {
      res <- .Call("distAngle", x, y, res)
    }
    
    res[is.na(res)] <- 0 ## possible for some to be NA if trajectory does not move between two hours?
    
    dist.res <- as.dist(res)
    clusters <- cluster::pam(dist.res, n.cluster)
    cluster <- rep(clusters$clustering, each = n)
    traj$cluster <- as.character(paste("C", cluster, sep = ""))
    traj
  }
  
  ## this bit decides whether to separately calculate trajectories for each level of type
  
  if (split.after) {
    traj <- group_by(traj, default) %>%
      do(calcTraj(.))
    traj <- cutData(traj, type)
  } else {
    traj <- cutData(traj, type)
    
    traj <- traj %>%
      group_by(across(type)) %>%
      do(calcTraj(.))
  }
  
  # trajectory origin
  origin_xy <- head(subset(traj, hour.inc == 0), 1) ## origin
  tmp <- mapproj::mapproject(
    x = origin_xy[["lon"]][1],
    y = origin_xy[["lat"]][1],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  receptor <- c(tmp$x, tmp$y)
  
  ## calculate the mean trajectories by cluster
  
  vars <- c("lat", "lon", "date", "cluster", "hour.inc", type)
  vars2 <- c("cluster", "hour.inc", type)

  agg <- dplyr::select(traj, vars) %>%
    group_by(across(vars2)) %>%
    summarise(across(everything(), mean))
  
  # the data frame we want to return before it is transformed
  resRtn <- agg
  
  ## proportion of total clusters
  
  vars <- c(type, "cluster")
  
  clusters <- traj %>%
    group_by(across(vars)) %>%
    tally() %>%
    #mutate(freq = round(100 * n / sum(n), 1))
    mutate(freq = NA)
  
  ## make each panel add up to 100
  if (by.type) {
    clusters <- clusters %>%
      group_by(across(type)) %>%
      #mutate(freq = 100 * freq / sum(freq))
      mutate(freq = NA)
    
    #clusters$freq <- round(clusters$freq, 1)
    clusters$freq <- NA
  }
  
  ## make sure date is in correct format
  class(agg$date) <- class(traj$date)
  attr(agg$date, "tzone") <- "GMT"
  
  ## xlim and ylim set by user
  if (!"xlim" %in% names(Args)) {
    Args$xlim <- range(agg$lon)
    #Args$xlim <- c(90,130)
  }
  
  if (!"ylim" %in% names(Args)) {
    Args$ylim <- range(agg$lat)
    #Args$ylim <- c(33,55)
  }
  
  ## extent of data (or limits set by user) in degrees
  trajLims <- c(Args$xlim, Args$ylim)
  
  ## need *outline* of boundary for map limits
  Args <- setTrajLims(traj, Args, projection, parameters, orientation)
  
  ## transform data for map projection
  tmp <- mapproj::mapproject(
    x = agg[["lon"]],
    y = agg[["lat"]],
    projection = projection,
    parameters = parameters,
    orientation = orientation
  )
  agg[["lon"]] <- tmp$x
  agg[["lat"]] <- tmp$y
  
  plot.args <- list(
    agg,
    x = "lon", y = "lat", group = "cluster",
    col = cols, type = type, map = TRUE, map.fill = map.fill,
    map.cols = map.cols, map.alpha = map.alpha,
    projection = projection, parameters = parameters,
    orientation = orientation, traj = TRUE, trajLims = trajLims,
    clusters = clusters, receptor = receptor,
    origin = origin
  )
  
  ## reset for Args
  plot.args <- listUpdate(plot.args, Args)
  
  plot.args <- listUpdate(plot.args, list(plot = plot))
  
  ## plot
  plt <- do.call(scatterPlot, plot.args)
  
  ## create output with plot
  output <-
    list(
      plot = plt,
      data = list(
        traj = traj,
        results = dplyr::left_join(resRtn, clusters, by = c("cluster", type)),
        subsets = c("traj", "results")
      ),
      call = match.call()
    )
  class(output) <- "openair"
  invisible(output)
}
