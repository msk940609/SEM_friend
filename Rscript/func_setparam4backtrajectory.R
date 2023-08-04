setTrajLims <- function(mydata, Args, projection, parameters, orientation) {
  
  ## xlim and ylim set by user
  if ("xlim" %in% names(Args) & !all(is.na(Args$xlim))) {
    x1 <- Args$xlim[1]
    x2 <- Args$xlim[2]
  } else {
    x1 <- min(mydata$lon, na.rm = TRUE)
    x2 <- max(mydata$lon, na.rm = TRUE)
  }
  
  if ("ylim" %in% names(Args) & !all(is.na(Args$ylim))) {
    y1 <- Args$ylim[1]
    y2 <- Args$ylim[2]
  } else {
    y1 <- min(mydata$lat, na.rm = TRUE)
    y2 <- max(mydata$lat, na.rm = TRUE)
  }
  
  n <- 40 ## number of points along each vertex
  
  X <- c(
    seq(x1, x1, length.out = n), seq(x1, x2, length.out = n),
    seq(x2, x2, length.out = n), seq(x2, x1, length.out = n)
  )
  
  Y <- c(
    seq(y1, y2, length.out = n), seq(y2, y2, length.out = n),
    seq(y2, y1, length.out = n), seq(y1, y1, length.out = n)
  )
  
  tmp <- mapproj::mapproject(
    x = X, y = Y, projection = projection,
    parameters = parameters, orientation = orientation
  )
  
  Args$xlim <- tmp$range[1:2]
  Args$ylim <- tmp$range[3:4]
  Args
}

listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}