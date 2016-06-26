eem_interpolate_scattering <- function(eem) {

  stopifnot(.is_eemlist(eem) | .is_eem(eem))

  ## It is a list of eems, then call lapply
  if(.is_eemlist(eem)){

    res <- lapply(eem, eem_interpolate_scattering)

    class(res) <- class(eem)
    return(res)
  }

  #---------------------------------------------------------------------
  # Do the interpolation.
  #---------------------------------------------------------------------

  # http://stackoverflow.com/questions/31729855/using-interp2-in-matlab-with-nan-inputs

  x <- eem$x
  em <- eem$em
  ex <- eem$ex

  ex2 <- rep(ex, each = length(em))
  em2 <- rep(em, times = length(ex))

  # http://stackoverflow.com/questions/30113019/how-can-i-create-a-surface-plot-with-missing-values-in-r

  nas <- !is.na(x)
  x2 <- akima::interp(
    row(x)[nas],       #row index   - 'x' values
    col(x)[nas],       #col index   - 'y' values
    x[nas],
    yo = seq(1, length(ex), length.out = length(ex)),
    xo = seq(1, length(em), length.out = length(em))
  )

  ## Construct an eem object.
  res <- eem
  res$x <- x2$z


  attributes(res) <- attributes(eem)
  attr(res, "is_interpolated") <- TRUE

  class(res) <- class(eem)

  return(res)

}
