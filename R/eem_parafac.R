#' Read a PARAFAC model
#'
#' @param matfile Path of the mat file.
#' @param object Matlab object containing the model (ex. \emph{Test2}).
#' @param ncomp Numeric. The number of components to extract from \code{object}.
#'
#' @import tidyr
#' @import rmatio
#' @importFrom dplyr starts_with
#'
#' @return A data frame with loading.
#' @export
#'
#' @examples
#' f <- system.file("extdata/parafac_model.mat", package = "eemR")
#' m1 <- eem_read_parafac(f, object = "mymodel", 4)

eem_read_parafac <- function(matfile, object, ncomp) {

  stopifnot(
    file.exists(matfile),
    is.character(matfile),
    is.character(object),
    is.numeric(ncomp),
    ncomp > 1
    )

  # https://github.com/stewid/rmatio
  M <- rmatio::read.mat(matfile)

  model <- paste0("Model", ncomp)

  loading <- M[[object]][[model]]

  if (is.null(loading)) {
    stop("Can't find the requested model in the specified mat file.",
         call. = FALSE)
  }

  em_loading <- matrix(unlist(loading[[1]][2]), ncol = ncomp)
  ex_loading <- matrix(unlist(loading[[1]][3]), ncol = ncomp)

  ex <- unlist(M[[object]][["Ex"]])
  em <- unlist(M[[object]][["Em"]])

  em_loading <- data.frame(em_loading)
  names(em_loading) <- paste0("component", 1:ncol(em_loading))
  em_loading$wavelength <- em
  em_loading$type <- "emission"

  ex_loading <- data.frame(ex_loading)
  names(ex_loading) <- paste0("component", 1:ncol(ex_loading))
  ex_loading$wavelength <- ex
  ex_loading$type <- "excitation"

  loading <- dplyr::bind_rows(ex_loading, em_loading)
  loading <- tidyr::gather(loading,
                           "component",
                           "fluorescence",
                           starts_with("component"))

  class(loading) <- c("parafac_model", "data.frame")

  return(loading)

}

# plot <- function(x){
#   NextMethod()
# }

#' Plot a PARAFAC model
#'
#' @param x A data frame returned by \code{eem_read_parafac}.
#' @param nrow Number of rows in the plot grid.
#' @param ncol Number of columns in the plot grid.
#' @param ... Additional parameters (not used at the moment).
#'
#' @return A ggplot2 object
#' @import ggplot2
#' @export
#'
#' @details Use \code{vignette("parafac")} for more details.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' f <- system.file("extdata/parafac_model.mat", package = "eemR")
#' m1 <- eem_read_parafac(f, object = "mymodel", 4)
#'
#' p <- plot(m1, nrow = 2, ncol = 2)
#' p
plot.parafac_model <- function(x, nrow, ncol, ...) {

  stopifnot(
    "parafac_model" %in% class(x),
    is.numeric(nrow),
    is.numeric(ncol),
    length(nrow) == 1 & length(ncol) == 1
  )

  x <- dplyr::group_by(x, component)
  x <- tidyr::nest(x)

  eem_plots <- lapply(x$data, plot_component)

  p <- cowplot::plot_grid(plotlist = eem_plots, nrow = nrow, ncol = ncol)

  invisible(p)

}

to_mat <- function(df) {

  # df <- dplyr::filter(df, component == comp)

  x <- outer(df$fluorescence[df$type == "emission"],
             df$fluorescence[df$type == "excitation"]) %>%
    as.vector()

  ex <- unique(df$wavelength[df$type == "excitation"])
  em <- unique(df$wavelength[df$type == "emission"])

  exx <- rep(ex, each = length(em))
  emm <- rep(em, length(ex))

  x <- data.frame(ex = exx, em = emm, fluo = x)

}


plot_component <- function(df) {

  components <- to_mat(df)

  # Surface plot

  p3d <- components %>%
    ggplot(aes(x = ex, y = em, fill = fluo)) +
    geom_raster(interpolate = FALSE) +
    viridis::scale_fill_viridis(256) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw(base_size = 10) +
    theme(axis.line = element_line(colour = "black")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_blank()) +
    xlab("Excitation (nm)") +
    ylab("Emission (nm)") +
    theme(legend.position = "none")

  # Loading plot
  pl <- df %>%
    ggplot(aes(x = wavelength, y = fluorescence, color = type)) +
    geom_line() +
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    xlab("Wavelength (nm)") +
    ylab("Loading")

  p <- cowplot::plot_grid(p3d, pl, nrow = 1, align = "hv")

  invisible(p)

}

# Extract FMax
eem_parafac_fmax <- function() {



}
