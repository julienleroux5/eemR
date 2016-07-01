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
                           "fluo",
                           starts_with("component"))

  return(loading)

}

#' Plot a PARAFAC model
#'
#' @param x A data frame returned by \code{eem_read_parafac}.
#' @param type Character. \emph{loading} for the loading plot or
#'   \emph{component} for the PARAFAC model.
#' @param ... Additional parameters (not used at the moment).
#'
#' @return A ggplot2 object
#' @export
#'
#' @details Use \code{vignette("parafac")} for more details.
#'
#' @examples
#' f <- system.file("extdata/parafac_model.mat", package = "eemR")
#'
#' m1 <- eem_read_parafac(f, object = "mymodel", 4)
#'
#' p <- eem_plot_parafac(m1, type = "loading")
#' p
#'
#' p <- eem_plot_parafac(m1, type = "component")
#' p

eem_plot_parafac <- function(x, type, ...) {

  stopifnot(type %in% c("loading", "component"))

  # Make labels prettier
  x$component <- gsub("(component)", "\\1 \\2", x$component)

  if (type == "loading") {

    .plot_loading(x)

    } else if (type == "component") {

    .plot_component(x)

  }

}

#' @import ggplot2
.plot_loading <- function(x) {

  p <- ggplot(x, aes_string(x = "wavelength",
                            y = "fluo")) +
    geom_line(aes_string(color = "type"), size = 1) +
    facet_wrap(~component) +
    theme_bw() +
    xlab("Wavelength (nm)") +
    ylab("Fluorescence") +
    theme(legend.justification = c(1, 1),  legend.position = c(1,1)) +
    labs(color = "Loadings") +
    theme(strip.text.x = element_text(size = 14, face = "bold"))

  invisible(p)

}

#' @importFrom dplyr group_by_ mutate_
#' @importFrom tidyr unnest_ nest_
.plot_component <- function(x) {

  ll <- group_by_(x, "component")
  ll <- nest_(ll, "data")
  ll <- mutate_(ll, res = ~purrr::map(data, mat2vec))
  ll <- unnest_(ll, "res")

  jet.colors <- colorRampPalette(c("#00007F",
                                   "blue",
                                   "#007FFF",
                                   "cyan",
                                   "#7FFF7F",
                                   "yellow",
                                   "#FF7F00",
                                   "red",
                                   "#7F0000"))

  p <-   ggplot(ll, aes_string(x = "ex", y = "em", fill = "fluo")) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(colours = jet.colors(255)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Excitation (nm)") +
    ylab("Emission (nm)") +
    facet_wrap(~component) +
    labs(fill = "Fluorescence") +
    theme(strip.text.x = element_text(size = 14, face = "bold"))

  invisible(p)

}

# *************************************************************************
# Function to tranform the loadings in a convenient format for ggplot2.
# *************************************************************************

mat2vec <- function(df) {

  ex_wl <- df$wavelength[df$type == "excitation"]
  em_wl <- df$wavelength[df$type == "emission"]
  ex_fluo <- df$fluo[df$type == "excitation"]
  em_fluo <- df$fluo[df$type == "emission"]

  m <- outer(ex_fluo, em_fluo)

  exx <- rep(ex_wl, time = length(em_wl))
  emm <- rep(em_wl, each = length(ex_wl))
  mm <- matrix(m, nrow = length(exx))

  df <- data.frame(ex = exx, em = emm, fluo = mm)

  return(df)

}

# Extract FMax
eem_parafac_fmax <- function() {



}
