#' Create raster layers from tibbles
#'
#' Analysis results loaded in tibble format can be overlayed with the project's
#' raster mask layer with this function
#'
#' @param tbl Result tibble
#' @param ref_raster The reference mask raster layer (Stored as 'mask_ugke.tif'
#'   on the hard drive). \code{ref_raster} has to be the loaded raster layer or
#'   the full path to the '.tif' on the hard drive.
#' @param ref_idx Logical vector that defines the raster cells of the raster
#'   layer to which result values should be assigned (Stored as
#'   'has_value_mask.rds' on the hard drive). \code{ref_idx} has to be the
#'   loaded index vector or the full path to the '.rds' on the hard drive.
#' @param variable (Optional) Character vector to define the variables in the
#'   tibble that should be converted to a raster layer. If \code{variable} is
#'   not defined, all columns are converted
#' @param write_path (Optional) Character string to defie the path where the
#'   raster layers should be written as GeoTiffs. If \code{write_path} is not
#'   defined, the raster layers are returned to R by default.
#' @param and_return (Optional) Logical. If \code{and_return = TRUE}, the raster
#'   layers are additionally returned in R, when they are written to the hard
#'   drive (write_path is set). drive
#' @importFrom dplyr select
#' @importFrom purrr map set_names walk2
#' @importFrom raster raster stack values writeRaster
#'
#' @export
#'
make_raster_from_tibble <- function(tbl, ref_raster, ref_idx, variable = NULL,
                                    write_path = NULL, and_return = FALSE) {
  if(is.character(ref_idx)) {
    ref_idx <- readRDS(ref_idx)
  }


  if(is.null(variable)) {
    variable <- names(tbl)
  }

  if(any(!(variable %in% names(tbl)))) {
    stop("One of the 'variable' names is not available in 'tbl'.")
  }

  rst_out <- tbl %>%
    select(., !!variable) %>%
    map(., ~ pass_raster_value(ref_raster, .x, ref_idx)) %>%
    set_names(., variable)

  if(is.null(write_path)) {
    rst_out <- format_raster(rst_out)
    return(rst_out)
  } else {
    if(!dir.exists(write_path)) {
      dir.create(path = write_path, recursive = TRUE)
    }
    walk2(rst_out, variable, ~writeRaster(.x, write_path%//%.y%.%"tif"))

    if(and_return) {
      rst_out <- format_raster(rst_out)
      return(rst_out)
    }
  }
}

#' Pass values to raster and assign at the right positions
#'
#' @param rst Raster layer
#' @param val Values vector
#' @param pos Position vector
#' @importFrom raster values
#' @keywords internal
#'
pass_raster_value <- function(rst, val, pos) {
  values(rst)[pos] <- val
  return(rst)
}


#' Format raster before returning
#'
#' @param rst Raster layer
#' @importFrom raster stack
#' @keywords internal
#'
format_raster <- function(rst) {
  if(length(rst) > 1) {
    rst <- stack(rst)
  } else {
    rst <- rst[[1]]
  }
  return(rst)
}
