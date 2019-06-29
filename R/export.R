#' Export the cache
#'
#' Saves the cache to a zipped file that can be imported
#' elsewhere. Exported cache includes both twitter data
#' and registered tokens. Import the cache on a different
#' computer with [import_cache()].
#'
#' @param path
#'
#' @export
#'
export_cache <- function(path) {
  cache_dir <- get_cache_dir()
  cache_files <- list.files(cache_dir, full.names = TRUE)
  zip(path, cache_files)
  message(glue("Cache exported to {path.expand(path)}."))
}

#' Import the cache
#'
#' Not yet implemented.
#'
#' Adds users and token from the zipped cache file to the
#' local cache.
#'
#' @param path Path to a zipped folder created by [export_cache()].
#' @param overwrite Logical indicating whether or not to overwrite
#'   user data when already present in the cache. When `TRUE`,
#'   data in the cache file will overwrite data in the local
#'   cache. Defaults to `FALSE`.
#'
#' @export
import_cache <- function(path, overwrite = FALSE) {
  .NotYetImplemented()
}
