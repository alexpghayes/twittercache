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
#' @seealso import_cache
export_cache <- function(path) {
  cache_dir <- get_cache_dir()
  cache_files <- list.files(cache_dir, full.names = TRUE)
  zip(path, cache_files)
  message(glue("Cache exported to {path.expand(path)}."))
}

#' Import the cache
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

  cache_dir <- get_cache_dir()

  # TODO: is this a correct use of tempdir()?

  # unzip into a tmp folder
  unzip(path, exdir = tempdir())

  # TODO

  # for each token, try to register it

  # for each files, try to copy if it doesn't already exist

  # tell user:
  #   - how many new users were added to the cache
  #   - how many new tokens were added to the cache
}
