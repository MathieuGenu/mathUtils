#' Get and build result path.
#'
#' Executing this function in an open script in Rstudio will create a mirror repository
#'  in "res" folder to store results of the script.
#'
#' @return 2 actions are made
#'         \enumerate{
#'           \item gives the path name of the res folder mirror of a R script
#'           \item create the res path corresponding of the R script
#'         }
#' @examples
#'
#' @import stringr
#' @importFrom rstudioapi getSourceEditorContext
#' @export


get_build_res_path <- function() {

  # get script path
  script_path <- rstudioapi::getSourceEditorContext()$path

  # delete root and .R extension
  path_w_root <- stringr::str_extract(pattern = "/R/(.*)",string = script_path)
  path_w_root <- stringr::str_extract(pattern = "^.*(?=(.R))",string = path_w_root)
  path_w_root <- stringr::str_extract(pattern = "R/(.*)",string = path_w_root)


  # res path to create
  res_path <- gsub(pattern = "R/", replacement = "res/", path_w_root)

  # check if repository already exist
  if (!file.exists(res_path)) {
    dir.create(res_path, showWarnings = F, recursive = T)
  }

  # return
  return(res_path)

}



