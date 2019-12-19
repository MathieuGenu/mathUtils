#' @export

get_build_res_reper <- function(r_script_path) {

  # get script path
  script_path <- rstudioapi::getSourceEditorContext()$path

  # delete root and .R extension
  path_w_root <- str_extract(pattern = "R/(.*)",string = script_path)
  path_w_root <- str_extract(pattern = "^.*(?=(.R))",string = path_w_root)

  # res path to create
  res_path <- gsub(pattern = "R/", replacement = "res/", path_w_root)

  # check if repersitory already exist
  if (!file.exists(res_path)) {
    dir.create(res_path, showWarnings = F, recursive = T)
  }

  # return
  return(res_path)

}



