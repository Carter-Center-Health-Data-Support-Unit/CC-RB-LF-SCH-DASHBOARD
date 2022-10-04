

#' Title
#'
#' @return
#' @export
#' @description extract monthly report data from onedrive. You need to set an environment variable : `CC_RB_LF_SCH_MONTHLY`. Recommend setting this via `usethis::edit_r_environ()`
#' @examples \donrun{
#' fetch_monthly_data()
#' }
fetch_monthly_data<- function(){
  monthly_folder_contents = Sys.getenv("CC_RB_LF_SCH_MONTHLY") |> dir()
  report_folders <- stringr::str_subset(monthly_folder_contents,pattern = "^20.+Reports$")
  report_folder_files <- report_folders |>
    purrr::map(~file.path(Sys.getenv("CC_RB_LF_SCH_MONTHLY") ,.x) |>
                 dir(recursive = T) ) |>
    rlang::set_names(report_folders)


  report_folder_files |>
    purrr::map2(.y= names(report_folder_files),
                ~ {if(.y=="2021 Reports"){
                  foi <- stringr::str_subset(.x,"RBLF- CDTIA.*")
                } else{
                  foi <- stringr::str_subset(.x,".*ETH.*\\.xlsx$")
                }
                  return(foi)
                  }
                )




}



report_folders |>
  purrr::map(~file.path(Sys.getenv("CC_RB_LF_SCH_MONTHLY") ,.x) |> dir() )
