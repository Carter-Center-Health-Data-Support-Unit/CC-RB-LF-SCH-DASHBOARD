

#' inventory_old_data_storage
#'
#' @return
#' @export
#' @description extract monthly report data from onedrive. You need to set an environment variable : `CC_RB_LF_SCH_MONTHLY`. Recommend setting this via `usethis::edit_r_environ()`
#' @examples \donrun{
#' inventory_old_data_storage() |>
#' mutate(
#'   correct_file=T,
#'   replace_file_name=NA
#'   ) |>
#'   write_csv("20221005_RBLFSCHI_monthly_dataset_inventory.csv",na = "")
#' }
#'

inventory_old_data_storage<- function(){
  month_labs <- lubridate::month(c(1:12),label = T) |> glue::glue_collapse(sep = "|")
  month_labs2022 <- lubridate::month(c(1:12),label = T) |>
    glue::glue_collapse(sep = "[ .]|")
  monthly_folder_contents = Sys.getenv("CC_RB_LF_SCH_MONTHLY") |> dir()
  report_folders <- stringr::str_subset(monthly_folder_contents,pattern = "^20.+Reports$")
  report_folder_files <- report_folders |>
    purrr::map(~file.path(Sys.getenv("CC_RB_LF_SCH_MONTHLY") ,.x) |>
                 dir(recursive = T) ) |>
    rlang::set_names(report_folders)

  report_folder_files |>
    purrr::map2_dfr(.y= names(report_folder_files),
                ~ {if(.y%in%c("2021 Reports","2022 Reports")){
                  foi <- stringr::str_subset(.x,"RBLF- CDTIA.*")
                } else{
                  foi <- stringr::str_subset(.x,".*ETH.*\\.xlsx$")
                }
                  foi |>
                    as_tibble() |>
                    mutate(
                      year=.y
                           )
                  }
                ) |>
    mutate(
      mo= case_when(
          year %in% c("2016 Reports",
                      "2017 Reports",
                      "2018 Reports",
                      "2019 Reports",
                      "2020 Reports")~ sprintf(parse_number(value),fmt="%02d")
        ),
      mo2=case_when(
        year %in% c("2021 Reports")~ str_extract(string = value,pattern = as.character(month_labs)),
        year %in% c("2022 Reports")~ replace_na(str_extract(string = value,pattern = as.character(month_labs2022)),"Jun")
      ),
      mo2 = sprintf(match(str_remove(mo2,"\\.") |> trimws(),month.abb),fmt="%02d"),
      mo3= case_when(
        !is.na(mo)~mo,
        TRUE ~ mo2
      ),
      new_file= glue::glue("{parse_number(year)}{mo3}_ETH_CCRBLFSCHI_Monthly.xlsx")
      ) |>
    arrange(parse_number(year),mo3) |>
    select(
      report_year=year,
      original_file= value,
      new_file
      )
}


#' Title
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' inventory <- load_data_inventory_sheet()
#'
#' }
#' TODO - make conditional on country code
load_data_inventory_sheet <-  function(){
  eth_monthly_dir <- glue::glue(Sys.getenv("CC_RB_LF_SCH_MONTHLY"),"/ETH")
  readxl::read_xlsx(file.path(eth_monthly_dir,"20221005_RBLFSCHI_monthly_dataset_inventory.xlsx"))
}



#' copy_raw_to_new_catalogue
#'
#' @return function copies data from nested folder structure to new directory with standardized names
#' @export
#'

copy_raw_to_new_catalogue <-  function(){
  inventory <- load_data_inventory_sheet()
  root_dir <- Sys.getenv("CC_RB_LF_SCH_MONTHLY")
  inventory_to_map <- inventory |>
    filter(correct_file) |>
    mutate(final_file_name= case_when(
      is.na(replace_file_name)~ new_file,
      TRUE~ replace_file_name
    ),
    old_file_name_full = glue::glue("{root_dir}/{report_year}/{original_file}"),
    catalogue_dir = glue::glue("{root_dir}/ETH/data_raw/{final_file_name}")
    ) |>
    select(old_file_name_full, catalogue_dir)

  purrr::map2(.x = inventory_to_map$old_file_name_full,
              .y=inventory_to_map$catalogue_dir,
              .f = ~file.copy(from =.x,to = .y,overwrite = F ))
}

