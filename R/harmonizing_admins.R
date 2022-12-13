



#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
load_sanitized_admins <- function(df){
  df |>
    parse_top_table() |>
    janitor::clean_names() |>
    mutate(
      adm1_name = clean_vec_names(region),
      adm2_name = clean_vec_names(zone),
      adm3_name = clean_vec_names(name_of_woredas)
    ) |>
    filter(!is.na(adm1_name)) |>
    select(matches("^adm_*"))

}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
load_admins_from_raw <- function(df){

  df |>
    parse_top_table() |>
    janitor::clean_names() |>
    mutate(
    adm1_name = region,
    adm2_name = zone,
    adm3_name = name_of_woredas
    ) |>
    filter(
      !is.na(adm1_name),
      !str_detect(adm1_name,"^[Zz]onal"),
      !str_detect(adm1_name,"^[Rr]egion")
    ) |>
    select(matches("^adm_*"))

}

#'
#'
#' #' Title
#' #'
#' #' @param vec
#' #'
#' #' @return
#' #' @description taken from `janitor::make_clean_names()`
#' #' @export
#' #'
#' #' @examples
#' clean_vec_names <-  function(vec){
#'   good_start <- stringr::str_replace(string = {{vec}},
#'                                      pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
#'                                      replacement = "\\1")
#'   cleaned_within <- stringr::str_replace(string = {{vec}},
#'                                          pattern = "[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]+",
#'                                          replacement = ".")
#'
#'   cased_names <- snakecase::to_snake_case(cleaned_within)
#'   return(cased_names)
#' }


#' clean_str_vec
#'
#' @param x vec
#'
#' @return  vector cleaned with `janitor::make_clean_names()` without unique criteria
#' @export
#'
#' @examples
clean_vec_names<- function(x){

  x_unique <- x[!is.na(x)] |>
    unique()

  x_unique_clean <- x_unique |>
    janitor::make_clean_names()

  x_lookup <- x_unique_clean |>
    rlang::set_names(x_unique)

  dplyr::recode(x,!!!x_lookup)


}


#' Title
#'
#' @param df
#' @param df_master
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' check_admin1(df = admins_c1,df_master = master_adm)
#' check_admin1(df = cleaned_adm1_1,df_master = master_adm)
#' }

check_admin1<- function(df, df_master){
  df_adm_san <-df |>
    sanitize_admins()
  df_master_adm_clean <-  df_master |>
    sanitize_admins()
  problem_freq <- df_adm_san |>
    filter(!adm1_name %in% df_master_adm_clean$adm1_en) |>
    count(adm1_name)
  if(nrow(problem_freq)==0){
    cat(crayon::green("All admin 1 in raw data are in master list"))
  }
  if(nrow(problem_freq)>0){
    cat(crayon::red("Below is a freq table of adm1_names used in raw data that are not in master\n"))
    problem_freq
  }

}





#' Title
#'
#' @param df
#' @param df_master
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' check_admin2(df = cleaned_adm1_a,df_master = master_adm)
#' check_admin2(df = cleaned_adm2_a,df_master = master_adm)
#' }
check_admin2<- function(df, df_master){
  df_adm_san <-df |>
    sanitize_admins() |>
    mutate(adm_1_2 = paste0(adm1_name,"_",adm2_name))
  df_master_adm_clean <-  df_master |>
    sanitize_admins() |>
    mutate(adm_1_2 = paste0(adm1_en,"_",adm2_en))
  problem_records <- df_adm_san |>
    filter(!adm_1_2 %in% df_master_adm_clean$adm_1_2)
  if("file_name" %in% colnames(df)){
    problem_freq <- problem_records |>
      count(file_name,adm_1_2)
  }else{
    problem_freq <- problem_records |>
      count(adm_1_2)
  }




  if(nrow(problem_freq)==0){
    cat(crayon::green("All admin 2 in raw data are in master list"))
  }
  if(nrow(problem_freq)>0){
    cat(crayon::red("Below is a freq table of adm1_name + adm2_name used in raw data that are not in master\n"))
    problem_freq
  }

}


check_admin3 <-  function(df,df_master){
  df_adm_san <-df |>
    sanitize_admins() |>
    mutate(
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}")
    )
  df_master_adm_clean <-  df_master |>
    sanitize_admins() |>
    mutate(
      adm_1_2_3=  glue::glue("{adm1_en}-{adm2_en}-{adm3_en}")
    )
  problem_freq <- df_adm_san |>
    filter(!adm_1_2_3 %in% df_master_adm_clean$adm_1_2_3) |>
    count(adm_1_2_3)

  num_issues <- nrow(problem_freq)
  if(num_issues==0){
    cat(crayon::green("All admin 1 in raw data are in master list"))
  }
  if(num_issues>0){
    cat(crayon::red("Below is a freq table of adm1_name + adm2_name + adm3_name used in raw data that are not in master\n"))
    problem_freq |>
      as_tibble() |>
      print(n=num_issues)
  }


}




