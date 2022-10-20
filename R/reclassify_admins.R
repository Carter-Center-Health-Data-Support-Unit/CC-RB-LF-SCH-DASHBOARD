

# rename_admin_cols <- function()

#' sep_adm_2_3
#'
#' @param df data.frame - raw RBLFSCHIST data
#' @description fixes common issue in admin 2 & 3 from field reporting. Oftenwhen there is just one admin 3 in reported in an admin 3 they include it in admin 2 with the following syntax admin2 = admin2_name - admin3_name and then write 1 in admin 3 column. This data entry procedure should be improved in future data, but this functions resolves the issue in pre-existing data
#' @return
#' @export
#'
#' @examples \dontrun{
#'
#' # first step of cleaning admin names. Need to do this before sanitizomg
#' df |>
#'   sep_adm_2_3()
#' }
sep_adm_2_3 <-  function(df){
  df |>
    separate(col = adm2_name,into = c("adm2_rev","adm3_rev"),sep = "-") |>
    mutate(adm3_f= if_else(!is.na(adm3_rev),adm3_rev, adm3_name)) |>
    filter(!adm3_f %in% as.character(c(1:1000))) |>
    select(adm1_name, adm2_name = adm2_rev, adm3_name= adm3_f)
}




#' sanitize admins
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' # step 2 of cleaning admin names - perform once done with sep_2_3
#' df |>
#'   sep_admin_2_3() |>
#'   sanitize_admins()
#' }
sanitize_admins <-  function(df){
  df |>
    mutate(
      across(.cols = matches("adm\\d"),~clean_vec_names(.x))
    )
}


#' clean_adm1
#'
#' @param df data.frame containing renamed, sanitized admin names
#'
#' @return
#' @export
#' @description admin names are manually input and contain spelling differences. The admin names must be standardized. For standardization we are using OCHA admin list obtained from HDX. The case_when statement in the function reclassifies the column of interest (adm1). As new spelling differences/mistakes arise, we can easily add to the case_when statement.
#'
#' @examples \dontrun{
#'
#' # step 3/4 of cleaning
#' df |>
#'   sep_adm_1_2() |>
#'   sanitizea_admins()
#'   clean_adm1()
#' }

clean_adm1 <-  function(df){
  df |>
    mutate(
      adm1_name = case_when(
        adm1_name == "ben_gumuz"~"benishangul_gumz",
        adm1_name == "gambella"~"gambela" ,
        adm1_name == "snnpr"~"snnp", #snnp used to match HDX file
        adm1_name == "swe"~"south_west_ethiopia",
        TRUE~ adm1_name
      )
    )

}


