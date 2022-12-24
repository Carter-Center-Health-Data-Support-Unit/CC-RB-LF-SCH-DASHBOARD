

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
    mutate(adm3_f= if_else(!is.na(adm3_rev),adm3_rev, adm3_name),.after="adm2_rev") |>
    filter(!adm3_f %in% as.character(c(0:1000))) |>
    select(-adm3_name,-adm3_rev) |>
    dplyr::rename(
      adm2_name="adm2_rev",
      adm3_name="adm3_f"
    )
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
    dplyr::mutate(
      adm1_name = case_when(
        adm1_name == "ben_gumuz"~"benishangul_gumz",
        adm1_name == "gambella"~"gambela" ,
        adm1_name == "snnpr"~"snnp", #snnp used to match HDX file
        adm1_name %in% c("s_we","swe")~"south_west_ethiopia",
        TRUE~ adm1_name
      )
    )

}

clean_metekel_dam_worker <-  function(df){
  df |>
    dplyr::mutate(
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}"),
      adm1_name = dplyr::case_when(
        stringr::str_detect(adm_1_2_3, "dam-work|dam_work")~"benishangul_gumz",
        TRUE~adm1_name
      ),
      adm2_name = dplyr::case_when(
        stringr::str_detect(adm_1_2_3,"dam-work|dam_work")~"metekel",
        TRUE~adm2_name
      ),
      adm3_name = dplyr::case_when(
        stringr::str_detect(adm_1_2_3, "dam-work|dam_work")~"dam_workers",
        TRUE~adm3_name
      ),
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}")
    )
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

clean_admins_lfrx_patch <- function(df){
  df |>
    clean_metekel_dam_worker() |>
    dplyr::mutate(
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}"),
      adm1_name =
        dplyr::case_when(
          adm1_name == "south_west_ethiopia" & adm2_name== "south_omo"~"snnp",
          adm1_name %in% c("gambela","amahara","amhara") & adm2_name %in% c("metekel","dam_workers_metekel")~"benishangul_gumz",
          TRUE ~ adm1_name
        ),
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}"),

      adm3_name =
        dplyr::case_when(
          # snnp-gofa-mekeloza  ~ ?
          # ~snnp-south_omo-selamego
          # "snnp-south_omo-bena_tse_june"
          adm_1_2_3=="snnp-gofa-melogada"~ "melo_gada",
          # "snnp-south_omo-baka_dawula_ari" "boko_dawula"
          adm_1_2_3=="snnp-south_omo-baka_dawula_ari"~"boko_dawula",
          adm_1_2_3=="snnp-south_omo-bena_tse_june" ~"bena_tsemay",
          adm_1_2_3=="oromia-ilu_aba_bora-alge_sache"~"alge_sachi",
          adm_1_2_3=="oromia-ilu_aba_bora-boilo_nopa"~ "bilo_nopha",
          adm_1_2_3=="amhara-south_gondar-muja" ~ "sede_muja",
          adm_1_2_3 ==  "snnp-gofa-mekeloza"  ~ "melekoza",
          adm_1_2_3=="snnp-south_omo-selamego"~"salamago",
          TRUE~ adm3_name
        ),
      adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}")
    ) #|>
    # select(-adm_1_2_3)
}

apply_name_clean_patch <-  function(df){
  clean_admins_lfrx_patch(df)
}
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
clean_adm2 <-  function(df,data_format="current"){
  # df <- df |>
  #   dplyr::mutate(
  #     # adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}") # remove for now for the sake of the old format data which does not ahve adm1 , or 3 at this point
  #     )

  if(data_format=="current"){
    res <- df |>
      dplyr::mutate(
        # TYPE 2: INCORRECT ADMIN 1 - FIX ADMIN 1
        adm1_name= case_when(
          adm1_name == "snnp" & adm2_name %in%c("kaffa","kefa")~"south_west_ethiopia",
          adm1_name == "snnp" & adm2_name %in%c("dawuro")~"south_west_ethiopia",
          adm1_name == "snnp" & adm2_name %in%c("bench_sheko","bench_maji", "sheka")~"south_west_ethiopia",
          adm1_name == "snnp" & adm2_name %in%c("west_omo")~"south_west_ethiopia",
          TRUE~adm1_name
        ),
        # TYPE 1:  INCORRECT ADMIN 2 - FIX ADMIN 2
        adm2_name = case_when(
          adm2_name == "illubabor"~"ilu_aba_bora" ,
          adm2_name == "agnuwa"~"agnewak" ,
          adm2_name == "west_gojjam"~"west_gojam" ,
          adm1_name == "south_west_ethiopia" & adm2_name == "kaffa"~"kefa" ,
          adm1_name == "oromia" & adm2_name == "north_shoa"~"north_shewa_or" ,
          adm1_name == "amhara" & adm2_name == "north_shoa"~"north_shewa_am" ,
          adm1_name == "oromia" & adm2_name == "sw_shoa"~"south_west_shewa" ,
          adm1_name == "gambela" & adm2_name == "mezheng"~"majang" ,
          adm1_name == "gambela" & adm2_name == "agnua"~"agnewak" ,
          adm1_name == "amhara" & adm2_name == "south_wollo"~"south_wello" ,
          adm1_name == "gambela" & adm2_name == "itang_sp_w"~"itang_special_woreda" ,
          adm1_name == "oromia" & adm2_name == "e_arsi"~"arsi" ,
          adm1_name %in%c("snnp", "snnpr") & adm2_name =="kaffa"~"kefa",
          adm1_name == "benishangul_gumz" & adm2_name == "dam_workers_metekel"~"metekel" ,
          adm1_name == "gambela" & adm2_name == "refugees_7_camps"~"refugees" ,
          adm1_name == "gambela" & adm2_name == "refugees_7_vamps"~"refugees" ,
          adm1_name == "gambela" & adm2_name == "refugees_gambella"~"refugees" ,
          adm2_name == "gamogofa" ~ "gofa",
          adm2_name == "west_omo" ~ "mirab_omo",
          adm2_name == "west_om_b_149_o" ~ "mirab_omo",
          adm2_name == "bench_maji" ~ "bench_sheko",

          TRUE ~ adm2_name
        ),
        adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}")
      )


  }
  if(data_format=="old"){
    res <- df |>
      mutate(
        # TYPE 1:  INCORRECT ADMIN 2 - FIX ADMIN 2
        adm2_name = case_when(
          adm2_name == "illubabor"~"ilu_aba_bora" ,
          adm2_name == "agnuwa"~"agnewak" ,
          adm2_name == "west_gojjam"~"west_gojam" ,
          adm2_name == "kaffa"~"kefa" ,
          adm2_name == "north_shoa"~"north_shewa_or" ,
          adm2_name == "north_shoa"~"north_shewa_am" ,
          adm2_name == "sw_shoa"~"south_west_shewa" ,
          adm2_name == "mezheng"~"majang" ,
          adm2_name == "south_wollo"~"south_wello" ,
          adm2_name == "itang_sp_w"~"itang_special_woreda" ,
          adm2_name == "e_arsi"~"arsi" ,
          adm2_name =="kaffa"~"kefa",
          adm2_name == "dam_workers_metekel"~"metekel" ,
          adm2_name == "refugees_7_camps"~"refugees" ,
          adm2_name == "refugees_7_vamps"~"refugees" ,
          adm2_name == "refugees_gambella"~"refugees" ,
          adm2_name == "gamogofa" ~ "gofa",
          adm2_name == "west_omo" ~ "mirab_omo",
          adm2_name == "west_om_b_149_o" ~ "mirab_omo",
          adm2_name == "bench_maji" ~ "bench_sheko",
          adm2_name == "konta_spe_district"~ "konta_special",
          adm2_name == "yem_spe_district"~ "yem_special",
          adm2_name == "e_gojjam"~"east_gojam",
          adm2_name == "w_gojjam"~"west_gojam",
          adm2_name == "agnua"~"agnewak",
          adm2_name == "itang_spe_woreda"  ~  "itang_special_woreda",
          adm2_name == "konta_spe_woreda"~ "konta_special",
          adm2_name ==  "yem_spe_woreda" ~ "yem_special",
          adm2_name == "basketo_sp_district"~ "basketo",
          adm2_name == "east_harerge"~"east_hararge",
          adm2_name == "west_harerge"~"west_hararge",
          adm2_name == "west_harerge"~"west_hararge",
          adm2_name =="refugges_gambella"~"refugees" ,# this is how it's reclassified in "new", but not sure if should add gambella tag here since we don't have adm1...
          TRUE ~ adm2_name
        )#,
        # adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}") # no adm1 or 3 in old format data yet
      )
  }
  return(res)

}




#' clean_adm3
#'
#' @param df
#'
#' @return data.frame with admin 3 names reclassified
#' @export
#'
#' @examples
clean_adm3 <- function(df){
  df |>
  mutate(
    # lets separate admins with "-" to avoid confusion
    adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}"),

    # SCENARIO 1: adm1 mixed up - uncomment and fill if necessary
    # adm1_name= case_when()

    # SCENARIO 2: adm2 mixed upp
    adm2_name = case_when(
      adm_1_2_3=="amhara-central_gondar-quara"~ "west_gondar",
      adm_1_2_3 == "snnp-gamo-gezie_gofa" ~ "gofa",
      adm_1_2_3 == "snnp-gamo-melokoza" ~ "gofa",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-meneat_shasha" ~ "mirab_omo",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-meneat_goldia" ~ "mirab_omo",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-maji" ~ "mirab_omo",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-bero" ~ "mirab_omo",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-surma" ~ "mirab_omo",
      TRUE~ adm2_name
    ),

    adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}"),

    # SCENARIO 3 adm3 wrong
    adm3_name = case_when(
      adm_1_2_3=="amhara-south_gondar-e_estie"~ "east_esite",
      adm_1_2_3 %in% c("amhara-south_gondar-w_estie_andabet","amhara-south_gondar-w_estie")~ "andabet_west_esite",
      adm_1_2_3=="amhara-west_gojam-s_achefer"~ "debub_achefer",
      adm_1_2_3=="amhara-west_gojam-n_achefer"~ "semen_achefer",
      adm_1_2_3=="gambela-agnewak-dimma"~ "dima_gm",
      adm_1_2_3=="gambela-majang-goderie"~ "godere",
      adm_1_2_3=="oromia-buno_bedele-bedele"~ "bedele_zuria",
      adm_1_2_3=="oromia-buno_bedele-bedele_t"~ "bedele_town",
      adm_1_2_3=="gambela-agnewak-gambella_town"~ "gambela_town",
      adm_1_2_3=="gambela-agnewak-gambella_zuria"~ "gambela_zuria",
      adm_1_2_3 == "amhara-south_gondar-simada" ~ "semada",
      adm_1_2_3 == "benishangul_gumz-metekel-dibatie" ~ "dibate",
      adm_1_2_3 == "benishangul_gumz-metekel-NA" ~ "dam_workers",
      adm_1_2_3 == "benishangul_gumz-metekel-0" ~ "dam_workers",
      adm_1_2_3 == "benishangul_gumz-metekel-wombera" ~ "wembera",
      adm_1_2_3 == "gambela-agnewak-gambella" ~ "gambela_zuria",
      adm_1_2_3 == "gambela-itang_special_woreda-itang_sp_w" ~"itang",
      adm_1_2_3 == "gambela-majang-mengeshi" ~ "mengesh",
      adm_1_2_3 == "gambela-refugees-0" ~ "refugees",
      adm_1_2_3 == "gambela-refugees-NA" ~ "refugees",
      adm_1_2_3 == "oromia-arsi-digaluna_tijo" ~ "degeluna_tijo",
      adm_1_2_3=="south_west_ethiopia-dawuro-terch_town"~  "tercha_town",
      adm_1_2_3=="south_west_ethiopia-dawuro-tarcha_zuria"~  "tercha_zuriya",
      adm_1_2_3=="south_west_ethiopia-kefa-goba"~  "goba_sp",
      adm_1_2_3== "oromia-buno_bedele-chewaka" ~ "chwaka",
      adm_1_2_3== "oromia-buno_bedele-chora" ~ "chora_(buno_bedele)",
      adm_1_2_3 == "oromia-buno_bedele-didesa" ~ "dedesa",
      adm_1_2_3 == "oromia-east_hararge-deder_rural" ~ "deder",
      adm_1_2_3 == "oromia-east_hararge-gursum" ~ "gursum_(or)",
      adm_1_2_3 == "oromia-east_hararge-jarso" ~ "jarso_(east_hararghe)",
      adm_1_2_3 == "oromia-east_hararge-kersa" ~ "kersa_(east_hararge)",
      adm_1_2_3 == "oromia-east_hararge-melka_belo" ~ "melka_balo",
      adm_1_2_3 == "oromia-east_hararge-meyumulke" ~ "meyu_muleke",
      adm_1_2_3 == "oromia-ilu_aba_bora-algie_sachi" ~ "alge_sachi",
      adm_1_2_3 == "oromia-ilu_aba_bora-becho" ~ "becho_(ilu_aba_bora)",
      adm_1_2_3 == "oromia-ilu_aba_bora-bure" ~ "bure_(or)",
      adm_1_2_3 == "oromia-ilu_aba_bora-halu" ~ "halu /huka",
      adm_1_2_3 == "oromia-ilu_aba_bora-hurrumu" ~ "hurumu",
      adm_1_2_3 == "oromia-ilu_aba_bora-mettu" ~ "metu_zuria",
      adm_1_2_3 == "oromia-ilu_aba_bora-mettu_town" ~ "metu_town",
      adm_1_2_3 == "oromia-ilu_aba_bora-yayo" ~ "yayu",
      adm_1_2_3 == "oromia-jimma-agaro" ~ "agaro_town",
      adm_1_2_3 == "oromia-jimma-chora_botor" ~ "chora_(jimma)",
      adm_1_2_3 == "oromia-jimma-gomma" ~ "goma",
      adm_1_2_3 == "oromia-jimma-gumey" ~ "gumay",
      adm_1_2_3 == "oromia-jimma-kersa" ~ "kersa_(jimma)",
      adm_1_2_3 == "oromia-jimma-limu_saka" ~ "limu_seka",
      adm_1_2_3 == "oromia-jimma-mana" ~ "mena_(jimma)",
      adm_1_2_3 == "oromia-jimma-mencho" ~ "mancho",
      adm_1_2_3 == "oromia-jimma-shebe_sembo" ~ "shebe_sambo",
      adm_1_2_3 == "oromia-jimma-sokoru" ~ "sekoru",
      adm_1_2_3 == "oromia-north_shewa_or-girar_jarso" ~ "gerar_jarso",
      adm_1_2_3 == "oromia-north_shewa_or-hidhabu_abote" ~ "hidabu_abote",
      adm_1_2_3 == "oromia-west_hararge-chiro_rural" ~ "chiro_zuria",
      adm_1_2_3 == "oromia-west_hararge-guba_qoricha" ~ "goba_koricha",
      adm_1_2_3 == "oromia-west_hararge-oda_bultum" ~ "kuni_/oda_bultum",
      adm_1_2_3 == "oromia-west_hararge-tulo" ~ "tulo_(or)",
      adm_1_2_3 == "snnp-gofa-gezie_gofa" ~ "gezei_gofa",
      adm_1_2_3 == "snnp-gofa-melokoza" ~ "melekoza",
      adm_1_2_3 == "oromia-west_hararge-burka_dimtu" ~ "burqua_dhintu",
      adm_1_2_3 == "snnp-south_omo-selamago" ~ "salamago",
      adm_1_2_3 == "snnp-south_omo-selamego" ~ "salamago",
      adm_1_2_3 == "snnp-south_omo-semen_ari" ~ "north_ari",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-gide_bench" ~ "gidi_bench",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-guraferda" ~ "gurafereda",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-mizan_aman" ~ "mizan_aman_town",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-shie_bench" ~ "shay_bench",
      adm_1_2_3 == "south_west_ethiopia-bench_sheko-siz_town" ~ "size_town",
      adm_1_2_3 == "south_west_ethiopia-dawuro-esera" ~ "isara",
      adm_1_2_3 == "south_west_ethiopia-dawuro-essera" ~ "isara",
      adm_1_2_3 == "south_west_ethiopia-dawuro-gena_bossa" ~ "gena",
      adm_1_2_3 == "south_west_ethiopia-dawuro-kechi" ~ "kachi",
      adm_1_2_3 == "south_west_ethiopia-dawuro-mermanisa" ~ "mari_mansa",
      adm_1_2_3 == "south_west_ethiopia-dawuro-tercha_city" ~ "tercha_town",
      adm_1_2_3 == "south_west_ethiopia-dawuro-zaba_gazo" ~ "zabagazo",
      adm_1_2_3 == "south_west_ethiopia-kefa-adiyo" ~ "adiyio",
      adm_1_2_3 == "south_west_ethiopia-kefa-bitta" ~ "bita",
      adm_1_2_3 == "south_west_ethiopia-kefa-bonga" ~ "bonga_town",
      adm_1_2_3 == "south_west_ethiopia-kefa-shishoende" ~ "shisho_ande",
      adm_1_2_3 == "south_west_ethiopia-kefa-tello" ~ "tullo",
      adm_1_2_3 == "south_west_ethiopia-mirab_omo-gorigesha" ~ "gori_gesha",
      adm_1_2_3 == "south_west_ethiopia-mirab_omo-meneat_goldia" ~ "menit_goldiye",
      adm_1_2_3 == "south_west_ethiopia-mirab_omo-meneat_shasha" ~ "menit_shasha",
      adm_1_2_3 == "south_west_ethiopia-sheka-andracha" ~ "anderacha",
      adm_1_2_3 == "south_west_ethiopia-sheka-masha_woreda" ~ "masha",
      adm_1_2_3 == "south_west_ethiopia-sheka-teppi_town" ~ "tepi",
      adm_1_2_3 == "south_west_ethiopia-sheka-yeki_woreda" ~ "yeki",

      TRUE~ adm3_name
    ),
    adm_1_2_3=  glue::glue("{adm1_name}-{adm2_name}-{adm3_name}")

  )
}




#' standardize admin names
#'
#' @param df
#' @description checks data.frame for required raw admin name columns (region, zone, name_of_woredas) then renames them adm1_name, adm2_name, adm3_name
#' @return data.frame with admin names standardized (i.e adm1_name, adm2_name,adm3_name)
#' @export
#'
#' @examples
standardize_admin_names <-  function(df,data_format="current"){

  if(data_format=="current"){

    missing_adm_col <- setdiff(c("region","zone","name_of_woredas"),
                               colnames(df))

    assertthat::assert_that(length(missing_adm_col)==0,
                            msg = glue::glue("{crayon::red(missing_adm_col)} is missing from raw data"))
    res <- df |>
      dplyr::rename(
        adm1_name = "region",
        adm2_name = "zone",
        adm3_name = "name_of_woredas"
      )

  }
  # so no admin 1 in old format data....need to join later by adm2
  if(data_format=="old"){
    res <- df|>
      dplyr::rename_with(.cols = dplyr::any_of(c("name_of_zone" )),.fn = ~"zone") |>
      dplyr::rename(
        # adm1_name="region",
        adm2_name = "zone"
      )
  }
  return(res)


}


#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
drop_summary_rows <-  function(df,data_format="current"){
  if(data_format=="current"){
   res <-  df |>
      filter(
        !is.na(adm1_name),
        !str_detect(adm1_name,"^[Zz]onal"),
        !str_detect(adm1_name,"^[Rr]egion")
      )
  }
  if(data_format=="old"){
    res <- df |>
      sanitize_admins() |>
      dplyr::filter(
        !is.na(adm2_name),
        !str_detect(adm2_name,"^[Tt]otal"),
        !str_detect(adm2_name,"^note"),
        !str_detect(adm2_name,"^treatment"),
        !str_detect(adm2_name,"7_kebeles_in_surma_district_of_bench_maji_zone*"),
        !str_detect(adm2_name,"x7kebeles_in_surma_district_of_bench_maji_zone_were_not_inclu")
        )

  }
  return(res)

}



clean_names_and_admins <-  function(df,data_format="current"){
  df |>
    janitor::clean_names() |>
    standardize_admin_names(data_format = data_format)|>
    drop_summary_rows() |>
    sep_adm_2_3() |>
    sanitize_admins() |>
    clean_adm1() |>
    clean_adm2() |>
    clean_adm3() |>
    clean_admins_lfrx_patch() |>
    remove_empty_artefact_cols()
}


remove_empty_artefact_cols <-  function(df){
  df_p_artefacts <-  df |>
    parse_top_table() |>
    select(matches("^x\\d*"))

  df_x_cols_rm <- df |>
    select(-matches("^x\\d*"))

  mask_keep <- colSums(!is.na(df_p_artefacts)) > 0
  df_p_artefacts_m <- df_p_artefacts[, mask_keep, drop = FALSE]
  if(nrow(df_p_artefacts_m)>0){
    cbind(df_x_cols_rm,df_p_artefacts_m)
  }

}



#' extract_pre_clean_names_adms_batch
#' @description take list of data.frames/ iterative mutate on file name col, parse extract and parse top table, rename admin cols, sanitize admin cols, rename admin cols according to evolving lookup case_when
#' @param df_list
#' @param data_format
#'
#' @return
#' @export
#'
#' @examples
extract_pre_clean_names_adms_batch <- function(df_list,data_format= "current"){
  if(data_format=="current"){
    res <- df_list |>
      purrr::map2(.y = names(df_list),
                  ~ {
                    .x |>
                      dplyr::mutate(
                        file_name = .y
                      ) |>
                      parse_top_table() |>
                      clean_names_and_admins(data_format = data_format)

                  }
      )



  }

  if(data_format=="old"){
    res <-  df_list |>
      purrr::map( ~ {
        .x |>
          parse_top_table() |>
          janitor::clean_names() |>
          # clean_names_and_admins(data_format = data_format)
          standardize_admin_names(data_format = data_format) |>
          drop_summary_rows(data_format = data_format) |>
          sanitize_admins() |>
          # no admin 1 provided in old format data... need to attach later
          clean_adm2(data_format = data_format)

      }
      )


  }
  return(res)

}


rename_cols_lookup_add_dates_batch <- function(df_list,colname_lookup,lookup_fixed=T){
  df_prepped <- df_list |>
    purrr::map2_dfr(.y= names(df_list), ~{
      print(.y)
      df_names_harmonized <-rename_cols_with_lookup(df = .x,
                                                    lookup = colname_lookup,
                                                    lookup_fixed = lookup_fixed)
      df_names_harmonized |>
        dplyr::mutate(
          file_name= .y,
          year=str_sub(string = file_name,start = 1,end = 4) |> as.numeric(),
          month=str_sub(string = file_name,start = 5,end = 6) |> as.numeric(),
          date= lubridate::ymd(glue::glue("{year}-{month}-01")),
          reporting_level= "admin 2"
        )

    }
    )
  df_prepped |>
    dplyr::mutate(
      total_population= dplyr::if_else(is.na(total_popn_census),total_popn_projected,total_popn_census)
    ) |>
    dplyr::filter(!(year==2016 & adm2_name %in% c("west_gojam","east_gojam")))

}




#' adjust_master_admins
#'
#' @param df master admin (ethiopia from OCHA)
#'
#' @return df with "sanitized admins" and new col with adm1 + 2 + 3 concatenated
#' @export
#'
#' @examples
adjust_master_admin_file <- function(df){
   df |>
    sanitize_admins() |>
    mutate(
      adm_1_2_3=  glue::glue("{adm1_en}-{adm2_en}-{adm3_en}")
    )

}

join_master_admin_to_pre201905_data <- function(df_list, master_adm){
  df_list |>
    purrr::map(
      ~.x |>
        dplyr::left_join(
          master_adm |>
            dplyr::select(adm1_name=adm1_en, adm2_name = adm2_en) |>
            dplyr::distinct(),
          by=c("adm2_name"="adm2_name")
        )
    )

}
# standard_pop_col <- function(df){
#   df |>
#     dplyr::mutate(
#       total_population= dplyr::if_else(is.na(total_popn_census),total_popn_projected,total_popn_census)
#     )
# }

# for current data
bind_rows_add_dates_fill_pop <-  function(df_list, data_format= "current"){
  w_dates <- dplyr::bind_rows(df_list) |>
    dplyr::mutate(
      year=str_sub(string = file_name,start = 1,end = 4) |> as.numeric(),
      month=str_sub(string = file_name,start = 5,end = 6) |> as.numeric(),
      date= lubridate::ymd(glue::glue("{year}-{month}-01")),
      reporting_level= "admin 3"
    ) |>
    dplyr::mutate(
      across(everything(), ~as.character(.x))
    ) |>
    readr::type_convert( ) |>
    dplyr::mutate(
      active_villages_for_the_year =as.numeric(active_villages_for_the_year),
      total_population= dplyr::if_else(is.na(total_popn_census),total_popn_projected,total_popn_census)
    ) |>
    dplyr::select(date,everything())

  # this section is meant to fill in population value that dont make sense
  # this is mostly taken care of w `filter` for old format data in `rename_cols_lookup_add_dates_batch`, new data needs a litle more help
  if(data_format=="old"){
    res <- w_dates
  }

  if(data_format=="current"){
    res <- w_dates |>
      dplyr::group_by(adm1_name,adm2_name,adm3_name) |>
      dplyr::arrange(adm3_name,year,month,date) |>
      tidyr::fill(total_population,.direction="downup") |>
      dplyr::ungroup()
  }
  return(res)
}





