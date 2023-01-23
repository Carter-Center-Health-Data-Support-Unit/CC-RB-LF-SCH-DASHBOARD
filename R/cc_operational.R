


#' carter center operational admins
#'
#' @return data.frame with columns consistent with `eth_adm_master`
#' @export

cc_operational <-  function(){
  tibble::tribble(
  ~adm0_pcode,           ~adm1_en, ~adm1_pcode,  ~adm2_en, ~adm2_pcode,             ~adm3_en,    ~adm3_pcode,                             ~adm_1_2_3,
         "et",          "gambela",     "et_12", "agnewak",   "et_1202",           "refugees", "et_1202_cc01",             "gambela_agnewak_refugees",
         "et",          "gambela",     "et_12", "agnewak",   "et_1202", "refugees (7 camps)", "et_1202_cc02",   "gambela_agnewak_refugees (7 camps)",
         "et", "benishangul_gumz",     "et_06", "metekel",   "et_0602",        "dam_workers", "et_0602_cc01", "benishangul_gumz_metekel_dam_workers"
  )
}


