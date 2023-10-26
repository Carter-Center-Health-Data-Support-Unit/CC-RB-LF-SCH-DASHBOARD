library(tidyverse)


#
df_seach_catalogue <-
  tibble(
    date = seq(as_date("2016-01-01"),
    as_date("2023-08-01"),
    by='month'),

    year= year(date),
    month = format(date,"%m"),
    file_rgx = paste0("^",year,month,"_.*\\.xlsx$"),
    fname = str_subset(fp_excel,file_rgx)


    # missing
    # issue = adakdsf duplicating prev month
    # duplicate prev mo
    # first appearance of x admin?

  )

tibble(
  file_name =list.files(data_dir),
  year = str_sub(file_name,start = 1,end = 4),
  month= str_sub (file_name, start=5, end=6),
  date = ym(paste0(year,month))
) %>%
  mutate(
    status= "present"
  ) %>%
  complete(
      date = seq(as_date("2016-01-01"),
                 as_date("2023-08-01"), by= "month"),
      fill = list(status="missing")
  ) %>%
  filter(status!="present")



base::basename(fp_excel
               )
"^201607_.*\\.xlsx$"
df_seach_catalogue$file_rgx %>%
  map(
    \(rgxt){
      str_subset(fp_excel,rgxt)
    }
  )
