
### read data ##################
read_data <- function(data_path =path,
                      sheet_contains = "Total",
                      additional_sheets =NULL,
                      cols_words_replace_to_na = "x_|_for_oncho_lf"){

  cols_replace_to_na <- paste0(cols_words_replace_to_na,collapse = "|")

  sheet_name <- excel_sheets(data_path)
  sheet_name_total<- sheet_name[grepl(sheet_contains,x = sheet_name)]
  sheets_name <- c(additional_sheets,sheet_name_total)

  df_all <- list()
  for (i in sheets_name) {
    data_read <- read.xlsx(data_path,sheet = i,skipEmptyRows = T,
                           skipEmptyCols = T,
                           startRow = 2,
                           fillMergedCells = T)  %>%
      type.convert()  %>% mash_colnames(n_name_rows = 3,sep = ".") %>%
      mutate(filename = paste0(data_path),
             tab_name = i)
    names(data_read) <- names(data_read) %>% snakecase::to_snake_case() %>%
      str_replace_all(cols_replace_to_na,"")
    names(data_read) <- gsub("^[0-9]+[//_]","",names(data_read))
    df_all[[snakecase::to_snake_case(i)]]  <- data_read %>%
      filter(!is.na(name_of_woredas)) %>%
      filter(!grepl("total|Total",name_of_woredas))
  }
  return(df_all)
  }
################ check cols name ###############

check_cols_name <- function(df_list) {
  cols_name <- map(df_list,~names(.x))
  all_cols_name <- unlist(cols_name) %>% unique()
  df <- data.frame(cols_name_all = all_cols_name)

  for (i in names(df_list)) {
    list_name <- paste0(i)
    df <- df %>% mutate(!!sym(list_name) := cols_name_all %in% names(df_list[[i]]))
  }


  df %>% filter(!rowSums(df[names(df_list)]) == length(names(df_list)))

}


####### bind and check rows ##################



bind_data <- function(df_list, needed_cols) {
  check_name_list <- list()
  for(i in names(df_list)){
    # print(i)
    df <- df_list[[i]]
    check_name_list[[i]] <- tibble(
      dataframe_name = paste0(i),
      column_status = if(all(needed_cols %in% names(df))) {
        "All columns found in dataset"} else {
          paste0(needed_cols[!needed_cols %in% names(df)], " was/were not found in the dataset")
        }
      )
  }

  check_status <-do.call("bind_rows",check_name_list)

  check_status_not_found <- check_status%>%
    filter(column_status != "All columns found in dataset")

  to_ignore <- check_status_not_found$dataframe_name

  if(length(to_ignore)>0){warning(paste0(to_ignore, " was/were ignored due to missing columns. Have a look to check_column df from the output list"))}

  df_list_to_bind <- df_list[!names(df_list) %in% to_ignore]

 binded_df <- do.call("bind_rows",df_list_to_bind)

 return(list(
   check_status = check_status,
   binded_df= binded_df
 ))
}






