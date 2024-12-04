library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)

list.files("functions",full.names = T) %>% map(source)

page <- "https://www.abs.gov.au/statistics/people/crime-and-justice/sort?sort_by=field_abs_release_date_value" %>%
  read_html()

crime_and_justice <-
tibble(
  title = page %>% html_nodes(".abs-layout-title") %>% html_text() %>% str_trim() ,
  release_date = page %>% html_nodes(".datetime") %>% html_text() ,
  reference_period = page %>% html_nodes(".field--label-inline .field__item") %>% html_text(),
  url = page %>% html_nodes(".card , .field--label-visually_hidden .field__item") %>% html_attr(., "href") %>% keep(~ !is.na(.)) %>% paste0("https://www.abs.gov.au/",.)
)

crime_and_justice_df_01 <-
  crime_and_justice %>%
  mutate(releases_link = url %>% str_extract("https://www.abs.gov.au//statistics/people/crime-and-justice/.*?/") %>% unique()) %>%
  mutate(releases_df = map(releases_link, ~get_releases(.)))

crime_and_justice_df_02 <- crime_and_justice_df_01 %>% unnest(releases_df) 

crime_and_justice_df_03 <- crime_and_justice_df_02 %>% mutate(download_df = map2(release_url,row_number(), possibly(function(x,cnt){print(x); print(cnt);get_downloads(x)},NA)))

crime_and_justice_df_04 <- crime_and_justice_df_03 %>% unnest(download_df, keep_empty = T) 

crime_and_justice_df_04$data_link[
  crime_and_justice_df_04$data_link %>% str_extract("\\.xlsx|\\.xls|\\.zip|\\.pdf|Data Cube") %>% is.na() %>% which()]

crime_and_justice_df_05 <–
crime_and_justice_df_04 %>%
 mutate(extension = data_link %>% str_extract("\\.xlsx|\\.xls|\\.zip|\\.pdf|Data Cube") %>% str_replace_all("Data Cub",".zip")) %>%
 mutate(name = row_number() %>% as.character() %>% str_pad( width = 4, pad = "0", side = "left")) %>%
 mutate(file_name = paste0(name,extension)) %>%
 mutate(p = !(file_name %in% list.files())) %>%
 pull(p) %>% sum
 mutate(dl = pmap(list(p,str_replace_all(data_link, " ", "%20"),file_name), possibly(function(p,x,y){if(p){print(y); download.file(url = x, destfile = y, mode = "wb")}},NA)))

crime_and_justice_df_04$data_link[[200]] %>%  
str_replace_all(" ", "%20") %>%
download.file( destfile = "test01.xls", mode = "wb", url = .)


getOption("width")
options("width"=200)

crime_and_justice_df_04 %>% mutate(file.name = row_number()) %>%
  filter(!is.na(download_url)) %>%
  mutate(download_url = str_replace(download_url,"\\.xls$",".xlsx")) %>%


library("reticulate")
library(reticulate)

py_install("pyexcel")
py_install("pyexcel-xls")
py_install("pyexcel-xlsx")

py_run_string("
import pyexcel as p
p.save_book_as(file_name='test.xls', dest_file_name='test.xlsx')
")

mean(is.na(crime_and_justice_df_03$download_df))

crime_and_justice_df_03 %>% filter(is.na(download_df))



(268*8)/60

releases <- 
"https://www.abs.gov.au/statistics/people/crime-and-justice/recorded-crime-offenders" %>%
  read_html() %>%
  html_nodes("#block-views-block-topic-releases-listing-topic-previous-releases-block a") %>%
  html_attr("href") %>%
  ifelse(!str_detect(.,"^https"), paste("https://www.abs.gov.au", ., sep = ""), .) 


crime_and_justice_df_02$release_url[[3]] %>% get_downloads()
crime_and_justice_df_02$release_url[[3]] %>% get_downloads()

crime_and_justice_df_02 %>%
  mutate(downloads_df = map(release_url, ~get_downloads(.))) %>%
  unnest(downloads_df) %>%
  write_csv("crime_and_justice_df.csv")


url <- 

crime_and_justice_df_02$release_url %>% sample(.,1) %>% get_downloads()



get_downloads(url)

get_downloads <- function(url){
  if(!str_detect(url,"AUSSTATS")){
    page <- url %>% read_html() 
    data_name <- page %>% html_nodes(".abs-data-download-content") %>% html_text()
    data_link <-  page %>% html_nodes(".abs-data-download-content") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.abs.gov.au/",.)

    result_df <- tibble(data_name, data_link)

    if(nrow(result_df)==0){
      data_name <- page %>% html_nodes(".file-description-link-formatter") %>% html_text()
      data_link <-  page %>% html_nodes(".file-description-link-formatter") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.abs.gov.au/",.)

      result_df <- tibble(data_name, data_link)
      return(result_df)
    }

    return(result_df)
  }

  if(str_detect(url,"AUSSTATS")){
     page <- url %>% str_replace_all(" ","%20") %>% read_html()      

      dl_ref_df <- 
      tibble(
        x = page %>% html_nodes("a") %>% html_text(),
        ref = page %>% html_nodes("a") %>% html_attr("href")
      ) 

    dl_ref <- dl_ref_df %>% filter(str_detect(x, "Downloads")) %>% pull(ref) %>% .[[1]] %>% paste0("https://www.abs.gov.au",.) %>% read_html()

    data_link <- dl_ref %>% html_nodes(".listentry td") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.abs.gov.au",.)

    result_df <- tibble(data_link)
    return(result_df)
  }

}
