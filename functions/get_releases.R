
get_releases <- function(url) {

  old_releases_url <- 
    url %>%
    read_html() %>%
    html_nodes("#block-views-block-topic-releases-listing-topic-previous-releases-block a") %>%
    html_attr("href") %>%
    ifelse(!str_detect(.,"^https"), paste("https://www.abs.gov.au", ., sep = ""), .) 

  old_releases_text <- 
    url %>%
    read_html() %>%
    html_nodes("#block-views-block-topic-releases-listing-topic-previous-releases-block a") %>%
    html_text()

  new_release <- 
    url %>%
    read_html() %>%
    html_nodes("#block-views-block-topic-releases-listing-topic-latest-release-block a") %>%
    html_attr("href") %>%
    ifelse(!str_detect(.,"^https"), paste("https://www.abs.gov.au", ., sep = ""), .) 

  new_release_text <- 
    url %>%
    read_html() %>%
    html_nodes("#block-views-block-topic-releases-listing-topic-latest-release-block a") %>%
    html_text() 

  tibble(
    release_text = c(new_release_text, old_releases_text),
    release_url = c(new_release, old_releases_url)
    )

}

