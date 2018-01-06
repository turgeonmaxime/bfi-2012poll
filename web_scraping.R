# BFI 2012 poll web scraping
library(rvest)
library(magrittr)
library(dplyr)

# There are 1205 voters
n_max <- 1205

# Schemas
schema_details <- tibble::tibble(
    ID = numeric(0),
    Name = character(0),
    Details = character(0)
)

schema_votes <- tibble::tibble(
    ID = numeric(0),
    Title = character(0),
    Year = integer(0),
    Director = character(0)
)

for (index in 1:n_max) {
    # Read web content
    web_content <- read_html(paste0("http://www.bfi.org.uk/films-tv-people/sightandsoundpoll2012/voter/",
                                    index))
    
    # Find voter's name
    name <- web_content %>% 
        html_nodes("h1") %>% 
        html_text() %>% 
        trimws()
    
    # Parse voter's details
    details <- web_content %>% 
        html_nodes(".sas-poll-voter-details") %>% 
        html_text() %>% 
        # strsplit("\n") %>% 
        # dplyr::first() %>% 
        # Filter(function(x) x!= "", .) %>% 
        trimws
    
    details <- tibble::tibble(
        ID = index,
        Name = name,
        Details = details
    )
    schema_details %<>% dplyr::bind_rows(details)
    
    # Parse votes
    votes <- web_content %>% 
        html_nodes("table") %>% 
        dplyr::first() %>% 
        html_table() %>% 
        tibble::as_tibble() %>% 
        dplyr::rename(Title = X1,
                      Year = X2,
                      Director = X3) %>% 
        dplyr::mutate(ID = index)
    schema_votes %<>% dplyr::bind_rows(votes)
}

# Clean schema_details
schema_details2 <- schema_details %>% 
    dplyr::mutate(Poll = dplyr::case_when(
        grepl("critic", Details) ~ "Critic",
        grepl("director", Details) ~ "Director",
        TRUE ~ NA_character_
    ),
    Details = trimws(stringr::str_replace(Details, "Voted in the (critics|directors)\u2019 poll", "")),
    n_elements = sapply(strsplit(Details, "\n"), length)) %>% 
    tidyr::separate(Details, sep = "\n", into = c("Details1", "Details2", "Details3",
                                                  "Details4", "Details5", "Country"), 
                    fill = "left") %>% 
    dplyr::mutate(Details = paste(Details1, Details2, Details3, Details4, Details5)) %>% 
    dplyr::mutate(Details = stringr::str_replace_all(Details, "NA", "")) %>% 
    dplyr::select(-Details1, -Details2, -Details3, -Details4, -Details5, -n_elements)

# Clean schema_votes
schema_votes2 <- schema_votes %>% 
    dplyr::mutate(
            Director = dplyr::case_when(
            Director == "Kurosawa Akira" ~ "Akira Kurosawa", 
            Director == "Kurosawa Kiyoshi" ~ "Kiyoshi Kurosawa",
            TRUE ~ Director
        ),
        Year = dplyr::case_when(
            Title == "4 months, 3 weeks and 2 days" ~ 2007L,
            Title == "Amadeus" ~ 1984L,
            Title == "Berlin Alexanderplatz" ~ 1980L, 
            Title == "Dekalog" ~ 1989L,                    
            Title == "Diary" ~ 1983L,
            Title == "Tale of the Wind, A" ~ 1988L,         
            Title == "Turin Horse, The" ~ 2011L,
            TRUE ~ Year
        ))

# Create SQLite db
bfi_db <- DBI::dbConnect(RSQLite::SQLite(), "bfi-2012poll.sqlite")

DBI::dbWriteTable(bfi_db, "details", schema_details2, overwrite = TRUE)
DBI::dbWriteTable(bfi_db, "votes", schema_votes2, overwrite = TRUE)

DBI::dbDisconnect(bfi_db)
