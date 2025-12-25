library(rvest)
library(dplyr)
library(stringr)
library(httr)

start_time <- Sys.time()

# Cities to scrape
cities <- c("delhi", "pune", "gurgaon", "mumbai")

# Set max pages per city
max_pages_to_scrape <- 1
max_empty_allowed <- 5

for (city in cities) {
  
  
  base_url <- paste0("https://www.cardekho.com/used-cars+in+", city)
  all_data <- list()
  empty_count <- 0
  
  page_limit <- if (is.finite(max_pages_to_scrape)) max_pages_to_scrape else 500
  
  for (page in 1:page_limit) {
    url <- if (page == 1) base_url else paste0(base_url, "?page=", page)
    cat(sprintf("[%s] Scraping page %d... ", toupper(city), page))
    
    Sys.sleep(runif(1, 3, 7))
    
    result <- tryCatch({
      response <- GET(
        url,
        add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.9",
          "Connection" = "keep-alive"
        ),
        timeout(45)
      )
      
      if (status_code(response) != 200) {
        cat("Failed (HTTP error).\n")
        NULL
      } else {
        page_content <- content(response, as = "text", encoding = "UTF-8")
        page_obj <- read_html(page_content)
        
        no_results <- html_element(page_obj, "div.no-result-found")
        if (!is.na(no_results) && html_text2(no_results) != "") {
          cat("No more results found.\n")
          NULL
        } else {
          cars <- html_elements(page_obj, "div[class*='gsc_col-md-4']")
          if (length(cars) == 0) cars <- html_elements(page_obj, "div.bottom_container")
          if (length(cars) == 0) {
            cat("No cars found.\n")
            NULL
          } else {
            data <- tibble(
              title = cars %>% html_element("h3 a, h3.title a") %>% html_text2(),
              price = cars %>% html_element("div.price, .Price p") %>% html_text2(),
              details = cars %>% html_element("div.specs, .dotsDetails") %>% html_text2(),
              location = cars %>% html_element("div.location, .distanceText") %>% html_text2()
            ) %>%
              filter(!is.na(title), title != "", !is.na(price), price != "")
            
            if (nrow(data) == 0) {
              cat("No valid data.\n")
              NULL
            } else {
              cat(sprintf("Found %d cars.\n", nrow(data)))
              data
            }
          }
        }
      }
    }, error = function(e) {
      cat("Error.\n")
      NULL
    })
    
    # Store data
    if (!is.null(result) && nrow(result) > 0) {
      all_data[[length(all_data) + 1]] <- result
      empty_count <- 0
    } else {
      empty_count <- empty_count + 1
      if (empty_count >= max_empty_allowed) {
        message(paste("No new cars for", max_empty_allowed, "pages — stopping for", city))
        break
      }
    }
  }
  
  if (length(all_data) == 0) {
    warning(paste("No data collected for city:", city))
    next
  }
  
  # Combine all data for city
  raw_data <- bind_rows(all_data) %>% distinct(title, price, .keep_all = TRUE)
  message(paste("Scraping complete for", city, "-", nrow(raw_data), "raw listings."))
  
  # CLEANING
  cleaned_data <- raw_data %>%
    mutate(
      brand_model = title,
      year = as.integer(str_extract(title, "\\b20\\d{2}\\b")),
      kms = str_extract(details, "[\\d,]+(?=\\s*kms?)") %>%
        str_remove_all(",") %>%
        as.numeric(),
      fuel = str_extract(details, "(Petrol|Diesel|CNG|Electric|Hybrid)"),
      transmission_raw = str_extract(details, "(Automatic|Manual|AMT|CVT|iMT)"),
      transmission = case_when(
        transmission_raw %in% c("AMT", "CVT", "iMT") ~ "Automatic",
        TRUE ~ transmission_raw
      ),
      city_location = ifelse(
        is.na(str_extract(location, "(?<=,\\s)[^,]+$")),
        str_to_title(city),
        str_trim(str_extract(location, "(?<=,\\s)[^,]+$"))
      ),
      price_lakhs = as.numeric(str_extract(price, "[\\d.]+(?=\\s*Lakh)")),
      price_crores = as.numeric(str_extract(price, "[\\d.]+(?=\\s*Cr)")),
      price_inr = case_when(
        !is.na(price_lakhs) ~ price_lakhs * 100000,
        !is.na(price_crores) ~ price_crores * 10000000,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(
      !is.na(price_inr),
      price_inr >= 50000,
      price_inr <= 100000000,
      !is.na(brand_model)
    ) %>%
    distinct(brand_model, price_inr, kms, year, .keep_all = TRUE) %>%
    select(brand_model, price_inr, year, kms, fuel, transmission, city_location) %>%
    arrange(desc(year), price_inr)
  
  message(paste("Cleaned", nrow(cleaned_data), "listings for", city))
  
  # SAVE FILE
  csv_filename <- paste0(city, ".csv")
  write.csv(cleaned_data, csv_filename, row.names = FALSE)
  message(paste("Saved as:", csv_filename))
}

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
message(paste("\n✅ All cities done! Total time:", elapsed, "minutes."))
