library(dplyr)

print("Combining, Cleaning, and Sampling Data")
kaggle_data <- read.csv("kaggle.csv", stringsAsFactors = FALSE)
csv_files <- c("pune.csv", "mumbai.csv", "delhi.csv", "gurgaon.csv")
scraped_data <- lapply(csv_files, read.csv, stringsAsFactors = FALSE) %>%
  bind_rows()

kaggle_data <- kaggle_data %>%
  rename(
    Vehicle_Age = myear,
    Transmission = transmission,
    Fuel_Type = fuel,
    Kilometers_Driven = km,
    City_of_Listing = City,
    Brand = oem,
    Model = model,
    Price = listed_price,
    Market_Segment = body
  ) %>%
  select(Price, Kilometers_Driven, Fuel_Type, Transmission,
         City_of_Listing, Brand, Model, Vehicle_Age, Market_Segment)

scraped_data <- scraped_data %>%
  rename(
    Brand_Model = brand_model,
    Price = price_inr,
    Vehicle_Age = year,
    Kilometers_Driven = kms,
    Fuel_Type = fuel,
    Transmission = transmission,
    City_of_Listing = city_location
  ) %>%
  mutate(
    Brand = sub(" .*", "", Brand_Model),
    Model = sub("^[^ ]+ ?", "", Brand_Model),
    Market_Segment = NA_character_
  ) %>%
  select(Price, Kilometers_Driven, Fuel_Type, Transmission,
         City_of_Listing, Brand, Model, Vehicle_Age, Market_Segment)

combined_data <- bind_rows(kaggle_data, scraped_data)

cleaned_data <- combined_data %>%
  mutate(
    Price = as.numeric(Price),
    Kilometers_Driven = as.numeric(Kilometers_Driven),
    Vehicle_Age = as.integer(2025 - as.numeric(Vehicle_Age) + 1)
  ) %>%
  filter(
    !is.na(Price),
    !is.na(Kilometers_Driven),
    !is.na(Vehicle_Age),
    Kilometers_Driven <= 200000               # <-- filter 1: remove > 200000 km
  ) %>%
  mutate(across(where(is.character), ~ tolower(trimws(.)))) %>%
  filter(!grepl("^[0-9]+$", Brand), Brand != "mg") %>%  # <-- filter 2: remove bad brands
  mutate(City_Factor = factor(City_of_Listing,
                              levels = c("pune", "mumbai", "new delhi", "gurgaon"),
                              ordered = TRUE)) %>%
  arrange(City_Factor, City_of_Listing) %>%
  select(-City_Factor) %>%
  mutate(Kilometers_Driven = as.integer(Kilometers_Driven))

sampled_data <- cleaned_data %>%
  filter(City_of_Listing %in% c("pune", "mumbai", "new delhi", "gurgaon")) %>%
  group_by(City_of_Listing) %>%
  slice_sample(n = 500) %>%
  ungroup()

write.csv(sampled_data, "cars_cleaned_v3.csv", row.names = FALSE)

print("Data cleaning and sampling complete.")

