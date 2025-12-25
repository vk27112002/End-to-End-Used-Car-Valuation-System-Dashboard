# 🚗 Interactive Analysis of the Indian Used Car Market

**MTH-208 Data Science Project**

This project presents a **data-driven and interactive analysis of the Indian used car market**, aimed at improving pricing transparency for buyers and sellers. By combining large-scale data acquisition, statistical analysis, and an interactive R Shiny dashboard, the study identifies the **key drivers of vehicle valuation**, regional pricing differences, and depreciation trends—particularly for electric vehicles (EVs).

🔗 **Live Dashboard:** https://unsaidvolcano.shinyapps.io/mth208proj/

---

## 📌 Problem Statement

India’s pre-owned vehicle market is large and rapidly growing, yet it lacks pricing transparency. Vehicle prices are influenced by multiple interacting factors such as **brand, age, mileage, fuel type, and city**, making fair valuation difficult.

**Objective:**  
To uncover the most significant factors affecting used car prices through a data-driven approach and present insights via an interactive dashboard to support informed decision-making.

---

## 📊 Data Acquisition

A **multi-source data collection strategy** was employed:

- **Web Scraping:**  
  - Developed a scraping pipeline using the `rvest` package in R to extract used car listings from online marketplaces.
- **Kaggle Integration:**  
  - Integrated a Kaggle dataset containing **38,000+ listings** to achieve extensive market coverage.
- **Final Dataset:**  
  - After cleaning and subsetting, approximately **2,000 high-quality listings** were retained to power the Shiny dashboard efficiently.

---

## 🧹 Data Cleaning & Feature Engineering

To ensure analytical reliability and consistency, the dataset underwent extensive preprocessing:

- Removed missing values, duplicates, and inconsistencies  
- Standardized categorical variables (brand, city, fuel type, segment)  
- Converted manufacture year into **vehicle age**  
- Engineered additional features such as:
  - Price-per-kilometer metrics  
  - Age-adjusted valuation indicators  

These steps ensured accurate insights and robust exploratory analysis.

---

## 🔍 Exploratory Data Analysis & Key Insights

### 📈 Market Overview
- Used car prices exhibit a **right-skewed distribution**, with a small number of high-priced listings
- **Petrol vehicles and Maruti cars dominate** the budget segment
- Hatchbacks form the most common vehicle type

---

### ❓ Research Question 1  
**What are the most significant predictors of price?**

Key findings:
- **Vehicle Age:** Strong negative correlation with resale price  
- **Kilometers Driven:** Higher mileage significantly lowers vehicle value  
- **Fuel Type Impact:**  
  - **Electric Vehicles (EVs)** show an average **₹3 lakh annual depreciation**
  - **CNG vehicles** retain value better compared to other fuel types

---

### ❓ Research Question 2  
**How do pricing and brand popularity vary across cities?**

- Core brands and fuel types remain consistent across cities
- **Luxury brands (BMW, Audi)** have strong representation in **Delhi and Mumbai**
- Minimal luxury brand presence observed in cities like **Pune**
- Significant city-level price variations exist even within similar segments

---

### ❓ Research Question 3  
**How does resale value relate to age and mileage across segments?**

- High mileage impacts **convertibles more severely** than other vehicle segments
- **Luxury brands (BMW)** show the steepest depreciation over ownership duration
- User segmentation revealed:
  - Majority classified as **Light Users** (high age, low mileage)
  - Very few **Heavy Users** (low age, high mileage)

---

## 🖥️ Interactive R Shiny Dashboard

The primary deliverable of this project is an **interactive R Shiny dashboard** that enables real-time exploration of the used car market.

### 🔧 Key Features

- **Global Filters:**  
  Filter data by city, brand, market segment, fuel type, price range, mileage, and age
- **Dynamic Insights Panel:**  
  Displays real-time summaries including:
  - Total listings
  - Average price and age
  - Most popular vehicles based on active filters
- **Recommendation Engine:**  
  Compares selected vehicles against segment-level averages to assess deal quality
- **Clean UI/UX:**  
  Intuitive navigation and responsive visualizations for seamless analysis

---

## 🛠️ Tech Stack

- **Programming Language:** R  
- **Web Scraping:** rvest  
- **Data Wrangling:** dplyr, tidyr, stringr  
- **Visualization:** ggplot2  
- **Dashboard:** shiny, shinydashboard  

---

## ⚠️ Limitations & Ethical Considerations

### Limitations
- Web scraping limited to statically loaded pages
- Dataset skewed toward budget-oriented segments
- Analysis is exploratory; no predictive pricing model deployed
- Kaggle data may not reflect the most recent market conditions

### Ethical Considerations
- Followed ethical web scraping practices with rate limiting
- Scraped only publicly accessible information
- No personal or seller-identifiable data collected
- Kaggle data used under open-data licensing terms

---

## ▶️ Running the Project Locally

1. Clone the repository  
2. Ensure `app.R` and `cars_cleaned_v3.csv` are in the same directory  
3. Install required R packages:
   ```r
   install.packages(c(
     "shiny", "shinydashboard", "ggplot2",
     "dplyr", "tidyr", "forcats",
     "scales", "DT"
   ))
