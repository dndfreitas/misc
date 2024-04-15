library(dplyr)
library(stringr)
library(lubridate)
#---
library(wiesbaden)
library(keyring)
library(janitor)
library(fixest)


# Tutorial Part 1: wiesbaden and keyring ####
# Introduction to the keyring package to securely store credentials
# Introduction to the wiesbaden package to access the GENESIS database

# Regular Log in Check without keyring
test_login(genesis=c(db='regio', 
                     user="your-username", 
                     password="your-password"))

# Or use keyring to store your data safely
key_set_with_raw_value(service = "regio", 
                       username = "your-username",
                       password = "your-password")

# ... and check login with keyring
test_login(genesis=c(db='regio', 
                     user="your-username", 
                     password=
                       key_get(
                         service = "regio", 
                         username = "your-username")
))

# Or pre-storing your credentials in a list
db_credentials <- list(db='regio', user = "your-username", password = key_get(service = "regio", username = "your-username")) # stored password securely using keyring package
test_login(genesis=db_credentials)

# Load built-in datasets
d <- retrieve_datalist(tableseries="124*", genesis=db_credentials)

# Filter for certain tables with municipal data
d <- subset(d, grepl("Gemeinde", description)) 

# Load data from a specific table by the tablename
df <- retrieve_data(
  tablename="12411GJ001",
  startyear = "2009", 
  endyear = "2021", 
  #  regionalschluessel = "09*", # To filter for certain states / regions etc.
  regionalmerkmal = "GEMEIN",
  genesis=db_credentials
)

# Tutorial Part 2: Using janitor to clean data ####
pop <- df %>%
  clean_names() %>% # all names to snake_case
  remove_constant() %>% # removes all columns where every obs. has the same value
  rename(
    mun_code = gemein,
    year = stag,
    pop = bevstd_val
  ) %>%
  select(-bevstd_qual) %>%
  mutate(
    year = year(as.Date(year, "%d.%m.%Y")) #extract year from date with lubridate
  ) 

# Tutorial Part 3: Multiple regressions at once with fixest ####

# Load built-in datasets for demonstration
data(mtcars)
data(iris)

etable( # Embedding regression in a table
  feols( # Fixed Effects OLS using multiple dependent variables 
    c(mpg, disp) ~ vs + am 
    + csw0(gear, carb, hp, drat, wt, qsec), # and sequentially adding controls with csw0 option 
    vcov = "HC1",
    data = mtcars
  ),
  view = T, # Shows table in browser
  dict = c(
    "mpg" = "Miles per Gallon",
    "disp" = "Displacement",
    "vs" = "Engine Shape (0 = V-shaped, 1 = straight)",
    "am" = "Transmission (0 = automatic, 1 = manual)",
    "gear" = "Number of Forward Gears",
    "carb" = "Number of Carburetors",
    "hp" = "Gross Horsepower",
    "drat" = "Rear Axle Ratio",
    "wt" = "Weight (1000 lbs)",
    "qsec" = "1/4 Mile Time"
  )
)

etable(
  feols(
    Sepal.Length ~ Species
    + sw0(Petal.Length, Petal.Width), # sw0 also possible (only one control at a time) - 0 indicates if regression without controls shall also be performed ot not; csw and sw also possible!
    vcov = "HC1",
    data = iris
  ),
  view = T, # Shows table in browser
  dict = c(
    "Sepal.Length" = "Sepal Length",
    "Sepal.Width" = "Sepal Width",
    "Species" = "Iris Species",
    "Petal.Length" = "Petal Length",
    "Petal.Width" = "Petal Width"
  )
)
