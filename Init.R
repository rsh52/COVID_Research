### Read in CSV Files from Open Data Philly ------------------------------------

## COVID Cases Datasets
COVID.Outcome <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_outcome&filename=covid_cases_by_outcome&format=csv&skipfields=cartodb_id")
COVID.Date <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_date&filename=covid_cases_by_date&format=csv&skipfields=cartodb_id")
COVID.ZIP <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_zip&filename=covid_cases_by_zip&format=csv&skipfields=cartodb_id")
COVID.Age <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_age&filename=covid_cases_by_age&format=csv&skipfields=cartodb_id")
COVID.Sex <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_sex&filename=covid_cases_by_sex&format=csv&skipfields=cartodb_id")

## COVID Deaths Datasets
COVID.DeathDate <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_date&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_date")
COVID.DeathZip <- read.csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_zip")
COVID.DeathAge <- read.csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_age")

# COVID Outcome Data -----------------------------------------------------------
# Data Cleaning
COVID.Outcome <- COVID.Outcome[,3:6]
colnames(COVID.Outcome) <- c("TestResult", "Outcome", "Count", "Timestamp")
COVID.Outcome$Outcome[COVID.Outcome$Outcome == "died"] <- "Died"
COVID.Outcome$Outcome[COVID.Outcome$Outcome == "positive"] <- "Positive"
COVID.Outcome$Outcome[COVID.Outcome$Outcome == "negative"] <- "Negative"
COVID.Outcome$TestResult[COVID.Outcome$TestResult == "positive"] <- "Positive"
COVID.Outcome$TestResult[COVID.Outcome$TestResult == "negative"] <- "Negative"
COVID.Outcome$Status <- paste0(COVID.Outcome$TestResult, " - ", COVID.Outcome$Outcome)

# COVID Date Data --------------------------------------------------------------
# Data Cleaning
COVID.Date <- COVID.Date[,3:6]
colnames(COVID.Date) <- c("ResultDate", "Outcome", "Count", "Timestamp")
COVID.Date$Outcome[COVID.Date$Outcome == "positive"] <- "Positive"
COVID.Date$Outcome[COVID.Date$Outcome == "negative"] <- "Negative"

colnames(COVID.DeathDate) <- c("ResultDate", "Outcome", "Count", "Timestamp")
COVID.DeathDate$Outcome[COVID.DeathDate$Outcome == "DIED"] <- "Died"

COVID.DateC <- rbind(COVID.Date, COVID.DeathDate)

# COVID Age --------------------------------------------------------------------
COVID.Age <- COVID.Age[,3:5]
colnames(COVID.Age) <- c("Age", "Count", "TimeStamp")
COVID.Age$Gender <- "All"
COVID.Age$Outcome <- "Positive"

colnames(COVID.DeathAge) <- c("Gender", "Age", "Count", "TimeStamp")
COVID.DeathAge$Outcome <- "Died"

COVID.AgeC <- rbind(COVID.Age, COVID.DeathAge)

