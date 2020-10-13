### Read in CSV Files from Open Data Philly ------------------------------------

## COVID Cases Datasets
COVID.Outcome <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_outcome&filename=covid_cases_by_outcome&format=csv&skipfields=cartodb_id")
COVID.Date <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_date&filename=covid_cases_by_date&format=csv&skipfields=cartodb_id")
COVID.ZIP <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_zip&filename=covid_cases_by_zip&format=csv&skipfields=cartodb_id")
COVID.Age <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_age&filename=covid_cases_by_age&format=csv&skipfields=cartodb_id")
COVID.Gender <- read_csv("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_sex&filename=covid_cases_by_Gender&format=csv&skipfields=cartodb_id")
COVID.Race <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_cases_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_cases_by_race")

## COVID Deaths Datasets
COVID.DeathDate <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_date&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_date")
COVID.DeathZip <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_zip&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_zip")
COVID.DeathAge <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_age&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_age")
COVID.DeathRace <- read_csv("https://phl.carto.com/api/v2/sql?filename=covid_deaths_by_race&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20covid_deaths_by_race")

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
colnames(COVID.Date) <- c("ResultDate", "Count", "Outcome", "Timestamp")
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

# COVID Gender Breakdown -------------------------------------------------------
COVID.Gender <- COVID.Gender[,3:5]
colnames(COVID.Gender) <- c("Gender", "Count", "TimeStamp")
COVID.Gender <- COVID.Gender %>% 
  # filter(Gender != "UNKNOWN") %>% # Removed only for simplicity
  select(Gender, Count) %>% 
  mutate(Outcome = "Positive")

COVID.GenderDeath <- COVID.DeathAge %>% 
  group_by(Gender) %>% 
  summarise(Count = sum(Count)) %>% 
  mutate(Outcome = "Died")

COVID.Gender <- rbind(COVID.Gender, COVID.GenderDeath)
COVID.Gender <- COVID.Gender[,c(1,3,2)] # Reorder to display properly

# COVID Race Breakdown ---------------------------------------------------------
COVID.Race <- COVID.Race %>% 
  filter(!is.na(racial_identity)) %>% 
  mutate(
    Percentage = round(count/sum(count),3)*100
  )

COVID.Race$racial_identity[COVID.Race$racial_identity == "HISPANIC"] <- "Hispanic"
COVID.Race$racial_identity[COVID.Race$racial_identity == "OTHER"] <- "Other"
COVID.Race$racial_identity[COVID.Race$racial_identity == "UNKNOWN"] <- "Unknown"

colnames(COVID.Race) <- c("Race", "Count", "Timestamp", "Percentage")

# COVID Race Death Breakdown ---------------------------------------------------
COVID.DeathRace <- COVID.DeathRace %>% 
  filter(!is.na(racial_identity)) %>% 
  mutate(
    Percentage = round(count/sum(count), 3)*100
  )

COVID.DeathRace$racial_identity[COVID.DeathRace$racial_identity == "UNKNOWN"] <- "Unknown"
COVID.DeathRace$racial_identity[COVID.DeathRace$racial_identity == "AFRICAN AMERICAN"] <- "African American"
COVID.DeathRace$racial_identity[COVID.DeathRace$racial_identity == "HISPANIC"] <- "Hispanic"

colnames(COVID.DeathRace) <- c("Race", "Count", "Timestamp", "Percentage")

# COVID Rolling Avg ------------------------------------------------------------
alldates <- data.frame(ResultDate = seq(min(COVID.DateC$ResultDate, na.rm = T), max(COVID.DateC$ResultDate, na.rm = T), 1))

roll.wk <- COVID.DateC %>% 
  drop_na() %>% 
  full_join(alldates, by = "ResultDate") %>% 
  # replace_na(0) %>% 
  arrange(ResultDate) %>% 
  group_by(Outcome) %>% 
  mutate(
    RollWindow = round(zoo::rollapplyr(Count, width = 7, FUN = mean, fill = NA),1)
  )


