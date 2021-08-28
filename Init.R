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

COVID.Outcome <- COVID.Outcome %>% 
  select(test_result, 
         covid_outcome, 
         count, 
         etl_timestamp) %>% 
  rename(Outcome = covid_outcome,
         TestResult = test_result,
         Count = count,
         TimeStamp = etl_timestamp) %>% 
  mutate(
    Outcome = case_when(
      Outcome == "died" ~ "Died",
      Outcome == "positive" ~ "Positive",
      Outcome == "negative" ~ "Negative",
      TRUE ~ NA_character_),
    TestResult = case_when(
      TestResult == "positive" ~ "Positive",
      TestResult == "negative" ~ "Negative",
      TRUE ~ NA_character_
    ),
    Status = paste(TestResult, " - ", Outcome)
  )

# COVID Date Data --------------------------------------------------------------
# Data Cleaning

COVID.Date <- COVID.Date %>% 
  select(collection_date,
         count,
         test_result,
         etl_timestamp) %>% 
  rename(ResultDate = collection_date,
         Count = count,
         Outcome = test_result,
         TimeStamp = etl_timestamp) %>% 
  mutate(
    Outcome = case_when(
      Outcome == "positive" ~ "Positive",
      Outcome == "negative" ~ "Negative",
      TRUE ~ NA_character_
    )
  )

COVID.DeathDate <- COVID.DeathDate %>% 
  select(clinical_date_of_death,
         count,
         covid_outcome,
         etl_timestamp) %>% 
  rename(ResultDate = clinical_date_of_death,
         Count = count,
         Outcome = covid_outcome,
         TimeStamp = etl_timestamp) %>% 
  mutate(
    Outcome = case_when(
      Outcome == "DIED" ~ "Died",
      TRUE ~ NA_character_
    )
  )

COVID.DateC <- rbind(COVID.Date, COVID.DeathDate)

# COVID Age --------------------------------------------------------------------
COVID.Age <- COVID.Age %>% 
  select(
    age,
    count, 
    etl_timestamp
  ) %>% 
  rename(
    Age = age,
    Count = count,
    TimeStamp = etl_timestamp
  ) %>% 
  mutate(
    Gender = "All",
    Outcome = "Positive"
  )

COVID.DeathAge <- COVID.DeathAge %>% 
  select(
    age,
    count,
    etl_timestamp,
    gender
  ) %>% 
  rename(
    Gender = gender,
    Age = age,
    Count = count,
    TimeStamp = etl_timestamp
  ) %>% 
  mutate(
    Outcome = "Died"
  )

COVID.AgeC <- rbind(COVID.Age, COVID.DeathAge)

# COVID Gender Breakdown -------------------------------------------------------
COVID.Gender <- COVID.Gender %>% 
  select(
    sex,
    count,
    etl_timestamp
  ) %>% 
  rename(
    Gender = sex,
    Count = count,
    TimeStamp = etl_timestamp
  ) %>% 
  transmute(
    Gender = case_when(
      Gender == "UNKNOWN"~ "Unknown",
      TRUE ~ Gender),
    Count,
    Outcome = "Positive"
  )

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
    Percentage = round(count/sum(count),3)*100,
    racial_identity = case_when(
      racial_identity == "AMERICAN INDIAN" ~ "American Indian",
      racial_identity == "PACIFIC ISLANDER" ~ "Pacific Islander",
      racial_identity == "NATIVE AMERICAN" ~ "Native American",
      racial_identity == "BLACK" ~ "Black",
      racial_identity == "HISPANIC" ~ "Hispanic",
      racial_identity == "OTHER" ~ "Other",
      racial_identity == "UNKNOWN" ~ "Unknown",
      racial_identity == "DECLINE" ~ "Declined to Specify",
      TRUE ~ racial_identity
    )
  ) %>% 
  rename(
    Race = racial_identity,
    Count = count,
    TimeStamp = etl_timestamp
  )

# COVID Race Death Breakdown ---------------------------------------------------
COVID.DeathRace <- COVID.DeathRace %>% 
  filter(!is.na(racial_identity)) %>% 
  mutate(
    Percentage = round(count/sum(count), 3)*100,
    racial_identity = case_when(
      racial_identity == "UNKNOWN" ~ "Unknown",
      racial_identity == "AFRICAN AMERICAN" ~ "African American",
      racial_identity == "HISPANIC" ~ "Hispanic",
      TRUE ~ racial_identity
    )
  ) %>% 
  rename(
    Race = racial_identity,
    Count = count,
    TimeStamp = etl_timestamp
  )
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


