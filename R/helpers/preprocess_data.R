##########################################################
# this code preprocesses the data for the app
#
#########################################################


library(DBI)
library(RSQLite)

openpl <- read_csv("data/openpowerlifting-2025-02-22-48890240.csv")
minima <- read_csv("data/minima.csv") %>%
  rename(WeightClass = `Catég.`)

openpl <- openpl %>%
  mutate(Equipment = recode(Equipment,
                            "Wraps" = "Equipped",
                            "Multi-ply" = "Equipped",
                            "Single-ply" = "Equipped",
                            "Straps" = "Equipped",
                            "Unlimited" = "Equipped"),
         Sex = recode(Sex,
                      "M" = "Male",
                      "F" = "Female"),
         year = year(Date),
         Comp = paste(year, MeetName, sep = "_"),
         Tested = case_when(
           is.na(Tested) ~ "No",
           TRUE ~ Tested
         ) ) %>%
  filter(Event == "SBD") %>%
  select(Name,Sex, Event, Equipment, Age, AgeClass, Tested,Division,
          MeetName, Best3SquatKg, Best3BenchKg, Best3DeadliftKg,
         BodyweightKg, WeightClassKg,TotalKg,Dots,year,Comp,MeetCountry,
         Squat1Kg,Squat2Kg,Squat3Kg, Bench1Kg,Country,Date,
         Bench2Kg,Bench3Kg,Deadlift1Kg,Deadlift2Kg,Deadlift3Kg)

# get rid of NA ages
  openpl <- openpl %>%
   filter(Age >= 14)


openpl <- openpl %>%
  filter(Best3BenchKg > 0,
         Best3SquatKg > 0,
         Best3DeadliftKg > 0,
         BodyweightKg > 0,
         Dots > 0,
         TotalKg > 0)

openpl <- openpl %>%
  filter(year > 2000)


all_lifter_names <- c("No Lifter Selected", unique(openpl$Name))


con <- dbConnect(RSQLite::SQLite(), "data/openpl_filtered.sqlite")

dbWriteTable(con, "openpl",  openpl,overwrite = TRUE)

dbDisconnect(con)


saveRDS(minima, "data/minima.rds")
# Save the all_lifter_names to a file
saveRDS(all_lifter_names, "data/all_lifter_names.rds")
qsave(openpl, "data/openpl.qs")
