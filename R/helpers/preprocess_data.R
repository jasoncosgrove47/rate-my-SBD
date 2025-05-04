##########################################################
# this code preprocesses the data for the app
#
#########################################################


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


openpl <- openpl %>% filter(Age >= 14, !(is.na(Age)))

#, !is.na(Sex),!is.na(BodyweightKg),!is.na(Division))
#openpl <- openpl %>% filter(!is.na(Best3SquatKg) | !is.na(Best3BenchKg) | !is.na(Best3DeadliftKg))

all_lifter_names <- c("No Lifter Selected", unique(openpl$Name))


saveRDS(minima, "data/minima.rds")
# Save the all_lifter_names to a file
saveRDS(all_lifter_names, "data/all_lifter_names.rds")
qsave(openpl, "data/openpl.qs")
