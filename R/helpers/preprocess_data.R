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
         year = year(Date),
         Comp = paste(year, MeetName, sep = "_")) %>%
  filter(Event == "SBD",Equipment == "Raw")

all_lifter_names <- c("No Lifter Selected", unique(openpl$Name))

saveRDS(openpl, "data/openpl.rds")
saveRDS(minima, "data/minima.rds")
# Save the all_lifter_names to a file
saveRDS(all_lifter_names, "data/all_lifter_names.rds")
qsave(openpl, "data/openpl.qs")
