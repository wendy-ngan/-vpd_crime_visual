library(dplyr)
library(purrr)
library(tidyr)
library(ggfortify)
library(feasts)
library(pdftools)
library(stringr)
library(tsibble)

# initialize
i = 0
listofdfs <- list()

# Read data from VPD Crime Report by Neighbourhood that's in pdf form
#  uses pdf_text from pdftools that reads pdf into texts
#  then use str_sub from stringr to read by column positions
#  for report 2018 and 2019 the column positions are different from other years due to formatting 

for (year in c(2015, 2016, 2017, 2020, 2021)) {
  for (month in 1:12) {
    df <-
      tryCatch(
        pdf_text(
          paste0(
            "https://vpd.ca/police/Planning/",
            as.character(year),
            "/",
            as.character(year),
            sprintf("%02d", month),
            "N.pdf"
          )
        )
        ,
        error = function(cond) {
          message(paste(
            "VPD Crime Report for",
            month.abb[month],
            year,
            "is not yet available."
          ))
          return(df)
        }
      ) %>%
      str_split("\n") %>%
      as_tibble(.name_repair = make.names) %>%
      mutate(
        "Neighbourhood" = str_sub(X, 0, 47),
        "Sex Offences" = str_sub(X, 48, 55),
        "Assaults" = str_sub(X, 56, 71),
        "Robbery" = str_sub(X, 72, 80),
        "B&E" = str_sub(X, 81, 87),
        "Theft of MV" = str_sub(X, 88, 101),
        "Theft from Auto" = str_sub(X, 102, 111),
        "Theft<>$5K" = str_sub(X, 112, 122),
        "Arson" = str_sub(X, 123, 129),
        "Mischief" = str_sub(X, 130, 138),
        "Offensive Weapons" = str_sub(X, 139, 148)
      ) %>%
      select(-X) %>%
      mutate_all(str_trim)
    i = i + 1
    listofdfs[[i]] <- df[6:30,]
  }
}

y = length(listofdfs)
for(year in c(2018,2019)) {
  for (Month in 1:12) {
    df <-
      pdf_text(
        paste0(
          "https://vpd.ca/police/Planning/",
          as.character(year),
          "/",
          as.character(year),
          sprintf("%02d", Month),
          "N.pdf"
        )
      ) %>%
      str_split("\n") %>%
      as_tibble(.name_repair = make.names) %>%
      mutate(
        "Neighbourhood" = str_sub(X, 0, 47),
        "Sex Offences" = str_sub(X, 48, 55),
        "Assaults" = str_sub(X, 56, 68),
        "Robbery" = str_sub(X, 69, 72),
        "B&E" = str_sub(X, 73, 79),
        "Theft of MV" = str_sub(X, 80, 92),
        "Theft from Auto" = str_sub(X, 93, 101),
        "Theft<>$5K" = str_sub(X, 102, 113),
        "Arson" = str_sub(X, 114, 120),
        "Mischief" = str_sub(X, 121, 130),
        "Offensive Weapons" = str_sub(X, 131, 137),
      ) %>%
      select(-X) %>%
      mutate_all(str_trim)
    y = y+1
    listofdfs[[y]] <- df[5:29,]
  }
}

# Combine data into one large data frame
temp <- do.call(rbind.data.frame, listofdfs)

#add time column
temp <- temp %>%
  mutate(Year = c(rep(as.character(c(2015,2016,2017,2020)), each = 25*12),
                  rep('2021', 25*(month-1)),
                  rep(as.character(c(2018,2019)), each = 25*12)))
temp <- temp %>%
  mutate(Month = c(rep(rep(month.abb[1:12], each = 25), 4),
                   rep(month.abb[1:month-1], each = 25),
                   rep(rep(month.abb[1:12], each = 25), 2)))
temp <- temp %>%
  mutate(Period = paste(Year, Month)) %>%
  select(-Year,-Month)

# Ensure formatting is right and we don't have cases like "21   4"
temp %>%
  filter_all(any_vars(str_detect(., "\\d*[[:blank:]]+\\d+")))

# Check formatting
table(temp$Neighbourhood)
# Remove unneeded lines scraped earlier, there are 3
temp %>% filter(Neighbourhood == "Please refer to the website for data disclaimer")
temp <- temp %>% filter(Neighbourhood != "Please refer to the website for data disclaimer")

# Noticed there are 2 different types of dashes, unify them
temp$Neighbourhood <- str_replace_all(temp$Neighbourhood, "[[:punct:]]", "-")

# Create tsibble for time series analysis
monthly.series <- temp %>%
  mutate(Month = yearmonth(Period)) %>%
  select(-Period) %>%
  as_tsibble(index = Month,
             key = Neighbourhood)

monthly.series <- monthly.series %>%
  pivot_longer(2:11) %>%
  rename(Offence = name,
         Count = value) %>%
  select(Month, everything())

monthly.series$Count <- as.numeric(monthly.series$Count)

monthly.series$Neighbourhood <- recode(monthly.series$Neighbourhood,
                                       "GRAND TOTAL" = "All Neighbourhoods")

# Save tsibble
saveRDS(monthly.series, "timeseries.RDS")

monthly.series %>%
  filter(Neighbourhood == "Fairview" & Offence == "Robbery") %>%
  autoplot(Count)

monthly.series$Count <- as.numeric(monthly.series$Count)


ggplot(date = data_input(), aes(
  x = as.Date(Month),
  y = Count,
  colour = Neighbourhood
)) +
  geom_point(size = 1) +
  geom_line() +
  ggtitle(paste("Reported", "crime", "Incidents in Selected Neighbourhoods")) +
  scale_x_date("Year", date_labels = "%Y", date_breaks = "year")
