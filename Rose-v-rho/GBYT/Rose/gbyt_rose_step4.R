# gbyt_rose_step4.R
# compare Rose and rho-adjusted to expanded survey biomass estimates

# set working directory to source file location

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(ASAPplots)
library(rose)
library(dplyr)
library(tidyr)
library(ggplot2)

# get the expanded survey data
surveyb <- read.csv("surveyB\\surveyB.csv", header = TRUE) %>%
  mutate(surveyB = (DFO + Spring + Fall_lag) / 3,
         surveyBcv = (CVDFO + CVSpring + CVFall_lag) / 3)
surveyb <- surveyb %>%
  mutate(surveyBlow = surveyB * (1.0 - 1.96 * surveyBcv),
         surveyBhigh = surveyB * (1.0 + 1.96 * surveyBcv)) %>%
  filter(Year <= 2018)
surveyb

# get SSB time series
ts.df <- read.csv("saved\\time_series.csv", header = TRUE)
ts.df

# get orig run
orig <- dget("saved\\GBYT_000.rdat")
orig.df <- data.frame(Year = seq(orig$parms$styr, orig$parms$endyr),
                      SSB = orig$SSB)

# rho-adjust orig run
orig.rho <- 2.14
orig_SSB_2018 <- filter(orig.df, Year == 2018) %>%
  select(SSB)

rhoadj <- data.frame(Year = 2018,
                     SSB = orig_SSB_2018 / (1 + orig.rho))

# make plots
ts_ssb_survey_plot <- ggplot(filter(ts.df, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=SSB)) +
  geom_line(aes(y=SSB)) +
  geom_line(data=surveyb, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  facet_grid(ramplab ~ case) +
  ggtitle("Blue = Expanded Survey, Red = Orig ASAP, Black = Modified ASAP") +
  theme_bw()
print(ts_ssb_survey_plot)
ggsave(filename = "saved\\ts_ssb_survey_ribbon.png", ts_ssb_survey_plot)

# now just Rose SSB
rose_ssb <- ts.df %>%
  group_by(Year) %>%
  summarize(roseSSB = mean(SSB))

ts_rose_ssb_survey_plot <- ggplot(filter(rose_ssb, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=roseSSB)) +
  geom_line(aes(y=roseSSB)) +
  geom_line(data=surveyb, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  ylab("SSB") +
  ggtitle("Blue = Expanded Survey, Red = Orig ASAP, Black = Rose") +
  theme_bw()
print(ts_rose_ssb_survey_plot)
ggsave(filename = "saved\\ts_rose_ssb_survey_ribbon.png", ts_rose_ssb_survey_plot)
