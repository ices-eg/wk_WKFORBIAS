# gbyt_rose_step2.R
# figure out which runs have zero retro, rerun these runs saving needed info

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ggplot2)
library(ASAPplots)
library(rose)

rhodb <- read.csv("ssbrhodatabase.csv")
dim(rhodb)

tdf <- rhodb %>%
  mutate(Multiplier = mmult * cmult) %>%
  filter(scenario != "Base Case") %>%
  mutate(case = ifelse(cmult > 1, "Cmult", paste("Mmult", mselxlab))) %>%
  mutate(ramplab = ifelse(ramp == 0, " Sudden", paste0("Ramp", ramp)))

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1_no_yellow <- cbp1[-5]

remove_retro_plot <- ggplot(tdf, aes(x=Multiplier, y=ssbrho, color=factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ylim(c(-1, 3.5)) +
  facet_grid(ramplab ~ case) +
  ylab("Mohn's rho for SSB") +
  labs(color = "Change Year") +
  scale_fill_manual(name = "Change Year", values = cbp1_no_yellow) +
  theme_bw()
print(remove_retro_plot)
ggsave(filename = "figs\\remove_retro.png", remove_retro_plot)

bestrho <- rhodb %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, change.year, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

nscenarios <- length(bestrho$scenario)

for (i in 1:nscenarios){
  myrun <- rhodb %>%
    mutate(absssbrho = abs(ssbrho)) %>%
    filter(scenario == bestrho$scenario[i],
           change.year == bestrho$change.year[i],
           ramp == bestrho$ramp[i],
           mselxlab == bestrho$mselxlab[i],
           absssbrho == bestrho$min[i])
  if (i == 1){
    bestruns <- myrun
  }else{
    bestruns <- rbind(bestruns, myrun)
  }
}
bestruns

ggplot(bestruns, aes(x=ssbrho, y=scenario, color=as.factor(change.year))) +
  geom_point() +
  facet_wrap(~change.year) +
  xlim(c(min(bestruns$ssbrho), 0.5)) +
  geom_vline(xintercept = c(-0.03, 0.03), color = "red", linetype = "dashed") +
  theme_bw()

ggplot(bestruns, aes(x=change.year, y=ssbrho, color=as.factor(change.year))) +
  geom_point() +
  facet_wrap(~scenario) +
  ylim(c(-0.03, 1)) +
  theme_bw()


# use this to figure out how many runs were within a given range for ssbrho
bestruns %>%
  filter(absssbrho <= 0.03)

## changed mind and went back to just 12 scenarios with best of best for each
bestbestrho <- rhodb %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestbestrho

for (i in 1:length(bestbestrho$scenario)){
  myrun <- rhodb %>%
    mutate(absssbrho = abs(ssbrho)) %>%
    filter(scenario == bestbestrho$scenario[i],
           ramp == bestbestrho$ramp[i],
           mselxlab == bestbestrho$mselxlab[i],
           absssbrho == bestbestrho$min[i])
  if (i == 1){
    useruns <- myrun
  }else{
    useruns <- rbind(useruns, myrun)
  }
}
useruns
myorder <- c("08", "09", "07", "02", "05", "03", "06", "01", "04", "11", "12", "10")
useruns$scen <- myorder
useruns <- useruns[order(useruns$scen), ]
write.csv(useruns, file = paste0("saved\\use12table.csv"), row.names = FALSE)

# change back to working directory to run these 12 cases
setwd(file.path(getwd(), "working"))
myname <- "GBYT.dat"
n.peels <- 5
nages <- 6
for (icase in 1:length(useruns$scenario)){
  myscen <- paste(useruns$scenario[icase], useruns$change.year[icase])
  myramp <- useruns$ramp[icase]
  myyear <- useruns$change.year[icase]
  mycmult <- useruns$cmult[icase]
  mymmult <- useruns$mmult[icase]
  mymselxlab <- useruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young"){
      mymselx <- c(1, 1, 1, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old"){
        mymselx <- c(0, 0, 0, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  runRetroMults(myscen, myname, n.peels, myramp, myyear, mycmult, mymmult, mymselx, mymselxlab, TRUE)
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""))
  print(fname)
  PlotASAP(wd=getwd(), asap.name = fname)
}

# save the necessary files (don't delete anything yet)
ts.df <- data.frame(case = character(),
                    ramplab = character(),
                    Year = integer(),
                    SSB = double(),
                    F = double(),
                    R = double())

myorder <- c("08", "09", "07", "02", "05", "03", "06", "01", "04", "11", "12", "10")
for (icase in 1:length(useruns$scenario)){
  myscen <- paste(useruns$scenario[icase], useruns$change.year[icase])
  myramp <- useruns$ramp[icase]
  myyear <- useruns$change.year[icase]
  mycmult <- useruns$cmult[icase]
  mymmult <- useruns$mmult[icase]
  mymselxlab <- useruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young"){
      mymselx <- c(1, 1, 1, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old"){
        mymselx <- c(0, 0, 0, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""))
  print(fname)
  # file.copy(from = paste0(fname, "_000.rdat"), to = paste0("..\\saved\\", fname, "_000.rdat"))
  # file.copy(from = file.path(getwd(), "plots", paste0("Retro.rho.values_", fname, "_000.csv")), 
  #           to = paste0("..\\saved\\Retro.rho.values_", fname, "_000.csv"))
  file.copy(from = file.path(getwd(), "plots", paste0(fname, ".ALL.PLOTS.pdf")), 
            to = paste0("..\\saved\\scen", myorder[icase], ".pdf"))
  asap <- dget(paste0(fname, "_000.rdat"))
  this.df <- data.frame(case = ifelse(mycmult > 1, "Cmult", paste("Mmult", mymselxlab)),
                        ramplab = ifelse(myramp == 0, " Sudden", paste0("Ramp", myramp)),
                        Year = seq(asap$parms$styr, asap$parms$endyr),
                        SSB = asap$SSB,
                        F = asap$F.report,
                        R = asap$N.age[, 1])
  
  ts.df <- rbind(ts.df, this.df)
}
write.csv(ts.df, file = "..\\saved\\time_series.csv", row.names = FALSE)

setwd("..")
ts.df <- read.csv("saved\\time_series.csv", header = TRUE)

ts.df <- ts.df %>%
  mutate(ramplab = replace(ramplab, ramplab == "Sudden", " Sudden"))

asap <- dget("saved\\GBYT_000.rdat")
base.df <- data.frame(Year = seq(asap$parms$styr, asap$parms$endyr),
                      SSB = asap$SSB,
                      F = asap$F.report,
                      R = asap$N.age[, 1])

ts_ssb_plot <- ggplot(ts.df, aes(x=Year, y=SSB)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=SSB), color = "blue") +
  facet_grid(ramplab ~ case) +
  theme_bw()
print(ts_ssb_plot)
ggsave(filename = "figs\\time_series_ssb.png", ts_ssb_plot)

ts_f_plot <- ggplot(ts.df, aes(x=Year, y=F)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=F), color = "blue") +
  facet_grid(ramplab ~ case) +
  theme_bw()
print(ts_f_plot)
ggsave(filename = "figs\\time_series_f.png", ts_f_plot)

# recruitment in millions
ts_r_plot <- ggplot(ts.df, aes(x=Year, y=R/1000)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=R/1000), color = "blue") +
  facet_grid(ramplab ~ case) +
  ylab("Recruitment") +
  theme_bw()
print(ts_r_plot)
ggsave(filename = "figs\\time_series_r.png", ts_r_plot)

ts_r_plot_limited <- ts_r_plot +
  ylim(c(0, 400))
print(ts_r_plot_limited)  
ggsave(filename = "figs\\time_series_r_limited.png", ts_r_plot_limited)
