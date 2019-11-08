# whitehake_rose_step2.R
# determine which multipliers removed retro

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
           absssbrho == bestrho$min[i]) %>%
    mutate(Multiplier = cmult * mmult) %>%
    mutate(case = ifelse(cmult > 1, "Cmult", paste("Mmult", mselxlab)),
           ramplab = ifelse(ramp == 0, " Sudden", paste0("Ramp", ramp))) %>%
    mutate(scenlab = paste(case, ramplab))
  if (i == 1){
    bestruns <- myrun
  }else{
    bestruns <- rbind(bestruns, myrun)
  }
}
bestruns <- bestruns %>%
  arrange(case, ramplab, change.year)
bestruns
write.csv(bestruns, file = "saved\\bestruns.csv", row.names = FALSE)

br_rho_plot <- ggplot(bestruns, aes(x=ssbrho, y=reorder(scenlab, desc(scenlab)), color=as.factor(change.year))) +
  geom_point() +
  facet_wrap(~change.year) +
  ylab("") +
  theme_bw()
print(br_rho_plot)
ggsave(filename = "figs\\bestruns_ssbrho.png", br_rho_plot)

br_rho_multiplier_plot <- ggplot(bestruns, aes(x=Multiplier, y=ssbrho, color=as.factor(change.year))) +
  geom_point() +
  facet_grid(ramplab ~ case) +
  theme_bw()
print(br_rho_multiplier_plot)
ggsave(filename = "figs\\bestruns_ssbrho_multiplier.png", br_rho_multiplier_plot)

# change back to working directory to run these cases
setwd(file.path(getwd(), "working"))
myname <- "whitehake.dat"
n.peels <- 5
nages <- 9
#for (icase in 1:length(bestruns$scenario)){
for (icase in 25:length(bestruns$scenario)){
  myscen <- paste0(bestruns$case[icase], bestruns$ramplab[icase], bestruns$change.year[icase])
  myramp <- bestruns$ramp[icase]
  myyear <- bestruns$change.year[icase]
  mycmult <- bestruns$cmult[icase]
  mymmult <- bestruns$mmult[icase]
  mymselxlab <- bestruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young Ages"){
      mymselx <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old Ages"){
        mymselx <- c(0, 0, 0, 0,0, 1, 1, 1, 1)
      } else {
        stop("something wrong")
      }
    }
  }
  runRetroMults(myscen, myname, n.peels, myramp, myyear, mycmult, mymmult, mymselx, mymselxlab, TRUE)
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""))
  print(fname)
  PlotASAP(wd=getwd(), asap.name = fname)
}

# save the necessary files, don't delete anything yet
ts.df <- data.frame(case = character(),
                    ramplab = character(),
                    change.year = integer(),
                    Year = integer(),
                    SSB = double(),
                    F = double(),
                    R = double())

for (icase in 1:length(bestruns$scenario)){
  myscen <- paste0(bestruns$case[icase], bestruns$ramplab[icase], bestruns$change.year[icase])
  myramp <- bestruns$ramp[icase]
  myyear <- bestruns$change.year[icase]
  mycmult <- bestruns$cmult[icase]
  mymmult <- bestruns$mmult[icase]
  mymselxlab <- bestruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young Ages"){
      mymselx <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old Ages"){
        mymselx <- c(0, 0, 0, 0, 0, 1, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""))
  print(fname)
  file.copy(from = paste0(fname, "_000.rdat"), to = paste0("..\\saved\\", myscen, ".rdat"))
  file.copy(from = file.path(getwd(), "plots", paste0("Retro.rho.values_", fname, "_000.csv")),            to = paste0("..\\saved\\Retro.rho.values_", myscen, ".csv"))
  file.copy(from = file.path(getwd(), "plots", paste0(fname, ".ALL.PLOTS.pdf")), 
            to = paste0("..\\saved\\scen", myscen, ".pdf"))
  asap <- dget(paste0(fname, "_000.rdat"))
  this.df <- data.frame(case = ifelse(mycmult > 1, "Cmult", paste("Mmult", mymselxlab)),
                        ramplab = ifelse(myramp == 0, " Sudden", paste0("Ramp", myramp)),
                        change.year = myyear,
                        Year = seq(asap$parms$styr, asap$parms$endyr),
                        SSB = asap$SSB,
                        F = asap$F.report,
                        R = asap$N.age[, 1])
  
  ts.df <- rbind(ts.df, this.df)
}
ts.df
write.csv(ts.df, file = "..\\saved\\time_series.csv", row.names = FALSE)
ts.df <- read.csv("..\\saved\\time_series.csv", header = TRUE)

# back up to base directory
setwd("..")
asap <- dget("saved\\whitehake_000.rdat")
base.df <- data.frame(Year = seq(asap$parms$styr, asap$parms$endyr),
                      SSB = asap$SSB,
                      F = asap$F.report,
                      R = asap$N.age[, 1])

ts_ssb_plot <- ggplot(ts.df, aes(x=Year, y=SSB, color=factor(change.year))) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=SSB), color = "blue") +
  facet_grid(ramplab ~ case) +
  labs(color = "Change Year") +
  theme_bw()
print(ts_ssb_plot)
ggsave(filename = "figs\\time_series_ssb.png", ts_ssb_plot)

ts_ssb_plot2 <- ggplot(ts.df, aes(x=Year, y=SSB, color=ramplab)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=SSB), color = "blue") +
  facet_grid(change.year ~ case) +
  labs(color = "Ramp") +
  theme_bw()
print(ts_ssb_plot2)
ggsave(filename = "figs\\time_series_ssb2.png", ts_ssb_plot2)

ts_f_plot <- ggplot(ts.df, aes(x=Year, y=F, color=factor(change.year))) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=F), color = "blue") +
  facet_grid(ramplab ~ case) +
  labs(color = "Change Year") +
  theme_bw()
print(ts_f_plot)
ggsave(filename = "figs\\time_series_f.png", ts_f_plot)

ts_f_plot2 <- ggplot(ts.df, aes(x=Year, y=F, color=ramplab)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=F), color = "blue") +
  facet_grid(change.year ~ case) +
  labs(color = "Ramp") +
  theme_bw()
print(ts_f_plot2)
ggsave(filename = "figs\\time_series_f2.png", ts_f_plot2)

# recruitment in millions
ts_r_plot <- ggplot(ts.df, aes(x=Year, y=R/1000, color=factor(change.year))) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=R/1000), color = "blue") +
  facet_grid(ramplab ~ case) +
  labs(color = "Change Year") +
  ylab("Recruitment") +
  theme_bw()
print(ts_r_plot)
ggsave(filename = "figs\\time_series_r.png", ts_r_plot)

ts_r_plot2 <- ggplot(ts.df, aes(x=Year, y=R/1000, color=ramplab)) +
  geom_point() +
  geom_line() +
  geom_line(data=base.df, aes(x=Year, y=R/1000), color = "blue") +
  facet_grid(change.year ~ case) +
  labs(color = "Ramp") +
  ylab("Recruitment") +
  theme_bw()
print(ts_r_plot2)
ggsave(filename = "figs\\time_series_r2.png", ts_r_plot2)
