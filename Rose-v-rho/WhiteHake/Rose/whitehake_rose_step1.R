# whitehake_rose_step1.R
# apply Rose approach to 2019 White Hake assessment and compare to rho adjustment

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ggplot2)
library(ASAPplots)
library(rose)

# get starting ASAP file
myname <- "whitehake.dat"
setwd(file.path(getwd(), "working"))

# copy ASAP executables into current and working directories
rose.dir <- find.package("rose")
file.copy(from = file.path(rose.dir, "ASAPexecutables", "ASAP3.exe"), to = "ASAP3.exe")
file.copy(from = file.path(rose.dir, "ASAPexecutables", "ASAPRETRO.exe"), to = "ASAPRETRO.exe")

# copy starting ASAP file into working directory, get info about run, and run retro
file.copy(from = file.path("..\\", myname), to = myname)
asap.dat <- ASAPplots::ReadASAP3DatFile(myname)
nages <- asap.dat$dat$n_ages
terminal.year <- as.numeric(asap.dat$dat[1]) + as.numeric(asap.dat$dat[2]) - 1
n.peels <- 5 # ICES default
retro.first.year <- terminal.year - n.peels
shell(paste("ASAPRETRO.exe", myname, retro.first.year), intern = TRUE)

# run ASAPplots on original data
asap.name <- substr(myname, 1, nchar(myname)-4)
ASAPplots::PlotASAP(wd = ".", asap.name = asap.name)
file.copy(from = paste0(asap.name, "_000.rdat"), to = paste0("..\\saved\\", asap.name, "_000.rdat"))
file.copy(from = paste0("plots\\Retro.rho.values_", asap.name, "_000.csv"), 
          to = paste0("..\\saved\\Retro.rho.values_", asap.name, "_000.csv"))

# compare ASAPplots and calcSSBrho values for Mohn's rho SSB
orig.retros <- read.csv(paste0(".\\plots\\Retro.rho.values_", asap.name, "_000.csv"), header = TRUE)
orig.ssbrho <- orig.retros$ssb.rho[orig.retros$X == "Mohn.rho"]
fxn.ssbrho <- rose::calcSSBrho(myname, n.peels)
c(orig.ssbrho, fxn.ssbrho, round(orig.ssbrho - fxn.ssbrho, 8))

# runRetroMults on base case so have needed data frame to combine later
orig.df <- runRetroMults("Base Case", myname, n.peels, 0, 2018, 1, 1, rep(1, nages), "All Ages", FALSE)
c(orig.ssbrho, orig.df$ssbrho, round(orig.ssbrho - orig.df$ssbrho, 8))

# run a set of catch mults for some arbitrary years
year.vals <- c(2000, 2005, 2010, 2015)
mymults <- seq(1.5, 5, 0.5)
myscenario <- "Sudden Cmults"
scenario1.df <- runRetroMults(myscenario, myname, n.peels, 0, year.vals, mymults, 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(orig.df, scenario1.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario1.df
ggplot(scenario1.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# dial in cmults
scenario1b.df <- runRetroMults(myscenario, myname, n.peels, 0, 2000, seq(2.6, 2.9, 0.1), 1, rep(1, nages), "All Ages", FALSE)

scenario1c.df <- runRetroMults(myscenario, myname, n.peels, 0, c(2005, 2010), seq(2.1, 2.4, 0.1), 1, rep(1, nages), "All Ages", FALSE)

scenario1d.df <- runRetroMults(myscenario, myname, n.peels, 0, 2015, seq(3.6, 3.9, 0.1), 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario1b.df, scenario1c.df, scenario1d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario1s.df <- rbind(scenario1.df, scenario1b.df, scenario1c.df, scenario1d.df)
ggplot(scenario1s.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# cmults ramp4
myscen2 <- "Ramp4 Cmults"
scenario2.df <- runRetroMults(myscen2, myname, n.peels, 4, year.vals, mymults, 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario2.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario2.df
ggplot(scenario2.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario2.df$scenario[1]) +
  theme_bw()

# dial in cmults ramp4
scenario2b.df <- runRetroMults(myscen2, myname, n.peels, 4, c(2000, 2015), seq(2.6, 2.9, 0.1), 1, rep(1, nages), "All Ages", FALSE)

scenario2c.df <- runRetroMults(myscen2, myname, n.peels, 4, c(2005, 2010), seq(2.1, 2.4, 0.1), 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario2b.df, scenario2c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario2s.df <- rbind(scenario2.df, scenario2b.df, scenario2c.df)
ggplot(scenario2s.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario2s.df$scenario[1]) +
  theme_bw()

# cmults ramp9
myscen3 <- "Ramp9 Cmults"
scenario3.df <- runRetroMults(myscen3, myname, n.peels, 9, year.vals, mymults, 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario3.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario3.df
ggplot(scenario3.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario3.df$scenario[1]) +
  theme_bw()

# dial in cmults ramp9
scenario3b.df <- runRetroMults(myscen3, myname, n.peels, 9, 2000, seq(3.6, 3.9, 0.1), 1, rep(1, nages), "All Ages", FALSE)

scenario3c.df <- runRetroMults(myscen3, myname, n.peels, 9, 2005, seq(2.6, 2.9, 0.1), 1, rep(1, nages), "All Ages", FALSE)

scenario3d.df <- runRetroMults(myscen3, myname, n.peels, 9, c(2010, 2015), seq(2.1, 2.4, 0.1), 1, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario3b.df, scenario3c.df, scenario3d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario3s.df <- rbind(scenario3.df, scenario3b.df, scenario3c.df, scenario3d.df)
ggplot(scenario3s.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario3s.df$scenario[1]) +
  theme_bw()

# sudden Mmults
myscen4 <- "Sudden Mmults"
scenario4.df <- runRetroMults(myscen4, myname, n.peels, 0, year.vals, 1, mymults, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar, scenario4.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario4.df
ggplot(scenario4.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario4.df$scenario[1]) +
  theme_bw()

# dial in sudden Mmults
scenario4b.df <- runRetroMults(myscen4, myname, n.peels, 0, c(2000, 2005, 2010), 1, seq(1.6, 1.9, 0.1), rep(1, nages), "All Ages", FALSE)

scenario4c.df <- runRetroMults(myscen4, myname, n.peels, 0, 2015, 1, seq(2.6, 2.9, 0.1), rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario4b.df, scenario4c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario4s.df <- rbind(scenario4.df, scenario4b.df, scenario4c.df)
ggplot(scenario4s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario4s.df$scenario[1]) +
  theme_bw()

# Mmult ramp4
myscen5 <- "Ramp4 Mmults"
scenario5.df <- runRetroMults(myscen5, myname, n.peels, 4, year.vals, 1, mymults, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario5.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario5.df
ggplot(scenario5.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario5.df$scenario[1]) +
  theme_bw()

# dial in Mmults ramp4
scenario5b.df <- runRetroMults(myscen5, myname, n.peels, 4, c(2000, 2005, 2010), 1, seq(1.6, 1.9, 0.1), rep(1, nages), "All Ages", FALSE)

# note 2015 run nailed it at mmult=2

# save work so far
sofar.df <- rbind(sofar.df, scenario5b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario5s.df <- rbind(scenario5.df, scenario5b.df)
ggplot(scenario5s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario5s.df$scenario[1]) +
  theme_bw()

# Mmult ramp9
myscen6 <- "Ramp9 Mmults"
scenario6.df <- runRetroMults(myscen6, myname, n.peels, 9, year.vals, 1, mymults, rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario6.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario6.df
ggplot(scenario6.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario6.df$scenario[1]) +
  theme_bw()

# dial in Mmults ramp9
scenario6b.df <- runRetroMults(myscen6, myname, n.peels, 9, c(2000, 2005, 2010, 2015), 1, seq(1.6, 1.9, 0.1), rep(1, nages), "All Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario6b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario6s.df <- rbind(scenario6.df, scenario6b.df)
ggplot(scenario6s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario6s.df$scenario[1]) +
  theme_bw()

# Old Sudden Mmult (ages 6-9+)
myscen7 <- "Old Sudden Mmults"
scenario7.df <- runRetroMults(myscen7, myname, n.peels, 0, year.vals, 1, mymults, c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario7.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario7.df
ggplot(scenario7.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario7.df$scenario[1]) +
  theme_bw()

# dial in Old Sudden Mmults
scenario7b.df <- runRetroMults(myscen7, myname, n.peels, 0, c(2000, 2005, 2010), 1, seq(2.1, 2.4, 0.1), c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# note 2015 nails it with mmult=4.0

# save work so far
sofar.df <- rbind(sofar.df, scenario7b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario7s.df <- rbind(scenario7.df, scenario7b.df)
ggplot(scenario7s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario7s.df$scenario[1]) +
  theme_bw()

# Old Ramp4 Mmults
myscen8 <- "Old Ramp4 Mmults"
scenario8.df <- runRetroMults(myscen8, myname, n.peels, 4, year.vals, 1, mymults, c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario8.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario8.df
ggplot(scenario8.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario8.df$scenario[1]) +
  theme_bw()

# dial in Old Ramp4 Mmults
scenario8b.df <- runRetroMults(myscen8, myname, n.peels, 4, c(2000, 2005, 2010), 1, seq(2.1, 2.4, 0.1), c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

scenario8c.df <- runRetroMults(myscen8, myname, n.peels, 4, 2015, 1, seq(2.6, 2.9, 0.1), c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario8b.df, scenario8c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario8s.df <- rbind(scenario8.df, scenario8b.df, scenario8c.df)
ggplot(scenario8s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario8s.df$scenario[1]) +
  theme_bw()

# Old Ramp9 Mmults
myscen9 <- "Old Ramp9 Mmults"
scenario9.df <- runRetroMults(myscen9, myname, n.peels, 9, year.vals, 1, mymults, c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario9.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario9.df
ggplot(scenario9.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario9.df$scenario[1]) +
  theme_bw()

# dial in Old Ramp9 Mmults
scenario9b.df <- runRetroMults(myscen9, myname, n.peels, 9, year.vals, 1, seq(2.1, 2.4, 0.1), c(0,0,0,0,0,1,1,1,1), "Old Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario9b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario9s.df <- rbind(scenario9.df, scenario9b.df)
ggplot(scenario9s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario9s.df$scenario[1]) +
  theme_bw()

# Young Sudden Mmults (ages 1-4)
myscen10 <- "Young Sudden Mmults"
scenario10.df <- runRetroMults(myscen10, myname, n.peels, 0, year.vals, 1, mymults, c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario10.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario10.df
ggplot(scenario10.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario10.df$scenario[1]) +
  theme_bw()

# dial in Young Sudden Mmults
scenario10b.df <- runRetroMults(myscen10, myname, n.peels, 0, c(2000, 2005), 1, seq(3.1, 3.5, 0.1), c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

scenario10c.df <- runRetroMults(myscen10, myname, n.peels, 0, 2010, 1, seq(4.6, 4.9, 0.1), c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# note 2015 would require mmult > 5, so didn't carry this case through to higher mmults

# save work so far
sofar.df <- rbind(sofar.df, scenario10b.df, scenario10c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario10s.df <- rbind(scenario10.df, scenario10b.df, scenario10c.df)
ggplot(scenario10s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario10s.df$scenario[1]) +
  theme_bw()

# Young Ramp4 Mmults (ages 1-4)
myscen11 <- "Young Ramp4 Mmults"
scenario11.df <- runRetroMults(myscen11, myname, n.peels, 4, year.vals, 1, mymults, c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario11.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario11.df
ggplot(scenario11.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario11.df$scenario[1]) +
  theme_bw()

# dial in Young Ramp4 Mmults
scenario11b.df <- runRetroMults(myscen11, myname, n.peels, 4, 2000, 1, seq(3.1, 3.4, 0.1), c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# note 2005 and 2010 were spot on with mmults of 3.0 and 3.5 respectively
# note 2015 would require mmult > 5, so didn't carry this case through to higher mmults

# save work so far
sofar.df <- rbind(sofar.df, scenario11b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario11s.df <- rbind(scenario11.df, scenario11b.df)
ggplot(scenario11s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario11s.df$scenario[1]) +
  theme_bw()

# Young Ramp9 Mmults (ages 1-4)
myscen12 <- "Young Ramp9 Mmults"
scenario12.df <- runRetroMults(myscen12, myname, n.peels, 9, year.vals, 1, mymults, c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario12.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario12.df
ggplot(scenario12.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario12.df$scenario[1]) +
  theme_bw()

# dial in Young Ramp9 Mmults
scenario12b.df <- runRetroMults(myscen12, myname, n.peels, 9, c(2000, 2005, 2010), 1, seq(3.1, 3.4, 0.1), c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

scenario12c.df <- runRetroMults(myscen12, myname, n.peels, 9, 2015, 1, seq(4.6, 4.9, 0.1), c(1,1,1,1,0,0,0,0,0), "Young Ages", FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario12b.df, scenario12c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take a quick peek at how well retro removed for the scenario
scenario12s.df <- rbind(scenario12.df, scenario12b.df, scenario12c.df)
ggplot(scenario12s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario12s.df$scenario[1]) +
  theme_bw()

# copy results file so not lost
file.copy(from = "sofar.csv", to = "..\\ssbrhodatabase.csv")



setwd("..") # back up to source file location
# sofar.df <- read.csv("ssbrhodatabase.csv", header = TRUE)
 
# plot em all
tdf <- sofar.df %>%
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
  ylim(c(-0.75, 0.5)) +
  facet_grid(ramplab ~ case) +
  ylab("Mohn's rho for SSB") +
  labs(color = "Change Year") +
  scale_fill_manual(name = "Change Year", values = cbp1_no_yellow) +
  theme_bw()
print(remove_retro_plot)
ggsave(filename = "figs\\remove_retro.png", remove_retro_plot)



