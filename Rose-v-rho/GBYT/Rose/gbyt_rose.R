# gbyt_rose.R
# run Georges Bank yellowtail flounder through Rose analyses for WKFORBIAS
# note changed rose package to include index multipliers during WKFORBIAS

devtools::install_github("cmlegault/ASAPplots")
devtools::install_github("cmlegault/rose")

library(dplyr)
library(ggplot2)
library(ASAPplots)
library(rose)

# set working directory to source file location to begin

# get starting ASAP file
myname <- "GBYT.dat"
file.copy(from = "..\\ASAP\\GBYT11_RETRO.dat", to = myname)

# create working directory
dir.create(".\\working")
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
year.vals <- c(2000, 2005, 2010)
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

# yikes - not even close to zero Mohn's rho yet, need to increase multipliers
mymults2 <- seq(5.5, 8, 0.5)
scenario1a.df <- runRetroMults(myscenario, myname, n.peels, 0, year.vals, mymults2, 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1a.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1a.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()


# yikes - still not there, need to increase multipliers
mymults3 <- seq(8.5, 10, 0.5)
scenario1b.df <- runRetroMults(myscenario, myname, n.peels, 0, year.vals, mymults3, 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1b.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# add a more recent year to this scenario to see if that works with lower cmult
scenario1c.df <- runRetroMults(myscenario, myname, n.peels, 0, 2015, mymults, 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1c.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# add a more recent year to this scenario to see if that works with lower cmult
scenario1d.df <- runRetroMults(myscenario, myname, n.peels, 0, 2013, mymults, 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1d.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# extend cmults for this scenario to see if that works
scenario1e.df <- runRetroMults(myscenario, myname, n.peels, 0, 2013, c(mymults2, mymults3), 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1e.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# for completeness
scenario1f.df <- runRetroMults(myscenario, myname, n.peels, 0, 2015, c(mymults2, mymults3), 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1f.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario1f.df
ggplot(sofar.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1.df$scenario[1]) +
  theme_bw()

# now try cmults with 4 year ramp
myscen2 <- "Ramp4 Cmults"
scenario2.df <- runRetroMults(myscen2, myname, n.peels, 4, c(2005, 2010, 2013, 2015), seq(2, 10, 1), 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario2.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# take another quick peek at how well retro removed for the scenario
scenario2.df
ggplot(rbind(scenario2.df, orig.df), aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario2.df$scenario[1]) +
  theme_bw()

# try going way back in time for cmult
scenario1g.df <- runRetroMults(myscenario, myname, n.peels, 0, 1995, seq(2, 10, 1), 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario1g.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario1g.df, aes(x=cmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario1g.df$scenario[1]) +
  theme_bw()

## now start in with M mults
myscen3 <- "Sudden Mmults"
scenario3.df <- runRetroMults(myscen3, myname, n.peels, 0, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(1.5, 10, 0.5), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario3.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario3.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario3.df$scenario[1]) +
  theme_bw()

# dial in M mults
scenario3b.df <- runRetroMults(myscen3, myname, n.peels, 0, c(1995, 2000, 2005, 2010), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario3b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario3c.df <- runRetroMults(myscen3, myname, n.peels, 0, 2013, 1, seq(2.6, 2.9, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario3c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario3d.df <- runRetroMults(myscen3, myname, n.peels, 0, 2015, 1, seq(4.1, 4.4, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario3d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario3s.df <- rbind(scenario3.df, scenario3b.df, scenario3c.df, scenario3d.df)
ggplot(scenario3s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario3.df$scenario[1]) +
  theme_bw()

# now try with M ramp (4 year variety)
myscen4 <- "Ramp4 Mmults"
scenario4.df <- runRetroMults(myscen4, myname, n.peels, 4, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(1.5, 10, 0.5), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario4.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario4.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario4.df$scenario[1]) +
  theme_bw()

# dial in the Mramp4 cases
# 1995 nailed already
scenario4b.df <- runRetroMults(myscen4, myname, n.peels, 4, c(2000, 2005, 2010, 2013), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario4b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario4c.df <- runRetroMults(myscen4, myname, n.peels, 4, 2015, 1, seq(2.6, 2.9, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario4c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario4s.df <- rbind(scenario4.df, scenario4b.df, scenario4c.df)
ggplot(scenario4s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario4s.df$scenario[1]) +
  theme_bw()

# now try with M ramp (9 year variety)
myscen5 <- "Ramp9 Mmults"
scenario5.df <- runRetroMults(myscen5, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(1.5, 5, 0.5), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario5.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario5.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario5.df$scenario[1]) +
  theme_bw()

# dial in the Mramp9 cases
scenario5b.df <- runRetroMults(myscen5, myname, n.peels, 9, 1995, 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario5b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario5c.df <- runRetroMults(myscen5, myname, n.peels, 9, c(2000, 2005, 2010, 2013, 2015), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario5c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario5s.df <- rbind(scenario5.df, scenario5b.df, scenario5c.df)
ggplot(scenario5s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario5s.df$scenario[1]) +
  theme_bw()

## now try Mmults for young fish (ages 1-3) only
myscen6 <- "Young Ramp9 Mmults"
scenario6.df <- runRetroMults(myscen6, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario6.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario6.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario6.df$scenario[1]) +
  theme_bw()

# dial in young ramp9 mmults cases
scenario6b.df <- runRetroMults(myscen6, myname, n.peels, 9, 1995, 1, seq(5.5, 5.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario6b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario6c.df <- runRetroMults(myscen6, myname, n.peels, 9, c(2000, 2015), 1, seq(4.5, 4.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario6c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario6d.df <- runRetroMults(myscen6, myname, n.peels, 9, c(2005, 2013), 1, seq(4.1, 4.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario6d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario6e.df <- runRetroMults(myscen6, myname, n.peels, 9, 2010, 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario6e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario6s.df <- rbind(scenario6.df, scenario6b.df, scenario6c.df, scenario6d.df, scenario6e.df)
ggplot(scenario6s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario6s.df$scenario[1]) +
  theme_bw()

## now try Mmults for old fish (ages 4-6+) only
myscen7 <- "Old Ramp9 Mmults"
scenario7.df <- runRetroMults(myscen7, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario7.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario7.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario7.df$scenario[1]) +
  theme_bw()

# dial in old ramp9 mmults cases
scenario7b.df <- runRetroMults(myscen7, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario7b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario7c.df <- runRetroMults(myscen7, myname, n.peels, 9, 2015, 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario7c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario7s.df <- rbind(scenario7.df, scenario7b.df, scenario7c.df)
ggplot(scenario7s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario7s.df$scenario[1]) +
  theme_bw()

## young sudden Mmults
myscen8 <- "Young Sudden Mmults"
scenario8.df <- runRetroMults(myscen8, myname, n.peels, 0, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario8.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario8.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario8.df$scenario[1]) +
  theme_bw()

# dial in young sudden Mmults
scenario8b.df <- runRetroMults(myscen8, myname, n.peels, 0, 1995, 1, seq(5.1, 5.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario8b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario8c.df <- runRetroMults(myscen8, myname, n.peels, 0, c(2000, 2010), 1, seq(4.1, 4.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario8c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario8d.df <- runRetroMults(myscen8, myname, n.peels, 0, 2005, 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario8d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario8e.df <- runRetroMults(myscen8, myname, n.peels, 0, 2013, 1, seq(8.5, 8.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario8e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario8s.df <- rbind(scenario8.df, scenario8b.df, scenario8c.df, scenario8d.df, scenario8e.df)
ggplot(scenario8s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario8s.df$scenario[1]) +
  theme_bw()

## now old sudden Mmults
myscen9 <- "Old Sudden Mmults"
scenario9.df <- runRetroMults(myscen9, myname, n.peels, 0, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario9.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario9.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario9.df$scenario[1]) +
  theme_bw()

# dial in old sudden Mmults
scenario9b.df <- runRetroMults(myscen9, myname, n.peels, 0, c(1995, 2000, 2005, 2010), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario9b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario9c.df <- runRetroMults(myscen9, myname, n.peels, 0, 2013, 1, seq(3.1, 3.5, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario9c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario9d.df <- runRetroMults(myscen9, myname, n.peels, 0, 2015, 1, seq(4.5, 4.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario9d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario9s.df <- rbind(scenario9.df, scenario9b.df, scenario9c.df, scenario9d.df)
ggplot(scenario9s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario9s.df$scenario[1]) +
  theme_bw()

## now try ramp4 Mmults for young fish (ages 1-3) only
myscen10 <- "Young Ramp4 Mmults"
scenario10.df <- runRetroMults(myscen10, myname, n.peels, 4, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario10.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario10.df$scenario[1]) +
  theme_bw()

# dial in young ramp9 mmults cases
scenario10b.df <- runRetroMults(myscen10, myname, n.peels, 4, 1995, 1, seq(5.1, 5.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario10c.df <- runRetroMults(myscen10, myname, n.peels, 4, 2000, 1, seq(4.3, 4.7, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario10d.df <- runRetroMults(myscen10, myname, n.peels, 4, c(2005, 2010), 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario10e.df <- runRetroMults(myscen10, myname, n.peels, 4, 2013, 1, seq(4.5, 4.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario10f.df <- runRetroMults(myscen10, myname, n.peels, 4, 2015, 1, seq(8.1, 8.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", FALSE)

sofar.df <- rbind(sofar.df, scenario10f.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario10s.df <- rbind(scenario10.df, scenario10b.df, scenario10c.df, scenario10d.df, scenario10e.df, scenario10f.df)
ggplot(scenario10s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario10s.df$scenario[1]) +
  theme_bw()

## now try ramp4 Mmults for old fish (ages 4-6+) only
myscen11 <- "Old Ramp4 Mmults"
scenario11.df <- runRetroMults(myscen11, myname, n.peels, 4, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(2, 10, 1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario11.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario11.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario11.df$scenario[1]) +
  theme_bw()

# dial in old ramp4 mmults
scenario11b.df <- runRetroMults(myscen11, myname, n.peels, 4, c(1995, 2000, 2005, 2010), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario11b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario11c.df <- runRetroMults(myscen11, myname, n.peels, 4, 2013, 1, seq(3.1, 3.5, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario11c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario11d.df <- runRetroMults(myscen11, myname, n.peels, 4, 2015, 1, seq(3.3, 3.7, 0.1), c(0, 0, 0, 1, 1, 1), "Old", FALSE)

sofar.df <- rbind(sofar.df, scenario11d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

scenario11s.df <- rbind(scenario11.df, scenario11b.df, scenario11c.df, scenario11d.df)
ggplot(scenario11s.df, aes(x=mmult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario11s.df$scenario[1]) +
  theme_bw()

## try cmults with 9 year ramp
myscen12 <- "Ramp9 Cmults"
scenario12.df <- runRetroMults(myscen12, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013, 2015), seq(2, 10, 1), 1, rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario12.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in ramp9 cmults
# no need only cmult=10 comes close (ssbrho = 0.0268)

## for completeness, fill out Ramp9 Mmults
myscen5complete <- "Ramp9 Mmults"
scenario5complete.df <- runRetroMults(myscen5complete, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013, 2015), 1, seq(6, 10, 1), rep(1, nages), "All Ages", FALSE)

sofar.df <- rbind(sofar.df, scenario5complete.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

### new runs during WKFORBIAS meeting
# run a set of Index mults for range of years
year.vals <- c(1995, 2000, 2005, 2010, 2013, 2015)
mymults <- seq(2, 10, 1)
myscen13 <- "Sudden Imults"
scenario13.df <- runRetroMults(myscen13, myname, n.peels, 0, year.vals, 1, 1, rep(1, nages), "All Ages", mymults, FALSE)

write.csv(scenario13.df, file="scenario13.csv", row.names = FALSE)
scenario13.df <- read.csv("scenario13.csv", header = TRUE)



sofar.df <- rbind(sofar.df, scenario13complete.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

ggplot(scenario13.df, aes(x=imult, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle(scenario13.df$scenario[1]) +
  theme_bw()
ggsave("..\\saved\\suddenImults.png")

### note: will need to run runRetroMults for cases that want to save setting save.files=TRUE

# copy results file so not lost
file.copy(from = "sofar.csv", to = "..\\ssbrhodatabase.csv")





## to read back in sofar.csv
sofar.df <- read.csv("sofar.csv", header = TRUE)
sofar.df <- read.csv("..\\ssbrhodatabase.csv")



tdf <- sofar.df %>%
  mutate(Multiplier = mmult * cmult) %>%
  filter(scenario != "Base Case")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1_no_yellow <- cbp1[-5]

ggplot(tdf, aes(x=Multiplier, y=ssbrho, color=as.factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ylim(c(-1, 3)) +
  facet_wrap(~scenario) +
  theme_bw() +
  scale_fill_manual(values = cbp1_no_yellow)
  














# set up functions to do the work

# use data frames to save results so can add scenarios and case descriptions for labeling

# set color palette early and use consistently

# add ramp to cmult and mmult

# make young old cmult option easier to use

# add young old M option???

# use wrapper to define scenario and cases within scenario

# don't repeat so much code - use functions

# use 5 peels as default

# generalize cmults to allow multiple fleets in original ASAP file

# use file.copy and dir.exists 

# make R package Rose-RhoAdj with all the necessary code, executables, file conventions, etc. so can just install package and run using wrappers to determine scenarios and cases

# add option to run SAM or other stock assessment model as well???

# comnpare SSB vs NAA rho adjustments - yes

# workflow
# identify original input file and model
# run original through to catch advice (assume F40???)
# check for major retro
# run ssb and naa rho adjusted through to catch advice (assumes rho adjustment necessary)
# set up different scenarios based on whether cmult, mmult, length of ramp, all age vs old or young
# run cases within scenario to find case that eliminates retro (if possible)
# run selected cases within scenario through to catch advice
# define color palette for selected cases and/or scenarios
# allow plots of ssb rho for cases within scenarios
# allow plots of time series, retros, selectivity, etc. for selected cases within scenarios
# gather all selected cases and compare catch advice in tables and plots
# allow ensemble estimation for Rose - weightings???


