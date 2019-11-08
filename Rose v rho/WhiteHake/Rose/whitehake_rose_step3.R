# whitehake_rose_step3.R
# calculate F40% and conduct short term projection at this F

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(ASAPplots)
library(rose)
library(dplyr)
library(tidyr)
library(ggplot2)

br <- read.csv("saved\\bestruns.csv", header = TRUE)
br
nscen <- length(br$scenario)

nrecentyears <- 5
myrecruityears <- seq(2008, 2017)
mynpy <- 3 # number of projection years
target.spr <- 0.40
Ftarget <- rep(NA, nscen)
Fterm <- rep(NA, nscen)
Fratio <- rep(NA, nscen)
ssbtarget <- rep(NA, nscen)
ssbterm <- rep(NA, nscen)
ssbratio <- rep(NA, nscen)
projcatch <- matrix(NA, nrow = nscen, ncol = mynpy)

myname <- "whitehake.dat"
asap.name <- substr(myname, 1, nchar(myname)-4)
asap <- dget(paste0("saved\\", asap.name, "_000.rdat"))
orig <- get_asap_recent_vals(asap, nrecentyears)
orig.rho.table <- read.csv(paste0("saved\\Retro.rho.values_", asap.name, "_000.csv"), header = TRUE)

# compute F to achieve target spawning potential ratio and project catch
ssbpr0 <- calc_ssb_per_r(orig$nAge, orig$ssbwaa, orig$maturity, orig$maa, 0, orig$selx, orig$spawntime)
F.start <- 0.11
spr.f <- function(F.start){
  abs(calc_ssb_per_r(nAge = orig$nAge, ssbwaa = orig$ssbwaa, maturity = orig$maturity, maa = orig$maa, fmult = F.start, selx = orig$selx, spawntime = orig$spawntime) / ssbpr0 - target.spr)
}
yyy <- nlminb(start = F.start, objective = spr.f, lower = 0, upper = 5)
Ftarget.orig <- yyy$par
Fterm.orig <- asap$F.report[asap$parms$nyears]
Fratio.orig <- Fterm.orig / Ftarget.orig
orig.rho.F <- orig.rho.table$f.rho[orig.rho.table$X == "Mohn.rho"]
Fterm.orig.rhoadj <- Fterm.orig / (1 + orig.rho.F)
Fratio.orig.rhoadj <- Fterm.orig.rhoadj / Ftarget.orig

naa.orig <- get_starting_naa(asap, myrecruityears)
projcatch.orig <- project_short_term(naa.orig, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])

# apply SSBrho and NAArho adjustments to orig and get projected catch 
orig.rho.SSB <- orig.rho.table$ssb.rho[orig.rho.table$X == "Mohn.rho"] 
nAge <- asap$parms$nages
myncols <- length(orig.rho.table[1, ])
mynrows <- length(orig.rho.table[, 1])
orig.rho.NAA <- rep(NA, nAge)
iage <- 0
for (icol in (myncols - nAge + 1):myncols){
  iage <- iage + 1
  orig.rho.NAA[iage] <- orig.rho.table[mynrows, icol]
}
naa.orig.ssbrhoadj <- naa.orig / (1 + orig.rho.SSB)
naa.orig.naarhoadj <- naa.orig / (1 + orig.rho.NAA)
rbind(naa.orig, naa.orig.ssbrhoadj, naa.orig.naarhoadj)
projcatch.orig.ssbrhoadj <- project_short_term(naa.orig.ssbrhoadj, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])
projcatch.orig.naarhoadj <- project_short_term(naa.orig.naarhoadj, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])
ssbterm.orig <- asap$SSB[asap$parms$nyears]
ssbterm.orig.rhoadj <- ssbterm.orig / (1 + orig.rho.SSB)
ssbprtarget.orig <- calc_ssb_per_r(orig$nAge, orig$ssbwaa, orig$maturity, orig$maa, Ftarget.orig, orig$selx, orig$spawntime)
ssbtarget.orig <- ssbprtarget.orig * quantile(asap$N.age[,1], 0.75)
ssbratio.orig <- ssbterm.orig / ssbtarget.orig
ssbratio.orig.rhoadj <- ssbterm.orig.rhoadj / ssbtarget.orig

orig.status.df <- data.frame(Scenario = rep(c("orig", "orig.rhoadj"), 3),
                             Metric = rep(c("Terminal", "Target", "Ratio"), each = 2),
                             SSB = c(ssbterm.orig, ssbterm.orig.rhoadj,
                                     ssbtarget.orig, ssbtarget.orig, 
                                     ssbratio.orig, ssbratio.orig.rhoadj),
                             F = c(Fterm.orig, Fterm.orig.rhoadj, 
                                   Ftarget.orig, Ftarget.orig, 
                                   Fratio.orig, Fratio.orig.rhoadj)) %>%
  mutate(Scenario = as.factor(Scenario))

# loop through the scenarios
for (i in 1:nscen){
  fname <- paste0(br$case[i], br$ramplab[i], br$change.year[i], ".rdat")
  asap <- dget(paste0("saved\\", fname))
  mya <- get_asap_recent_vals(asap, nrecentyears)
  
  # compute F to achieve target spawning potential ratio (NOTE: using original M in calcs)
  ssbpr0 <- calc_ssb_per_r(mya$nAge, mya$ssbwaa, mya$maturity, orig$maa, 0, mya$selx, mya$spawntime)
  F.start <- 0.11
  spr.f <- function(F.start){
    abs(calc_ssb_per_r(nAge = mya$nAge, ssbwaa = mya$ssbwaa, maturity = mya$maturity, maa = orig$maa, fmult = F.start, selx = mya$selx, spawntime = mya$spawntime) / ssbpr0 - target.spr)
  }
  yyy <- nlminb(start = F.start, objective = spr.f, lower = 0, upper = 5)
  Ftarget[i] <- yyy$par
  Fterm[i] <- asap$F.report[asap$parms$nyears]
  Fratio[i] <- Fterm[i] / Ftarget[i]
  
  ssbprtarget <- calc_ssb_per_r(mya$nAge, mya$ssbwaa, mya$maturity, orig$maa, Ftarget[i], mya$selx, mya$spawntime)
  ssbtarget[i] <- ssbprtarget * quantile(asap$N.age[,1], 0.75)
  ssbterm[i] <- asap$SSB[asap$parms$nyears]
  ssbratio[i] <- ssbterm[i] / ssbtarget[i]
  
  naa <- get_starting_naa(asap, myrecruityears)
  projcatch[i, ] <- project_short_term(naa, mya$maa, Ftarget[i], mya$selx, mya$cwaa, mynpy, naa[1]) / br$cmult[i]
  
}

scen.status.df <- data.frame(Scenario = rep(paste0(br$case, br$ramplab, br$change.year), 3),
                             Metric = rep(c("Terminal", "Target", "Ratio"), each = nscen),
                             SSB = c(ssbterm, ssbtarget, ssbratio),
                             F = c(Fterm, Ftarget, Fratio)) %>%
  mutate(Scenario = as.factor(Scenario))

rose.df <- scen.status.df %>%
  group_by(Metric) %>%
  summarize(SSB = mean(SSB), F = mean(F)) %>%
  mutate(Scenario = "Rose") %>%
  select(Scenario, Metric, SSB, F)

status.df <- rbind(orig.status.df, scen.status.df, rose.df) %>%
  transform(Metric = factor(Metric, levels = c("Terminal", "Target", "Ratio")))

term.plot <- ggplot(filter(status.df, Metric == "Terminal"), aes(x = SSB, y = F, color = Scenario)) +
  geom_point() +
  geom_segment(data=filter(orig.status.df, Metric == "Terminal"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
  scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
  ggtitle("Terminal Year") +
  geom_point(data=filter(rose.df, Metric == "Terminal"), aes(x=SSB, y=F), shape=11, color="black") +
  theme_bw() + 
  theme(legend.position = "none")
print(term.plot)
ggsave(filename = "saved\\Status_Terminal.png", term.plot)

target.plot <- ggplot(filter(status.df, Metric == "Target"), aes(x = SSB, y = F, color = Scenario)) +
  geom_point() +
  geom_segment(data=filter(orig.status.df, Metric == "Target"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
  scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
  expand_limits(y=0) +
  ggtitle("Targets") +
  geom_point(data=filter(rose.df, Metric == "Target"), aes(x=SSB, y=F), shape=11, color="black") +
  theme_bw() + 
  theme(legend.position = "none")
print(target.plot)
ggsave(filename = "saved\\Status_Target.png", target.plot)

ratio.plot <- ggplot(filter(status.df, Metric == "Ratio"), aes(x = SSB, y = F, color = Scenario)) +
  geom_point() +
  geom_segment(data=filter(orig.status.df, Metric == "Ratio"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
  scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
  xlab("SSB/SSBtarget") +
  ylab("F/Ftarget") +
  ggtitle("Status") +
  geom_point(data=filter(rose.df, Metric == "Ratio"), aes(x=SSB, y=F), shape=11, color="black") +
  theme_bw() + 
  theme(legend.position = "none")
print(ratio.plot)
ggsave(filename = "saved\\Status_Ratio.png", ratio.plot)

f3way_plot <- ggplot(status.df, aes(x=F, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_x") +
  ylab("") +
  theme_bw()
print(f3way_plot)
ggsave(filename = "saved\\F3way.png", f3way_plot)

ssb3way_plot <- ggplot(status.df, aes(x=SSB, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_x") +
  ylab("") +
  theme_bw()
print(ssb3way_plot)
ggsave(filename = "saved\\SSB3way.png", ssb3way_plot)

status_table <- pivot_wider(status.df, id_cols = Scenario, names_from = Metric, values_from = c(F, SSB))
write.csv(status_table, file = "saved\\status_table.csv", row.names = FALSE)

# catch plots
projyear <- asap$parms$endyr + seq(1, mynpy)

orig.c.df <- data.frame(Scenario = rep(c("orig", "orig.SSBrhoadj", "orig.NAArhoadj"), each = 3),
                        Year = rep(projyear, 3),
                        Catch = c(projcatch.orig, projcatch.orig.ssbrhoadj, projcatch.orig.naarhoadj))

scen.c.df <- data.frame(Scenario = rep(paste0(br$case, br$ramplab, br$change.year), 3),
                        Year = rep(projyear, each = nscen),
                        Catch = as.vector(projcatch))

rose.c.df <- scen.c.df %>%
  group_by(Year) %>%
  summarize(Catch = mean(Catch)) %>%
  mutate(Scenario = "Rose") %>%
  select(Scenario, Year, Catch)

c.df <- rbind(orig.c.df, scen.c.df, rose.c.df)

catch.plot <- ggplot(c.df, aes(x=Catch, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~Year) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none")
print(catch.plot)
ggsave(filename = "saved\\catch_advice.png", catch.plot)

catch_advice <- c.df %>%
  pivot_wider(id_cols = Scenario, names_from = Year, values_from = Catch)
write.csv(catch_advice, file = "saved\\catch_advice.csv", row.names = FALSE)
