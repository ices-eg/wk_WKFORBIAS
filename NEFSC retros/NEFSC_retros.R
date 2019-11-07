# NEFSC_retros.R
# plot NEFSC Mohn's rho values for SSB from recent assessments for ICES WKFORBIAS

# set working directory to source file location to begin
# assumes Excel file is located in same directory

library(readxl)
library(dplyr)
library(ggplot2)


dat <- readxl::read_xlsx("NEFSC_Mohn_rho_SSB_values.xlsx", sheet = "data") %>%
  mutate(Retro = ifelse(major_retro == "Yes", "Major", "Minor"))
dat

plot1 <- ggplot(dat, aes(x=TermYear, y=rhoSSB, color=Retro)) +
  geom_jitter(position = position_jitter(width=0.2, seed=14), size=2) +
  geom_hline(yintercept = 0, color="blue", linetype="dashed") +
  xlab("Terminal Year in Assessment") +
  ylab("Mohn's rho for SSB") +
  theme_bw(base_size = 18)
print(plot1)
ggsave(filename = "rho_year.png", plot1)

plot2 <- ggplot(dat, aes(x=rhoSSB, y=Stock, color=factor(TermYear))) +
  geom_point(size=2) +
  xlab("Mohn's rho for SSB") +
  labs(color = "Terminal Year") +
  theme_bw(base_size = 18)
print(plot2)
ggsave(filename = "stock_rho.png", plot2)

multiyear <- dat %>%
  group_by(Stock) %>%
  summarize(n = n()) %>%
  filter(n > 1)

plot3 <- ggplot(filter(dat, Stock %in% multiyear$Stock), aes(x=TermYear, y=rhoSSB, color=Stock)) +
  geom_point(size=2) +
  geom_line() +
  geom_hline(yintercept = 0, color="blue", linetype="dashed") +
  facet_wrap(~ Stock) +
  xlab("Terminal Year in Assessment") +
  ylab("Mohn's rho for SSB") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")
print(plot3)
ggsave(filename = "rho_year_stock_facet.png", plot3)
