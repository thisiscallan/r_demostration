 
# Callan Deline
# 22 Feb 2018


library(tidyverse)
library(ggthemes)
library(broom)
library(stargazer)
library(cowplot)
options(stringsAsFactors = FALSE)


df <- read.csv("Session10/data/FFH_replication.csv")

## Analysis 1 

primes.L <- lm(L ~ factor(Prime), df) %>% 
  tidy () %>% # makes regression into a table format
  mutate(outcome = "Local Resettlement")

primes.US <- lm(US ~ factor(Prime), df) %>% 
  tidy () %>% # regression to table format
  mutate(outcome = "National Resettlement")
  
df1 <- bind_rows(primes.L, primes.US) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(Var = ifelse(term == "factor(Prime)1", "Threatening Frame",
                      "Counter-Threatening Frame"),
         outcome = factor(outcome,
                          levels = c("National Resettlement", 
                                     "Local Resettlement"))) %>%
  select(term, estimate, std.error, Var, outcome) %>% 
  mutate(conf.low = estimate - 1.96 * std.error, #making high and low confidence intervals
         conf.high = estimate + 1.96 * std.error,
         group = "All")

# geom_pointrange is combo of geom_point and geom_linerange
fig1 <- ggplot(df1) +
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      x = Var,
                      color = group)) +
  facet_wrap(~ outcome) +
  coord_flip() +
  scale_y_continuous(limits = c(-1.5, 1.5),
                     breaks = c(-1.0, 0, 1.0)) +
  geom_hline(yintercept =  0, linetype = "dashed") +
  scale_color_manual(values = "black") +
  labs(color = NULL,
       x = NULL,
       y = " ") +
  theme_few()

fig1


## Analysis two:

subset0 <- df %>% filter(refugee_county_dum == 0)
subset1 <- df %>% filter(refugee_county_dum == 1)


# local only
f1a_0 <- lm(L ~ factor(Prime), subset0) %>% 
  tidy() %>% 
  mutate(data = "Local Resettlement",
         cov = "Low")

# National Only
f1b_0 <- lm(US ~ factor(Prime), subset0) %>% 
  tidy() %>% 
  mutate(data = "National Resettlement",
         cov = "Low")

# Local only 
f1a_1 <- lm(L ~ factor(Prime), subset1) %>% 
  tidy() %>% 
  mutate(data = "Local Resettlement",
         cov = "High")

# National only
f1b_1 <- lm(US ~ factor(Prime), subset1) %>% 
  tidy() %>% 
  mutate(data = "National Resettlement",
         cov = "High")


df2 <- bind_rows(f1a_0, f1b_0, f1a_1, f1b_1) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(Var = ifelse(term == "factor(Prime)1", "Threatening Frame", "Counter-Threatening Frame"),
         data = factor(data, levels = c("National Resettlement", "Local Resettlement"))) %>% 
  mutate(xvar = NA,
         xvar = ifelse(Var == "Counter-Threatening Frame" & cov == "Low", 0.75, xvar),
         xvar = ifelse(Var == "Counter-Threatening Frame" & cov == "High", 1.25, xvar),
         xvar = ifelse(Var == "Threatening Frame" & cov == "Low", 1.75, xvar),
         xvar = ifelse(Var == "Threatening Frame" & cov == "High", 2.25, xvar),
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)


fig2 <- ggplot(df2) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high,
                      x = xvar,
                      color = cov)) +
  facet_wrap(~ data) +
  coord_flip() + 
  scale_x_continuous(limits = c(0.5, 2.5),
                     breaks = c(1, 2),
                     labels = c("Counter-Threatening Frame",
                                "Threatening Frame")) +
  scale_y_continuous(limits = c(-1.5, 1.5),
                     breaks = c(-1.0, 0, 1.0)) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  labs(x = NULL,
       y = " ",
       color = "Refugee Density") +
  theme_few()
  


fig2


## Merge grids

plot_grid(fig1, fig2, ncol = 1, align = 'v')

ggplot2::ggsave("Session10/figures/FFH_figure.pdf", width = 6, height = 4)
