# data is from http://www.bea.gov/regional/downloadzip.cfm

# packages used
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


pce <- read_csv("state per capita expenditures.csv")
pci <- read_csv("state per capita income.csv")

######################
# U.S. level savings #
######################
# create tidy national-level expenditures
pce_us <- pce %>%
  filter(Description == "Personal consumption expenditures",
         GeoName == "United States") %>%
  gather(Year, Expenditures, `1997`:`2014`) %>%
  select(Year, Expenditures)

# create tidy state-level incomes
pci_us <- pci %>%
  filter(Description == "Per capita personal income (dollars) 2/",
         GeoName == "United States") %>%
  gather(Year, Income, `1997`:`2015`) %>%
  select(Year, Income)

# join income and expenditure data and calculate savings
savings_us <- pce_us %>% 
  left_join(pci_us) %>%
  mutate(Savings = Income - Expenditures,
         Savings_Rate = Savings / Income)

# visualize savings rate from 1997-2014
ggplot(savings_us, aes(Year, Savings_Rate, group = 1)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, .23))


#######################
# state level savings #
#######################
# create tidy state-level expenditures
pce_state <- pce %>%
  slice(25:1248) %>%
  filter(Description == "Personal consumption expenditures") %>%
  gather(Year, Expenditures, `1997`:`2014`) %>%
  select(GeoName, Year, Expenditures)

# create tidy state-level incomes
pci_state <- pci %>%
  slice(4:156) %>%
  filter(Description == "Per capita personal income (dollars) 2/") %>%
  gather(Year, Income, `1997`:`2014`) %>%
  select(GeoName, Year, Income)

# create tidy state-level savings
savings_state <- pce_state %>%
  left_join(pci_state) %>%
  mutate(Savings = Income - Expenditures,
         Savings_Rate = Savings / Income, 
         Year = as.numeric(Year)) %>%
  filter(GeoName != "District of Columbia")

# top 10 savers in 2014
savings_state %>% 
  filter(Year == 2014) %>% 
  arrange(desc(Savings_Rate))

# bottom 10 savers in 2014
savings_state %>% 
  filter(Year == 2014) %>% 
  arrange(Savings_Rate)

# top 10 average savers (1997-2014)
savings_state %>%
  group_by(GeoName) %>%
  summarise(Avg_Rate = mean(Savings_Rate)) %>%
  arrange(desc(Avg_Rate))

# bottom 10 average savers (1997-2014)
savings_state %>%
  group_by(GeoName) %>%
  summarise(Avg_Rate = mean(Savings_Rate)) %>%
  arrange(Avg_Rate)

# color by positive vs negative trend
savings_state <- savings_state %>% 
  group_by(GeoName) %>% 
  slice(c(1, 18)) %>% 
  select(GeoName, Year, Savings_Rate) %>% 
  spread(Year, Savings_Rate) %>% 
  mutate(Change = ifelse(`1997` < `2014`, "Positive", "Negative")) %>% 
  select(GeoName, Change) %>%
  right_join(savings_state)

ggplot(savings_state, aes(Year, Savings_Rate)) +
  geom_line(aes(group = GeoName, color = Change)) +
  facet_wrap(~ GeoName, ncol = 5) +
  scale_x_continuous(breaks = c(1997, 2014)) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, .3)) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() 

# need to figure out how to add background lines to each facet


# rank-order by savings rate growth since 1997
savings_state %>% 
  group_by(GeoName) %>% 
  slice(c(1, 18)) %>% 
  select(GeoName, Year, Savings_Rate) %>% 
  spread(Year, Savings_Rate) %>% 
  mutate(Diff = `2014` - `1997`, 
         Change = ifelse(`1997` < `2014`, "Positive", "Negative")) %>% 
  arrange(Diff)


# t-test for differences in savings rate for positive vs. negative trend states
t.test(Savings_Rate ~ Change, data = (savings_state))
wilcox.test(Savings_Rate ~ Change, data = (savings_state))


# so the next question is, what's driving the difference between the high and 
# low savers? We can first check if there is a difference between the incomes and
# consumption of the increasing states (15 states) versus decreasing states (35)

# create percent 
pce_us <- pce %>%
  slice(1) %>%
  gather(Year, US_Expenditures, `1997`:`2014`) %>%
  select(Year, US_Expenditures) %>%
  mutate(Year = as.numeric(Year))

pci_us <- pci %>%
  slice(3) %>%
  gather(Year, US_Income, `1997`:`2014`) %>%
  select(Year, US_Income) %>%
  mutate(Year = as.numeric(Year))

pcts <- savings_state %>%
  left_join(pce_us, by = "Year") %>%
  left_join(pci_us, by = "Year") %>%
  mutate(Exp_Pct = Expenditures / US_Expenditures,
         Inc_Pct = Income / US_Income) %>%
  select(-c(Expenditures:Savings_Rate))

# t-test shows their is a difference between expenses but not incomes
t.test(Inc_Pct ~ Change, data = pcts)

ggplot(pcts, aes(Change, Exp_Pct)) +
  geom_boxplot() +
  geom_jitter()

