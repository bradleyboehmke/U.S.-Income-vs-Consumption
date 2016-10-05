library(rvest)        # scraping data
library(tidyr)        # creating tidy data
library(dplyr)        # transforming (joining, summarizing, etc.) data
library(tibble)       # coercing data to tibbles
library(magrittr)     # for piping capabilities
library(DT)           # for printing nice HTML output tables
library(ggplot2)      # visualizing data
library(ggrepel)      # Repel overlapping text labels in plots


#####################
# download PCI data #
#####################
# url for PCI HTML table
url_pci <- read_html("http://www.bea.gov/iTable/iTableHtml.cfm?reqid=70&step=30&isuri=1&7022=21&7023=0&7024=non-industry&7033=-1&7025=0&7026=00000,01000,02000,04000,05000,06000,08000,09000,10000,11000,12000,13000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,40000,41000,42000,44000,45000,46000,47000,48000,49000,50000,51000,53000,54000,55000,56000&7027=-1&7001=421&7028=53&7031=0&7040=-1&7083=levels&7029=23&7090=70")

# download PCI table and extract the data frame from the list
pci_raw <- url_pci %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE) %>%
  .[[1]]

#####################
# download PCE data #
#####################
# url for PCE HTML table
url_pce <- read_html("http://www.bea.gov/iTable/iTableHtml.cfm?reqid=70&step=10&isuri=1&7003=2&7035=-1&7004=x&7005=1&7006=00000,01000,02000,04000,05000,06000,08000,09000,10000,11000,12000,13000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,40000,41000,42000,44000,45000,46000,47000,48000,49000,50000,51000,53000,54000,55000,56000&7036=-1&7001=62&7002=6&7090=70&7007=-1&7093=levels")

# download PCE table and extract the data frame from the list
pce_raw <- url_pce %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE) %>%
  .[[1]]

####################
# create tidy data #
####################
# create tidy PCI data
pci_clean <- pci_raw %>% 
  apply(2, function(x) gsub("[[:punct:]]", "", x)) %>%
  as_tibble(.) %>%
  group_by(GeoFips, GeoName) %>%
  mutate_each(funs(as.numeric)) %>%
  ungroup() %>%
  select(Fips = GeoFips, Location = GeoName, `1997`:`2014`) %>%
  gather(Year, Income, -c(Fips, Location))


# create tidy PCE data 
pce_clean <- pce_raw %>% 
  apply(2, function(x) gsub("[[:punct:]]", "", x)) %>%
  as_tibble(.) %>%
  group_by(Fips, Area) %>%
  mutate_each(funs(as.numeric)) %>%
  ungroup() %>%
  rename(Location = Area) %>%
  gather(Year, Expenditures, -c(Fips, Location))

# create tidy merged data frame
data_clean <- pci_clean %>%
  left_join(pce_clean) %>%
  mutate(Savings = Income - Expenditures,
         Year = as.numeric(Year)) %>%
  filter(Location != "District of Columbia")

rm(pce_clean, pce_raw, pci_clean, pci_raw, url_pce, url_pci)


###########################
# National Level Patterns #
###########################
# visualize difference between PCI & PCE
data_clean %>%
  filter(Location == "United States") %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Income, group = 1), color = "darkseagreen4") +
  geom_line(aes(y = Expenditures, group = 1), color = "firebrick3") +
  geom_ribbon(aes(ymin = Expenditures, ymax = Income, group = 1), fill = "darkseagreen1", alpha = .5) +
  annotate("text", x = 2014.25, y = 40471, label = "2014 PCI: $40.5K", color = "darkseagreen4", hjust = 0) +
  annotate("text", x = 2014.25, y = 37196, label = "2014 PCE: $37.2K", color = "firebrick3", hjust = 0) +
  annotate("text", x = 2014.25, y = 38833.5, label = "2014 Savings: $3.3K", color = "darkseagreen2", hjust = 0) +
  scale_x_continuous(NULL, limits = c(1997, 2016), breaks = seq(1998, 2014, by = 4)) +
  scale_y_continuous("Current Year Dollars", labels = scales::dollar) +
  theme_minimal()

# visualize savings rate from 1997-2014
data_clean %>%
  filter(Location == "United States") %>%
  mutate(Savings_Rate = Savings / Income) %>%
  ggplot(aes(Year, Savings_Rate)) +
  geom_line() +
  geom_hline(aes(yintercept = mean(Savings_Rate)), linetype = "dashed", alpha = .5) +
  scale_y_continuous(NULL, labels = scales::percent, limits = c(0, .115)) +
  ggtitle("National-level Savings Rate") +
  theme_minimal()

# any interesting year-over-year change patterns
data_clean %>%
  filter(Location == "United States") %>%
  mutate(PCI_YoY = (Income - Income[1]) / Income[1],
         PCE_YoY = (Expenditures - Expenditures[1]) / Expenditures[1],
         Sav_YoY = (Savings - Savings[1]) / Savings[1]) %>%
  select(Year, contains("YoY")) %>%
  gather(Variable, Change, - Year) %>%
  ggplot(aes(Year, Change, color = Variable)) +
  geom_line()


########################
# State Level Patterns #
########################
# map of 2014 savings by state
data_clean %>%
  mutate(region = tolower(Location),
         Savings_Rate = Savings / Income) %>%
  right_join(map_data("state")) %>% 
  select(-subregion) %>% 
  filter(Year %in% seq(1998, 2014, by = 2)) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Savings_Rate)) +
  facet_wrap(~ Year, ncol = 3) +
  scale_fill_gradient2(name="Savings Rate", labels = scales::percent) +
  ggtitle("Savings rate changes over time",
       subtitle = "Temporal assessment of state-level savings rates (1998-2014)") +
  expand_limits() +
  theme_void() +
  theme(strip.text.x = element_text(size = 12),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))

# Top 10 states
savings_rate <- data_clean %>%
  mutate(Savings_Rate = Savings / Income) %>%
  filter(Year == 2014, Location != "United States") %>%
  arrange(desc(Savings_Rate)) %>%
  slice(1:10)

savings_rate <- data_clean %>%
  mutate(Savings_Rate = Savings / Income) %>%
  filter(Location != "United States")

top5 <- savings_rate %>%
  arrange(desc(Savings_Rate)) %>%
  filter(Year == 2014) %>%
  slice(1:5)

bottom5 <- savings_rate %>%
  arrange(Savings_Rate) %>%
  filter(Year == 2014) %>%
  slice(1:5)

avg <- savings_rate %>%
  group_by(Year) %>%
  summarise(Avg_mn = mean(Savings_Rate),
            Avg_md = median(Savings_Rate)) %>%
  mutate(Avg = "Average")

ggplot(savings_rate, aes(Year, Savings_Rate, group = Location)) +
  geom_line(alpha = .1) +
  geom_line(data = filter(savings_rate, Location %in% top5$Location),
            aes(Year, Savings_Rate, group = Location), color = "dodgerblue") +
  geom_line(data = filter(savings_rate, Location %in% bottom5$Location),
            aes(Year, Savings_Rate, group = Location), color = "red") +
  geom_line(data = avg, aes(Year, Avg_mn, group = 1), linetype = "dashed") +
  annotate("text", x = 2014.25, y = .071, label = "Average", hjust = 0, size = 3) +
  geom_text_repel(data = top5, aes(label = Location), nudge_x = .5, size = 3) +
  geom_point(data = top5, aes(Year, Savings_Rate), color = "dodgerblue") +
  geom_text_repel(data = bottom5, aes(label = Location), nudge_x = 0.5, size = 3) +
  geom_point(data = bottom5, aes(Year, Savings_Rate), color = "red") +
  scale_x_continuous(NULL, limits = c(1997, 2015.25), breaks = seq(1998, 2014, by = 2)) +
  scale_y_continuous(NULL, labels = scales::percent) +
  ggtitle("Savings rate changes over time",
          subtitle = "Temporal assessment of state-level savings rates (1997-2014)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))


# States with largest changes
savings_rate %>%
  filter(Year == 1997 | Year == 2014) %>%
  select(Location, Year, Savings_Rate) %>%
  spread(Year, Savings_Rate) %>%
  mutate(Change = `2014` - `1997`) %>%
  arrange(desc(abs(Change)))

# is there a common theme for what drives
changes <- data_clean %>%
  filter(Location != "United States", Year == 1997 | Year == 2014) %>%
  arrange(Location) %>%
  select(Location, Year, Income, Expenditures) %>%
  group_by(Location) %>%
  mutate(Inc_Chg = diff(Income) / lag(Income),
         Exp_Chg = diff(Expenditures) / lag(Expenditures),
         ratio = Inc_Chg / Exp_Chg) %>%
  na.omit() %>%
  arrange(desc(ratio))

# difference in PCE & PCI growth rates for above vs below avg saving rate states
changes <- savings_rate %>%
  filter(Year == 1997 | Year == 2014) %>%
  arrange(Location) %>%
  select(Location, Year, Income, Expenditures, Savings_Rate) %>%
  group_by(Location) %>%
  mutate(PCI = diff(Income) / lag(Income),
         PCE = diff(Expenditures) / lag(Expenditures)) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(Group = ifelse(Savings_Rate > mean(Savings_Rate), "Above Average", "Below Average")) %>%
  gather(Metric, Value, PCI:PCE)


ggplot(changes, aes(Metric, Value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = .1, alpha = .5) +
  geom_text(data = filter(changes, Value > 1.1 | Value < .6), aes(label = Location), size = 3, hjust = 0) +
  facet_wrap(~ Group) +
  scale_y_continuous("Percent change from 1997 to 2014", labels = scales::percent) +
  xlab(NULL) +
  ggtitle("Figure 5: Percent change in PCE & PCI",
          subtitle = "Comparing the change in PCE & PCI from 1997 to 2014 for those states with above versus below average savings rates") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Georgia", size = 12), 
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size = 28, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)))
  



