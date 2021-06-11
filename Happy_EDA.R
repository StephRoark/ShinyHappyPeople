library(readr)
library(tidyverse)
library(countrycode)
library(Hmisc)
library(skimr)
library(wbstats)
library(viridis)
library(gganimate)
library(gifski)
library(lubridate)
library(png)


happy_data_15 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2015.csv")
happy_data_16 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2016.csv")
happy_data_17 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2017.csv")
happy_data_18 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2018.csv")
happy_data_19 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2019.csv")
happy_data_20 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2020.csv")
happy_data_21 <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/2021.csv")

happy_data <- read_csv("/Users/stephanie/Projects/DSND/ShinyHappyPeople/data/world-happiness-report.csv")

# #discovered issue with inconsistent GDP data, found a better source
# happy_logged_data <- read_csv("C:/Users/wh057e/Desktop/Projects/ShinyHappyPeople/data/world-happiness-report-log.csv")
# happy_logged_data_21 <- read_csv("C:/Users/wh057e/Desktop/Projects/ShinyHappyPeople/data/world-happiness-report-2021-log.csv")

#happy_report_15 <- read_csv("C:/Users/wh057e/Desktop/Projects/ShinyHappyPeople/data/2015_report.csv")
#continents <- unique(happy_report_15$continent)

colnames(happy_data_15)
unique(happy_data_15$Region)
length(unique(happy_data_15$Country))
colnames(happy_data_16)
length(unique(happy_data_16$Country))
colnames(happy_data_17)
length(unique(happy_data_17$Country))
colnames(happy_data_18)
unique(happy_data_18$`Country or region`)
length(unique(happy_data_18$`Country or region`))
length(unique(happy_data_19$`Country or region`))
colnames(happy_data_19)
length(unique(happy_data_20$`Country name`))
colnames(happy_data_20)
length(unique(happy_data_21$`Country name`))
colnames(happy_data_21)
unique(happy21$Region)


common_countries <- Reduce(intersect, list(happy_data_15$Country, happy_data_16$Country, happy_data_17$Country, happy_data_18$`Country or region`, happy_data_19$`Country or region`, happy_data_20$`Country name`, happy_data_21$`Country name`))
       
get_continent_date <- function(tb, year) {
  df <- data.frame(tb)
  df$continent <- countrycode(sourcevar = df[, "Country"],
                              origin = "country.name",
                              destination = "continent")
  df$year <- year
  return(df)
}

happy15 <- happy_data_15 %>% 
  filter(Country %in% common_countries) %>%
  select(Country, `Happiness Score`, `Economy (GDP per Capita)`, `Health (Life Expectancy)`, Freedom, `Trust (Government Corruption)`, Generosity) 
names(happy15) <- c("Country", "Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy15df <- get_continent_date(happy15, "2015")

happy16 <- happy_data_16 %>% 
  filter(Country %in% common_countries) %>%
  select(Country, `Happiness Score`, `Economy (GDP per Capita)`, `Health (Life Expectancy)`, Freedom, `Trust (Government Corruption)`, Generosity) 
names(happy16) <- c("Country", "Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy16df <- get_continent_date(happy16, "2016")

happy17 <- happy_data_17 %>% 
  filter(Country %in% common_countries) %>%
  select(Country, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., Freedom, Trust..Government.Corruption., Generosity) 
names(happy17) <- c("Country", "Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy17df <- get_continent_date(happy17, "2017")

happy18 <- happy_data_18 %>% 
  filter(`Country or region` %in% common_countries) %>%
  select(`Country or region`, Score, `GDP per capita`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
names(happy18) <- c("Country", "Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy18df <- get_continent_date(happy18, "2018")
happy18df$Trust <- as.numeric(happy18df$Trust)

happy19 <- happy_data_19 %>% 
  filter(`Country or region` %in% common_countries) %>%
  select(`Country or region`, Score, `GDP per capita`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
names(happy19) <- c("Country", "Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy19df <- get_continent_date(happy19, "2019")

# happy20 <- happy_data_20 %>% 
#   filter(`Country name` %in% common_countries) %>%
#   select(`Country name`, `Ladder score`, `Logged GDP per capita`, `Explained by: Healthy life expectancy`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
happy20 <- happy_data_20 %>% 
  filter(`Country name` %in% common_countries) %>%
  select(`Country name`, `Ladder score`, `Explained by: Log GDP per capita`, `Explained by: Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
names(happy20) <- c("Country","Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
happy20df <- get_continent_date(happy20, "2020")

# happy21 <- happy_data_21 %>% 
#   filter(`Country name` %in% common_countries) %>%
#   select(`Country name`, `Ladder score`, `Logged GDP per capita`, `Explained by: Healthy life expectancy`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
happy21 <- happy_data_21 %>% 
  filter(`Country name` %in% common_countries) %>%
  select(`Country name`, `Ladder score`, `Explained by: Log GDP per capita`, `Explained by: Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity) 
names(happy21) <- c("Country","Score", "GDP", "LifeExpectancy",  "Freedom", "Trust", "Generosity")
happy21df <- get_continent_date(happy21, "2021")



# happy_logged_data_df <- happy_logged_data %>%
#   filter(`Country name` %in% common_countries) %>%
#   select(`Country name`, `Life Ladder`, `Logged GDP per capita`, `Healthy life expectancy at birth`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity)
# names(happy21) <- c("Country","Score", "GDP", "LifeExpectancy", "Freedom", "Trust", "Generosity")
# happy21df <- get_continent_date(happy21, "2021")


happy_data_all <- bind_rows(happy15df, happy16df, happy17df, happy18df, happy19df, happy20df, happy21df)

describe(happy_data_all)

skimr::skim(happy_data_all)
#noticed there are NAs and Kosovo didn't get assigned a continent
unique(happy_data_all$continent)
happy_data_noNA <- happy_data_all %>% mutate(continent = ifelse(is.na(continent) == TRUE, "Europe", continent))
#this doesnt' seem to work 
happy_data_final <- happy_data_noNA %>% mutate(continent = ifelse(Country == c("Canada", "United States", "Mexico"), "North America", continent))

missing_Trust <- happy_data_final %>% filter(is.na(Trust)==TRUE)


describe(happy_data_final)

skimr::skim(happy_data_final)

#plots - animate

a <- happy_data_final %>% 
  #filter(year == "2021") %>%
  ggplot2::ggplot(aes(x = GDP, 
                    y = LifeExpectancy,                          
                    size = Score,
                    color = continent, 
                    frame = year)) +  
  ggplot2::geom_point(alpha = 0.5) +  
  ggplot2::scale_size(range = c(.1, 20), guide = FALSE) +    
  ggplot2::scale_x_continuous(limits = c(0, 3)) +  
  ggplot2::scale_y_continuous(limits = c(0, 1)) + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Continent", option = "viridis") + 
  ggplot2::labs(x = "GDP per capita",                
                y = "Life expectancy Contribution") + 
  ggplot2::ggtitle("Year: {frame_time}") +
  ggplot2::theme_classic() +  
  ggplot2::geom_text(aes(x = 7.5, y = 60, label = year), 
                     size = 14, color = 'lightgrey', 
                     family = 'Oswald')+ 
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

  # animate
  animate(a, width = 450, height = 450)
  # save as a GIF
  anim_save("output.gif", animation = last_animation(), path = "C:/Users/wh057e/Desktop/Projects/ShinyHappyPeople")

  
  ###WORKS
#try 2
  
  b <- happy_data_final %>% 
    ggplot() +
    geom_point(aes(x = GDP, 
               y = LifeExpectancy,                          
              size = Score,
              color = continent), alpha = 0.65) +
              theme_classic() +
              theme(legend.position = "right") + 
              guides(size = "none") + 
              labs(x = "GDP per capita",                
                  y = "Life expectancy Contribution") +
              geom_text(aes(x = 1, y = 1, label = as.factor(year)), 
                  size = 14, color = 'lightgrey', 
                  family = 'Oswald') +
              scale_size(range = c(.1, 12.5), guide = FALSE) +  
              ggtitle("Life Expectancy Contibrution v GDP by Happiness Score and Region") +
              #ggtitle("Year: {frame_time}") +
              transition_time(as.factor(year)) +
              transition_states(as.factor(year), transition_length = 1, state_length = 0) +
              ease_aes("linear") +
              enter_fade() +
              exit_fade() 
              
    # scale_size(range = c(.1, 12.5), guide = FALSE) +    
    # scale_x_continuous(limits = c(0, 2.5)) +  
    # scale_y_continuous(limits = c(45, 90)) + 
    
  
  if (TRUE) {
    # Renderers are specified in the `animate()` function
    animate(b, width = 800, height = 800, renderer = gifski_renderer())
  }
  

###TowardsDataScience Example  
#WB

wb_data <- wbstats::wb(
  indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL"),                         
  country = "countries_only", 
  startdate = 1960, 
  enddate = 2021
) %>%
  dplyr::left_join(wbstats::wb_countries() %>%                         
                     dplyr::select(iso3c, region)) %>%
  tidyr::pivot_wider(
    id_cols = c("date", "country", "region"), 
    names_from = indicator, 
    values_from = value
  )

unique(wb_data$country)
common_countries_w_WB <- Reduce(intersect, list(common_countries, wb_data$country))

wb_data_df <- data.frame(wb_data)
wb_data_df$continent <- countrycode(sourcevar = wb_data_df[, "country"],
                            origin = "country.name",
                            destination = "continent")

wbplot <- wb_data %>% ggplot2::ggplot(aes(x = log(`GDP per capita (current US$)`), 
                    y = `Life expectancy at birth, total (years)`,                          
                    size = `Population, total`,
                    color = region)) +  
  ggplot2::geom_point(alpha = 0.5) +  
  ggplot2::scale_size(range = c(.1, 16), guide = FALSE) +    
  ggplot2::scale_x_continuous(limits = c(2.5, 12.5)) +  
  ggplot2::scale_y_continuous(limits = c(30, 90)) + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  ggplot2::labs(x = "Log GDP per capita",                
                y = "Life expectancy at birth") + 
  ggplot2::theme_classic() +  
  ggplot2::geom_text(aes(x = 7.5, y = 60, label = date), 
                     size = 14, color = 'lightgrey', 
                     family = 'Oswald') +
  gganimate::transition_states(date, 
                             transition_length = 1, 
                             state_length = 1) +   
  gganimate::ease_aes('cubic-in-out')


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(wbplot, width = 800, height = 800, renderer = gifski_renderer())
}



##

wb_short <- wb_data_df %>%
    filter(country %in% common_countries_w_WB)

wb_less_short <- wb_short %>%
  filter(date >= "2015" ) %>%
  rename(Country = country) 

happy_short <- happy_data_final %>%
  filter(Country %in% common_countries_w_WB ) %>%
  filter(year <= "2018") %>%
  rename(date = year)

happy_wb_lifexpect <- left_join(happy_short, wb_less_short, by = c("Country", "date"))
happywb <- happy_wb_lifexpect %>% 
  select(Country, Score, GDP, LifeExpectancy, Life.expectancy.at.birth..total..years., Freedom, Trust, Generosity, continent.x, date, region, Population..total) %>%
  rename(LifeExpectancyYears = Life.expectancy.at.birth..total..years., continent = continent.x, Population = Population..total)

#DOESNOTWORK
###
happywbplot <- happywb %>% ggplot2::ggplot(aes(x = GDP, 
                                          y = LifeExpectancyYears,                          
                                          size = Population,
                                          color = continent)) +  
  ggplot2::geom_point(alpha = 0.5) +  
  ggplot2::scale_size(range = c(.1, 16), guide = FALSE) +    
  ggplot2::scale_x_continuous(limits = c(2.5, 12.5)) +  
  ggplot2::scale_y_continuous(limits = c(30, 90)) + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  ggplot2::labs(x = "Log GDP per capita",                
                y = "Life expectancy at birth") + 
  ggplot2::theme_classic() +  
  ggplot2::geom_text(aes(x = 7.5, y = 60, label = date), 
                     size = 14, color = 'lightgrey', 
                     family = 'Oswald') +
  gganimate::transition_states(date, 
                               transition_length = 1, 
                               state_length = 1) +   
  gganimate::ease_aes('cubic-in-out')

if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywbplot, renderer = gifski_renderer())
}



###WORKS
#bubbles by population
happywb_pop_plot <- happywb %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = LifeExpectancyYears,                          
                 size = Population,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 20), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(40, 90)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Life expectancy Contribution") +
  geom_text(aes(x = 1, y = 45, label = date), 
            size = 10, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Life Expectancy in Years v GDP by Population and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(date)) +
  transition_states(as.factor(date), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_pop_plot, width = 800, height = 800, renderer = gifski_renderer())
}

###WORKS
#alt
happywb_popalt_plot <- happywb %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = LifeExpectancyYears,                          
                 size = Population,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 20), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(45, 90)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Life expectancy in Years") +
  geom_text(aes(x = 1, y = 45, label = date), 
            size = 10, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Life Expectancy Contibrution v GDP by Population and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(date)) +
  transition_states(as.factor(date), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_popalt_plot, width = 800, height = 800, renderer = gifski_renderer())
}

#bubbles by happiness score
happywb_score_plot <- happywb %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = LifeExpectancyYears,                          
                 size = Score,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 12.5), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(45, 90)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Life expectancy Contribution") +
  geom_text(aes(x = 1, y = 45, label = date), 
            size = 15, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Life Expectancy in Years v GDP by Happiness Score and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(date)) +
  transition_states(as.factor(date), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_score_plot, width = 800, height = 800, renderer = gifski_renderer())
}

#bubbles by happiness score - Generosity
happywb_score_gen_plot <- happywb %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = Generosity,                          
                 size = Score,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 10), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Generosity") +
  geom_text(aes(x = 1, y = 1, label = date), 
            size = 15, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Generosity v GDP by Happiness Score and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(date)) +
  transition_states(as.factor(date), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_score_gen_plot, width = 800, height = 800, renderer = gifski_renderer())
}

happy_data_final

#bubbles by happiness score - Generosity
happywb_score_plot_genalt <- happy_data_final %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = Generosity,                          
                 size = Score,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 10), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Generosity") +
  geom_text(aes(x = 1, y = 1, label = year), 
            size = 15, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Alternate Generosity v GDP by Happiness Score and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(year)) +
  transition_states(as.factor(year), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_score_plot_genalt, width = 800, height = 800, renderer = gifski_renderer())
}

#bubbles by happiness score - Trust
happywb_score_trust_plot <- happy_data_final %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = Trust,                          
                 size = Score,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 10), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Trust") +
  geom_text(aes(x = 1, y = 1, label = year), 
            size = 15, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Trust v GDP by Happiness Score and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(year)) +
  transition_states(as.factor(year), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_score_trust_plot, width = 800, height = 800, renderer = gifski_renderer())
}

#bubbles by happiness score - Trust
happywb_score_free_plot <- happy_data_final %>% 
  ggplot() +
  geom_point(aes(x = GDP, 
                 y = Freedom,                          
                 size = Score,
                 color = continent), alpha = 0.65) +
  scale_size(range = c(.1, 10), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2.5)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  theme_classic() +
  theme(legend.position = "right") + 
  viridis::scale_color_viridis(
    discrete = TRUE, name = "Region", option = "viridis") + 
  guides(size = "none") + 
  labs(x = "GDP per capita",                
       y = "Freedom") +
  geom_text(aes(x = 1, y = 1, label = year), 
            size = 15, color = 'lightgrey', 
            family = 'Oswald') +
  ggtitle("Freedom v GDP by Happiness Score and Continent") +
  #ggtitle("Year: {frame_time}") +
  transition_time(as.factor(year)) +
  transition_states(as.factor(year), transition_length = 1, state_length = 0) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()


if (TRUE) {
  # Renderers are specified in the `animate()` function
  animate(happywb_score_free_plot, width = 800, height = 800, renderer = gifski_renderer())
}
