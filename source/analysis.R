library(tidyverse)
library(scales)
library(ggplot2)

## Section 2  ---- 
#----------------------------------------------------------------------------#
getOption('timeout')
options(timeout = 500)
incarcerated_trend <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

summary(incarcerated_trend$black_jail_pop)

max_black_pop_year <- incarcerated_trend %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(year)

max_black_rate_state <- incarcerated_trend %>%
  filter(black_jail_pop_rate== max(black_jail_pop_rate, na.rm = TRUE)) %>%
  pull(state)

max_white_rate_state <- incarcerated_trend %>%
  filter(white_jail_pop_rate== max(white_jail_pop_rate, na.rm = TRUE)) %>%
  pull(state)

black_max_state <- incarcerated_trend %>%
  filter(black_jail_pop== max(black_jail_pop, na.rm = TRUE)) %>%
  pull(state)

max_state_pop <- incarcerated_trend %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)

max_pop <- incarcerated_trend %>%
  filter(total_pop == max(total_pop, na.rm = TRUE)) %>%
  pull(state)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function creates a data frame with selected columns including the years
# and the total jail population. This data frame is arranged by years (which
# range from 1970 to 2018) and has N/A values removed.
get_year_jail_pop <- function() {
  year_and_total_jail <- incarcerated_trend %>%
    arrange(year) %>%
    select(year, total_jail_pop) %>%
    filter(!is.na(total_jail_pop))
    return(year_and_total_jail)   
}

# This function creates a bar graph of the increase in jail population in the 
# U.S. from 1970 to 2018 using the get_year_jail_pop function from above.
plot_jail_pop_for_us <- function()  {
  plot_jail <- ggplot(data = get_year_jail_pop(), aes(y = total_jail_pop, x = year)) +
    geom_bar(stat = 'identity') +
    labs(title = "Increase of Jail Population in U.S. (1970-2018).", caption = 
    "Figure 1. Increase of Jail Population in U.S. (1970-2018). 
         This chart shows the growth in total jail population in the U.S. from the year 1970 to 2018.
         Data source: Vera Institute of Justice" ) + 
    xlab("Year") + ylab("Total Jail Population") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
  return(plot_jail)  
}


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# This function creates a data frame with selected columns including total jail
# population, years, and states. You can select a state and a the rows for that
# state will be returned with the year and total jail population.

get_jail_pop_by_states <- function(states) {
  df_of_year_state_pop <- incarcerated_trend %>%
    filter(states == state) %>%
    select(year, state, total_jail_pop) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
              .groups = "drop")  
  return(df_of_year_state_pop)
}

#This function actually plots the function mentioned above
plot_jail_pop_by_states <- function(states) {
  plot_state_jail <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, group = state, color = state)) +
    geom_line() +
    labs(title = "Total Jail Population by State", caption = "Figure 2. Jail Population by State. 
         This chart shows the growth in total jail population for select states from the year 1970 to 2018.
         Data source: Vera Institute of Justice" ) +
    xlab("Year") + ylab("Total Jail Population")
  return(plot_state_jail)
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
#This function creates a data frame that shows the the total jail population for each
#urbanicity. Urbanicity is divided into 4 groups: rural, small/mid, suburban, and urban.
get_urbanicity_jail_pop <- function() {
  select_df <- incarcerated_trend %>%
    select(total_jail_pop, urbanicity) %>%
    group_by(urbanicity) %>%
    subset(total_jail_pop != 0) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
              .groups = "drop") 
  return(select_df)   
}
#This function creates a bar graph that plots the above data frame.
bar_graph_by_urbanciity <- function() {
  plot_urbanicity <- ggplot(data = get_urbanicity_jail_pop(), aes(y = total_jail_pop, x = urbanicity)) +
    geom_bar(stat = 'identity') +
    labs(title = "Increase In Jail Population Based On Urbanicity .", caption = 
           "Figure 3. Jail population totals grouped by urbanicity. 
         This chart shows the total jail population in the U.S. based on the urbanicity of the area.
         Data source: Vera Institute of Justice" ) + 
    xlab("Urbanicity") + ylab("Total Jail Population") +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
  return(plot_urbanicity)
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
#This function builds a data frame that is a list of all the total jail populations
#in each state in the year 2018.
jail_pop_per_state <- function() {
  df_of_state <- incarcerated_trend %>%
    filter(year == 2018) %>%
    select(state, total_jail_pop) %>%
    group_by(state) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
              .groups = "drop")
  return(df_of_state)
}
#This function creates a map for the above data frame.
jail_pop_map <- function() {
  state_shape <- map_data("state") %>% 
    rename(state = region) %>% 
    left_join(jail_pop_per_state(), by="state")
  map <- ggplot(state_shape) + 
    geom_polygon(mapping = aes(x = long, y = lat, group = group),color = "white", size = .1) + 
    coord_map() +
    labs(title = "Total Jail Population By State In 2018.", caption = 
           "Figure 4. Jail population totals grouped by state in the year 2018. 
         This chart shows the total jail population in the U.S. per state in the year 2018.
         Data source: Vera Institute of Justice" )
  return(map)
}

#----------------------------------------------------------------------------#



