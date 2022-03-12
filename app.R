library(dash)
library(dashCoreComponents)
library(tidyverse)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# load data
data <- read_csv('data/survey.csv')

# Data wrangling
data_employee_gender <- data %>%
  mutate(Gender = tolower(Gender)) %>%
  filter(Gender %in% c("male", "female")) %>%
  group_by(no_employees, Gender) %>%
  count()

data_employee_gender$pct <- data_employee_gender$n/nrow(data)*100

create_gender_list <- function() {
  gender_list <- list()
  for (gender in c("All", "male", "female")) {
    print(gender)
    gender_list <- append(gender_list,
                             list(list("label" = gender,
                                      "value" = gender)))
  }
  return(gender_list)
}


app$callback(
  output('employee-plot', 'figure'),
  list(input('gender-selector', 'value')),
  function(selected_gender="All") {
    if (selected_gender == 'All'){
    plot_data <- data_employee_gender %>%
    group_by(no_employees) %>%
      summarise(pct = sum(pct)) %>%
      mutate(Gender = 'All')
    }
    else if (selected_gender == "male") {
      plot_data <- data_employee_gender %>%
        filter (Gender == 'male')
    }
    else {
      plot_data <- data_employee_gender %>%
        filter (Gender == 'female')
    }

    plot <- ggplot(plot_data) +
    aes(x=pct, y=reorder(no_employees, pct), fill = Gender) +
    labs(y='Company size', x="percentage of respondents") +
    geom_bar(stat = 'identity') 
  
    ggplotly(plot)
})

app %>% set_layout(
  h1('Mental Health Dashboard'),
  list(
    dccDropdown(
      id = "gender-selector",
      options = create_gender_list(),
      value = 'All'
    )
  ),
  #dbcCard(
    div(
      list(
      dccGraph(id="employee-plot")
      )
  # )
  )
)

# Run the app
app$run_server(host = '0.0.0.0')
#app$run_server(debug = T)
