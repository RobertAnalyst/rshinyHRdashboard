library(shiny)
library(shinydashboard)
library(tidyverse)

# Sample data
employee_data <- tibble(
  department = rep(c("Finance", "IT", "HR", "MLE"), each = 50),
  tenure = runif(200, 1, 10),
  satisfaction_score = runif(200, 3, 5),
  status = sample(c("Active", "Resigned"), 200, replace = TRUE, prob = c(0.8, 0.2))
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "HR Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Details", tabName = "details", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("employeeCount"),
                valueBoxOutput("turnoverRate"),
                valueBoxOutput("avgTenure"),
                valueBoxOutput("avgSatisfaction")
              ),
              fluidRow(
                box(title = "Employee Status by Department", width = 12, plotOutput("statusPlot"))
              )
      ),
      tabItem(tabName = "details",
              fluidRow(
                box(title = "Satisfaction Scores by Department", width = 6, plotOutput("satisfactionPlot")),
                box(title = "Average Tenure by Department", width = 6, plotOutput("avgTenurePlot"))
              ),
              fluidRow(
                box(title = "Distribution of Satisfaction Scores", width = 12, plotOutput("satisfactionDistPlot"))
              )
      )
    ),
    # Footer with developer name
    fluidRow(
      column(width = 12, align = "center",
             tags$footer(tags$i("Developed by Omondi Robert"), style = "margin-top: 20px; font-size: 14px;")
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$employeeCount <- renderValueBox({
    valueBox(
      value = nrow(employee_data),
      subtitle = "Total Employees",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$turnoverRate <- renderValueBox({
    turnover_rate <- employee_data %>% 
      filter(status == "Resigned") %>% 
      nrow() / nrow(employee_data) * 100
    valueBox(
      value = sprintf("%.1f%%", turnover_rate),
      subtitle = "Turnover Rate",
      icon = icon("line-chart"),
      color = "red"
    )
  })
  
  output$avgTenure <- renderValueBox({
    avg_tenure <- mean(employee_data$tenure)
    valueBox(
      value = sprintf("%.1f years", avg_tenure),
      subtitle = "Average Tenure",
      icon = icon("clock-o"),
      color = "green"
    )
  })
  
  output$avgSatisfaction <- renderValueBox({
    avg_satisfaction <- mean(employee_data$satisfaction_score)
    valueBox(
      value = sprintf("%.1f", avg_satisfaction),
      subtitle = "Average Satisfaction Score",
      icon = icon("smile-o"),
      color = "yellow"
    )
  })
  
  output$statusPlot <- renderPlot({
    employee_data %>% 
      count(department, status) %>% 
      ggplot(aes(x = department, y = n, fill = status)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Employee Status by Department", y = "Count", x = "Department") +
      theme_minimal()
  })
  
  output$satisfactionPlot <- renderPlot({
    employee_data %>% 
      ggplot(aes(x = department, y = satisfaction_score)) +
      geom_boxplot() +
      labs(title = "Satisfaction Scores by Department", y = "Satisfaction Score", x = "Department") +
      theme_minimal()
  })
  
  output$avgTenurePlot <- renderPlot({
    employee_data %>%
      group_by(department) %>%
      summarize(avg_tenure = mean(tenure)) %>%
      ggplot(aes(x = department, y = avg_tenure, fill = department)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Tenure by Department", y = "Average Tenure (years)", x = "Department") +
      theme_minimal()
  })
  
  output$satisfactionDistPlot <- renderPlot({
    employee_data %>%
      ggplot(aes(x = satisfaction_score)) +
      geom_histogram(bins = 30, fill = "blue", color = "white") +
      labs(title = "Distribution of Satisfaction Scores", x = "Satisfaction Score", y = "Count") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
