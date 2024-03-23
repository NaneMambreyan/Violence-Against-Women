library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(networkD3)
library(GGally)
library(gridExtra)
library(readxl)

data <- read_excel("Desktop/GBV database, incl SDG indicators, updated.xlsx")

data_processed <- data %>%
  filter(!is.na(Q901), !is.na(Q708a), Q1008 %in% c(1, 2)) %>%
  mutate(
    Q901 = substr(Q901, 1, 1),
    Q901 = factor(Q901, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "X"),
                  labels = c("No specific reason", "Husband was drunk", "Financial crisis", "Husband's workplace issues",
                             "Unemployed husband", "No food at home", "Problems with each other's families", "Wife is pregnant",
                             "Husband is jealous", "Wife declines intercourse", "Wife does not obey", "Husband wants to teach a lesson",
                             "Husband wants to show dominance", "Wife can't get pregnant", "Other")),
    Q708a = factor(Q708a, levels = c(1, 2, 3, 4, 7, 8, 9),
                   labels = c("Never", "Sometimes", "Often", "Almost Always", "In Past", "IDK", "No Response")),
    DomesticViolenceType = case_when(
      Q705Aa == 1 ~ "Slapped",
      Q705Ab == 1 ~ "Beat/Pulled Hair",
      Q705Ac == 1 ~ "Beaten with Item",
      Q705Ad == 1 ~ "Hit by Foot",
      Q705Ae == 1 ~ "Intentionally Hurt",
      Q705Af == 1 ~ "Frightened/Used Weapon",
      TRUE ~ "No Violence"
    ),
    AlcoholUse = factor(Q509, levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                        labels = c("Everyday", "Once or twice a week", "1-3 times a month", 
                                   "Less than once a month", "Haven't drunk already a year", "Never", "IDK", "No Response")),
  ) %>%
  filter(!Q901 %in% c("Wife is pregnant", "Wife declines intercourse"))




sankey_data <- data %>%
  filter(Q509 %in% 1:6) %>%
  select(Q509, Q705Aa, Q705Ab, Q705Ac, Q705Ad, Q705Ae, Q705Af) %>%
  gather(key = "abuse_type", value = "abuse_reported", Q705Aa, Q705Ab, Q705Ac, Q705Ad, Q705Ae, Q705Af) %>%
  mutate(abuse_type = recode(abuse_type,
                             "Q705Aa" = "Slapped",
                             "Q705Ab" = "Beat/Pulled Hair",
                             "Q705Ac" = "Beaten with Item",
                             "Q705Ad" = "Hit by Foot",
                             "Q705Ae" = "Intentionally Hurt",
                             "Q705Af" = "Frightened/Used Weapon"),
         Q509 = factor(Q509, levels = 1:6, labels = c("Everyday", "Once or twice a week", "1-3 times a month", "Less than once a month", "Haven't drunk already a year", "Never"))) %>%
  filter(abuse_reported == 1) %>%
  group_by(Q509, abuse_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Nodes and links for the Sankey plot
nodes <- data.frame(name = c(as.character(sankey_data$Q509), as.character(unique(sankey_data$abuse_type))))
nodes <- unique(nodes)
links <- sankey_data %>%
  mutate(source = match(Q509, nodes$name) - 1,
         target = match(abuse_type, nodes$name) - 1) %>%
  select(source, target, count)

library(shiny)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Abuse Factors Analysis Dashboard"),
  dashboardSidebar(
    sidebarPanel(
      pickerInput(
        inputId = "selectedFactors",
        label = "Select Factors:",
        choices = setdiff(levels(data_processed$Q901), 
                          c("Wife is pregnant", "Wife declines intercourse")),
        options = list(`actions-box` = TRUE), 
        multiple = TRUE, 
        selected = levels(data_processed$Q901)[1]
      ),
      hr()
    )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Stacked Bar Chart", plotOutput("stackedBarChart")),
      tabPanel("Regular Bar Chart", plotOutput("regularBarChart")),
      tabPanel("Heatmap", plotOutput("heatmapPlot")),
      tabPanel("Sankey Diagram",
               box(title = "Patterns of Reported Abuse by Drinking Frequency",
                   sankeyNetworkOutput("sankeyDiagram", height = "500px"),
                   width = 12)
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive expression for preparing the data for bar charts and heatmap
  reactive_data <- reactive({
    req(input$selectedFactors)  
    data_processed %>%
      filter(Q901 %in% input$selectedFactors)
  })
  
  
  output$sankeyDiagram <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", 
                  Target = "target", Value = "count", NodeID = "name",
                  units = "Reports", fontSize = 12, nodeWidth = 30)
  })
  
  
  
  # Render Stacked Bar Chart
  output$stackedBarChart <- renderPlot({
    data_to_plot <- reactive_data()
    if (nrow(data_to_plot) > 0) {
      ggplot(data_to_plot, aes(x = Q901, fill = Q708a)) +
        geom_bar(position = "fill") +
        labs(title = "Proportion of Fear Factor by Abuse Reasons", x = "Reasons for Abuse", y = "Proportion") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Render Regular Bar Chart
  output$regularBarChart <- renderPlot({
    data_to_plot <- reactive_data()
    if (nrow(data_to_plot) > 0) {
      ggplot(data_to_plot, aes(x = Q901, fill = Q901)) +
        geom_bar() +
        labs(title = "Count of Abuse Reasons", x = "Reasons for Abuse", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Render Heatmap
  output$heatmapPlot <- renderPlot({
    data_to_plot <- reactive_data() %>%
      group_by(Q708a, Q901) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      ungroup() %>%
      complete(Q708a, Q901, fill = list(Count = 0))
    
    count_range <- range(data_to_plot$Count, na.rm = TRUE)
    
    if (nrow(data_to_plot) > 0) {
      ggplot(data_to_plot, aes(x = Q901, y = Q708a, fill = Count)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue4", space = "Lab", 
                            na.value = "white", limit = count_range, name = "Count") +
        labs(title = "Heatmap of Fear Factor vs. Reasons for Abuse", x = "Reasons for Abuse", y = "Fear Factor") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              axis.text.y = element_text(angle = 45, hjust = 1),
              legend.position = "right")
    }
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)