# ============================================================
# app.R — WVS Wave 6 Shiny Dashboard
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Load prepared data
load("data/wvs_prepared.RData")

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "World Values Survey Explorer"),
  
  dashboardSidebar(
    selectInput(
      inputId  = "country",
      label    = "Select a Country:",
      choices  = country_list,
      selected = "United States"
    ),
    sidebarMenu(
      menuItem("Overview",  tabName = "overview",  icon = icon("home")),
      menuItem("Democracy", tabName = "democracy", icon = icon("landmark")),
      menuItem("News",      tabName = "news",      icon = icon("newspaper")),
      menuItem("Science",   tabName = "science",   icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── Tab 1: Overview ──────────────────────────────────
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Welcome to the World Values Survey Explorer",
                    h4("What is this app?"),
                    p("This dashboard lets you explore data from the ",
                      strong("World Values Survey (WVS) Wave 6"),
                      ", a global research project that examines people's values,
               beliefs, and attitudes across 60 countries."),
                    h4("How to use this app"),
                    p("Use the ", strong("dropdown menu on the left"),
                      " to select a country. The dashboard has three sections
               you can explore using the sidebar menu:"),
                    tags$ul(
                      tags$li(strong("Democracy:"),
                              " How people rate different aspects of democracy (V228A-V228I).
                  Rated 1-4, where 1 = not at all democratic, 4 = definitely democratic."),
                      tags$li(strong("News:"),
                              " How often people consume news from different sources (V217-V224).
                  Rated 1-5, where 1 = daily, 5 = never."),
                      tags$li(strong("Science:"),
                              " People's attitudes toward science and technology (V192-V197).
                  Rated 1-10, where 1 = strongly disagree, 10 = strongly agree.")
                    ),
                    h4("What you will find in each section"),
                    p("Each section contains:"),
                    tags$ol(
                      tags$li("An ", strong("interactive chart"),
                              " showing results for your selected country."),
                      tags$li("An ", strong("interactive table"),
                              " with the averages for your selected country."),
                      tags$li("A ", strong("global comparison table"),
                              " showing averages across the entire WVS sample.")
                    ),
                    hr(),
                    p(em("Data source: World Values Survey Wave 6 (2010-2014).
                  www.worldvaluessurvey.org"))
                )
              )
      ),
      
      # ── Tab 2: Democracy ─────────────────────────────────
      tabItem(tabName = "democracy",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Attitudes to Democracy",
                    p("Average score per question for the selected country.
               Scale: 1 = Not at all democratic, 4 = Definitely democratic.")
                )
              ),
              fluidRow(
                box(width = 12, plotlyOutput("democracy_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Country Averages", status = "info",
                    solidHeader = TRUE, DTOutput("democracy_country_table")),
                box(width = 6, title = "Global Averages (All Countries)", status = "warning",
                    solidHeader = TRUE, DTOutput("democracy_global_table"))
              )
      ),
      
      # ── Tab 3: News ──────────────────────────────────────
      tabItem(tabName = "news",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "News Consumption",
                    p("Proportion of respondents in each frequency category for the selected country.
               Scale: 1 = Daily, 2 = Weekly, 3 = Monthly, 4 = Less than monthly, 5 = Never.")
                )
              ),
              fluidRow(
                box(width = 12, plotlyOutput("news_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Country Proportions", status = "info",
                    solidHeader = TRUE, DTOutput("news_country_table")),
                box(width = 6, title = "Global Proportions (All Countries)", status = "warning",
                    solidHeader = TRUE, DTOutput("news_global_table"))
              )
      ),
      
      # ── Tab 4: Science ───────────────────────────────────
      tabItem(tabName = "science",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Attitudes to Science",
                    p("Average score per question for the selected country.
               Scale: 1 = Strongly disagree, 10 = Strongly agree.")
                )
              ),
              fluidRow(
                box(width = 12, plotlyOutput("science_plot", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Country Averages", status = "info",
                    solidHeader = TRUE, DTOutput("science_country_table")),
                box(width = 6, title = "Global Averages (All Countries)", status = "warning",
                    solidHeader = TRUE, DTOutput("science_global_table"))
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ── Reactive: filter data for selected country ──────────
  country_data <- reactive({
    wvs_clean %>% filter(country == input$country)
  })
  
  # ══════════════════════════════════════════════════════════
  # DEMOCRACY
  # ══════════════════════════════════════════════════════════
  
  democracy_country <- reactive({
    country_data() %>%
      summarise(across(V228A:V228I, ~mean(., na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "mean") %>%
      mutate(question = democracy_labels[variable])
  })
  
  output$democracy_plot <- renderPlotly({
    p <- democracy_country() %>%
      ggplot(aes(x = reorder(question, mean), y = mean,
                 fill = mean,
                 text = paste0(question, "\nMean: ", round(mean, 2)))) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#d73027", high = "#1a9850") +
      scale_y_continuous(limits = c(0, 4)) +
      labs(x = NULL, y = "Average Score (1-4)",
           title = paste("Attitudes to Democracy —", input$country)) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  output$democracy_country_table <- renderDT({
    democracy_country() %>%
      select(Question = question, `Mean Score` = mean) %>%
      mutate(`Mean Score` = round(`Mean Score`, 2)) %>%
      datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
  
  output$democracy_global_table <- renderDT({
    global_democracy %>%
      select(Question = question, `Global Mean` = global_mean) %>%
      mutate(`Global Mean` = round(`Global Mean`, 2)) %>%
      datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
  
  # ══════════════════════════════════════════════════════════
  # NEWS
  # ══════════════════════════════════════════════════════════
  
  news_country <- reactive({
    country_data() %>%
      select(V217:V224) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      filter(!is.na(value)) %>%
      group_by(variable, value) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(variable) %>%
      mutate(
        proportion = n / sum(n),
        question   = news_labels[variable],
        category   = factor(value, levels = 1:5,
                            labels = c("Daily", "Weekly", "Monthly",
                                       "Less than monthly", "Never"))
      )
  })
  
  output$news_plot <- renderPlotly({
    p <- news_country() %>%
      ggplot(aes(x = question, y = proportion, fill = category,
                 text = paste0(question, "\n", category, ": ",
                               round(proportion * 100, 1), "%"))) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = "RdYlGn", direction = -1) +
      labs(x = NULL, y = "Proportion", fill = "Frequency",
           title = paste("News Consumption —", input$country)) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$news_country_table <- renderDT({
    news_country() %>%
      mutate(proportion = round(proportion * 100, 1)) %>%
      select(Question = question, Category = category,
             `Proportion (%)` = proportion) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$news_global_table <- renderDT({
    global_news %>%
      mutate(
        proportion = round(proportion * 100, 1),
        category   = factor(value, levels = 1:5,
                            labels = c("Daily", "Weekly", "Monthly",
                                       "Less than monthly", "Never"))
      ) %>%
      select(Question = question, Category = category,
             `Proportion (%)` = proportion) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ══════════════════════════════════════════════════════════
  # SCIENCE
  # ══════════════════════════════════════════════════════════
  
  science_country <- reactive({
    country_data() %>%
      summarise(across(V192:V197, ~mean(., na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "mean") %>%
      mutate(question = science_labels[variable])
  })
  
  output$science_plot <- renderPlotly({
    p <- science_country() %>%
      ggplot(aes(x = reorder(question, mean), y = mean,
                 fill = mean,
                 text = paste0(question, "\nMean: ", round(mean, 2)))) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#d73027", high = "#1a9850") +
      scale_y_continuous(limits = c(0, 10)) +
      labs(x = NULL, y = "Average Score (1-10)",
           title = paste("Attitudes to Science —", input$country)) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  output$science_country_table <- renderDT({
    science_country() %>%
      select(Question = question, `Mean Score` = mean) %>%
      mutate(`Mean Score` = round(`Mean Score`, 2)) %>%
      datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
  
  output$science_global_table <- renderDT({
    global_science %>%
      select(Question = question, `Global Mean` = global_mean) %>%
      mutate(`Global Mean` = round(`Global Mean`, 2)) %>%
      datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
  
} # end server

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)

