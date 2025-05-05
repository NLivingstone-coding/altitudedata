# Load packages --------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(janitor)
library(tidyverse)
library(car)
library(lme4)
library(emmeans)
library(lmerTest)
library(DT)
library(shinythemes)
library(bslib)
library(broom)
library(plotly)
library(shinyWidgets)
library(waiter)
library(corrplot)
library(rsconnect)

# Import & clean -------------------------------------------------------------
setwd("C:/Users/Natas/Desktop/MSc Assessment Data")
msc_data <- read_csv("msc_data.csv")
data     <- clean_names(msc_data)

data$id            <- as.factor(data$id)
data$date          <- as.Date(data$date, format = "%m/%d/%Y")
data$altitude_code <- as.factor(data$altitude_code)
data$gd            <- as.factor(data$gd)
data$replication   <- as.factor(data$replication)

variable_titles <- c(
  "soreness_1" = "Soreness",
  "player_load" = "Player Load",
  "distance"    = "Distance",
  "mood_1"      = "Mood",
  "nutrition_1" = "Nutrition",
  "altitude"    = "Altitude"
)

data <- data %>%
  mutate(
    gd = case_when(
      gd == '0' ~ "Match Day",
      gd == '1' ~ "MD -1",
      gd == '2' ~ "Training MD -2",
      TRUE       ~ as.character(gd)
    ),
    altitude_code = case_when(
      altitude_code == '1' ~ "Sea Level",
      altitude_code == '2' ~ "1000-2000m",
      altitude_code == '3' ~ ">2000m",
      TRUE                 ~ as.character(altitude_code)
    )
  )

# Theme ----------------------------------------------------------------------
custom_theme <- bs_theme(
  bootswatch   = "superhero",
  primary      = "#011F4B",
  secondary    = "#6CAEDD",
  success      = "#B59A5E",
  info         = "#6CAEDD",
  warning      = "#B59A5E",
  danger       = "#E2434B",
  base_font    = "Segoe UI"
)

# UI -------------------------------------------------------------------------
ui <- fluidPage(
  theme = custom_theme,
  useWaiter(),
  
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #011F4B !important; border-color: #B59A5E !important; }
      .navbar-brand { color: white !important; font-weight: bold; }
      .box { border-top: 3px solid #011F4B; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
      .value-box { box-shadow: 0 4px 8px rgba(0,0,0,0.2); }
      .value-box .icon { font-size: 70px; opacity: 0.3; }

      /* Style the stats-printing area */
      #stat_output {
        background-color: #f0f8ff !important;
        border: 1px solid #011F4B !important;
        padding: 10px !important;
        border-radius: 4px !important;
        color: #011F4B !important;
        overflow-x: auto !important;
        white-space: pre-wrap !important;
      }
    "))
  ),
  
  ## Header
  fluidRow(column(
    12,
    div(style = "display:flex; justify-content:space-between; align-items:center;
               padding:10px 0; border-bottom:2px solid #B59A5E;",
        tags$img(src="logo_left.png",  height="100px", width="100px"),
        div(
          h1("Philadelphia Union", style="color:white;font-weight:bold;"),
          h3("Is altitude associated with increased fatigue after a game?", style="color:white;"),
          style="text-align:center;"
        ),
        tags$img(src="logo_right.png", height="100px", width="100px")
    )
  )),
  
  ## Main nav
  navbarPage("Dashboard",
             
             ### Team tab
             tabPanel("Team",
                      dashboardPage(
                        dashboardHeader(title="Philadelphia Union", disable=TRUE),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Select GD:", icon=icon("dashboard")),
                            selectInput("gd_select", "Select GD:", choices=NULL),
                            
                            menuItem("Select Y-Axis Variable:", icon=icon("chart-line")),
                            selectInput("yaxis_select", "Select Y-Axis Variable:", choices=NULL),
                            
                            menuItem("Plot Options", icon=icon("chart-bar")),
                            selectInput("plot_type", "Plot Type:", choices=c("Boxplot","Bar Chart"), selected="Boxplot"),
                            
                            menuItem("Statistical Tests", icon=icon("calculator")),
                            checkboxInput("show_stats", "Show Statistical Tests", TRUE),
                            conditionalPanel(
                              condition = "input.show_stats == true",
                              selectInput("stat_test", "Statistical Test:", choices=c("ANOVA","Linear Mixed Model"), selected="Linear Mixed Model"),
                              actionBttn("run_stats", "Run Analysis", style="fill", color="primary", icon=icon("play"), block=TRUE)
                            )
                          ),
                          fluidRow(
                            column(6, downloadBttn("download_plot",  "Download Plot",      style="fill", color="warning", size="xs")),
                            column(6, downloadBttn("download_stats", "Download Statistics", style="fill", color="warning", size="xs"))
                          )
                        ),
                        dashboardBody(
                          fluidRow(
                            valueBoxOutput("avg_value_box"),
                            valueBoxOutput("median_value_box"),
                            valueBoxOutput("sd_value_box")
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              box(
                                title = "Altitude Response Analysis",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 12,
                                tabsetPanel(
                                  id = "plotTabs",
                                  tabPanel("Interactive Plot", plotlyOutput("interactive_plot", height = "500px")),
                                  tabPanel("Static Plot", plotOutput("static_plot",  height = "500px")),
                                  tabPanel("Correlations",
                                           fluidRow(
                                             column(4,
                                                    checkboxGroupInput(
                                                      "corr_vars", 
                                                      "Select metrics to correlate:",
                                                      choices = c(
                                                        "Soreness"     = "soreness_1",
                                                        "Player Load"  = "player_load",
                                                        "Distance"     = "distance",
                                                        "Mood"         = "mood_1",
                                                        "Nutrition"    = "nutrition_1",
                                                        "Altitude"     = "altitude_code"
                                                      ),
                                                      selected = c("soreness_1", "player_load")
                                                    )
                                             ),
                                             column(8,
                                                    plotOutput("correlation_plot", height = "500px")
                                             )
                                           )
                                  )
                                )    # end tabsetPanel
                              )      # end box
                            )        # end column
                          ),          # end fluidRow
                          
                          box(
                            title="Analysis for Coaches", width=12, collapsible=TRUE,
                            div(style="padding:15px; border-left:4px solid #0275d8; background:#e8f4f8;",
                                uiOutput("sorenessExplanation"))
                          ),
                          
                          conditionalPanel(
                            condition="input.show_stats == true",
                            fluidRow(
                              column(
                                12,
                                box(
                                  title="Statistical Analysis Results", status="info",
                                  solidHeader=TRUE, width=12,
                                  tabsetPanel(
                                    id="statsTabs",
                                    tabPanel("Summary",     verbatimTextOutput("stat_output")),
                                    tabPanel("Diagnostics", plotOutput("diagnostics_plot", height="500px")),
                                    tabPanel("Interpretation", uiOutput("interpretation")),
                                    tabPanel("Data Table",  DTOutput("stats_table")),
                                    tabPanel("Emmeans",     tableOutput("emmeans_table_simple"))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
             ), # end Team tab
             
             tabPanel(
               "Individual Players",
               dashboardPage(
                 dashboardHeader(title = "Individual Player Analysis", disable = TRUE),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("Select Player ID:", icon = icon("user")),
                     selectInput("player_id", "Select Player ID:", choices = NULL),
                     menuItem("Select Outcome Measure:", icon = icon("chart-line")),
                     selectInput("outcome_measure", "Select Outcome Measure:", choices = NULL)
                   )
                 ),
                 dashboardBody(
                   # top value‐boxes
                   fluidRow(
                     valueBoxOutput("ind_avg_box"),
                     valueBoxOutput("ind_sd_box"),
                     valueBoxOutput("ind_trend_box")
                   ),
                   # main content box with tabs
                   fluidRow(
                     column(
                       width = 12,
                       box(
                         title       = "Individual Player Analysis",
                         status      = "primary",
                         solidHeader = TRUE,
                         width       = 12,
                         tabsetPanel(
                           id = "individualTabs",
                           tabPanel(
                             "By Altitude",
                             plotOutput("individual_plot", height = "500px")
                           ),
                           tabPanel(
                             "Vs Team",
                             plotOutput("team_comparison_plot", height = "500px")
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
            )
)

# Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  # Loading screen
  w <- Waiter$new(html=spin_dots(), color="#011F4B")
  w$show(); Sys.sleep(1); w$hide()
  
  # Reactives
  filtered_data <- reactive({
    req(input$gd_select, input$yaxis_select)
    data %>% filter(gd == input$gd_select)
  })
  individual_filtered_data <- reactive({
    req(input$player_id, input$outcome_measure)
    data %>% filter(id == input$player_id)
  })
  
  # Populate the four NULL dropdowns--------------------------------------------
  observe({
    updateSelectInput(session, "gd_select",
                      choices  = unique(data$gd),
                      selected = unique(data$gd)[1])
    updateSelectInput(session, "yaxis_select",
                      choices  = setNames(names(variable_titles), variable_titles),
                      selected = names(variable_titles)[1])
    updateSelectInput(session, "player_id",
                      choices  = unique(data$id),
                      selected = unique(data$id)[1])
    updateSelectInput(session, "outcome_measure",
                      choices  = setNames(names(variable_titles), variable_titles),
                      selected = names(variable_titles)[1])
  })
  
  # Team Box Plot--------------------------------------------------
  output$interactive_plot <- renderPlotly({
    df     <- filtered_data()
    metric <- input$yaxis_select
    
    if (input$plot_type == "Boxplot") {
      p <- ggplot(df, aes_string(x = "altitude_code", y = metric)) +
        geom_boxplot(aes(fill = altitude_code), alpha = 0.7) +
        geom_jitter(aes(text = paste("Player ID:", id)),
                    color = "black", width = 0.3, size = 1.5, alpha = 0.7) +
        scale_fill_manual(values = c("#B59A5E", "#011F4B", "#6CAEDD")) +
        labs(
          x     = "Altitude Levels (m)",
          y     = variable_titles[metric],
          title = paste(input$gd_select, variable_titles[metric], "at Different Altitude Levels")
        ) +
        theme_classic()
      
      ggplotly(p, tooltip = "text") %>%
        layout(boxmode = "group")
      
    } else {
      # summary bar plot of mean ± SD
      summary_df <- df %>%
        group_by(altitude_code) %>%
        summarise(
          mean_val = mean(get(metric), na.rm = TRUE),
          sd_val   = sd(  get(metric), na.rm = TRUE),
          .groups  = "drop"
        )
      
      p <- ggplot(summary_df, aes(x = altitude_code, y = mean_val, fill = altitude_code)) +
        geom_col(width = 0.5, alpha = 0.6) +
        geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
                      width = 0.2, color = "black") +
        scale_fill_manual(values = c("Sea Level" = "#B59A5E",
                                     "1000-2000m"= "#011F4B",
                                     ">2000m"    = "#6CAEDD")) +
        labs(
          x     = "Altitude Level",
          y     = paste("Mean", variable_titles[metric]),
          title = paste("Mean", variable_titles[metric], "± SD by Altitude Level")
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "none"
        )
    }
    
    # render as Plotly
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(barmode = "group")
  })  
  
  
  # Team: Static Plot-----------------------------------------------------------
  static_plot_gg <- reactive({
    df     <- filtered_data()
    metric <- input$yaxis_select
    
    if (input$plot_type == "Boxplot") {
      ggplot(df, aes_string(x = "altitude_code", y = metric)) +
        geom_boxplot(aes(fill = altitude_code), alpha = 0.7) +
        geom_jitter(color = "black", width = 0.3, size = 1.5, alpha = 0.7) +
        scale_fill_manual(values = c("#B59A5E", "#011F4B", "#6CAEDD")) +
        labs(
          x     = "Altitude Levels (m)",
          y     = variable_titles[metric],
          title = paste(input$gd_select, variable_titles[metric], "at Different Altitude Levels")
        ) +
        theme_classic() +
        theme(
          plot.title      = element_text(size = 16, face = "bold"),
          axis.title      = element_text(size = 14),
          axis.text       = element_text(size = 12),
          legend.position = "bottom"
        )
    } else {
      summary_df <- df %>%
        group_by(altitude_code) %>%
        summarise(
          mean_val = mean(get(metric), na.rm = TRUE),
          sd_val   = sd(  get(metric), na.rm = TRUE),
          .groups  = "drop"
        )
      
      ggplot(summary_df, aes(x = altitude_code, y = mean_val, fill = altitude_code)) +
        geom_col(width = 0.5, alpha = 0.6) +
        geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val),
                      width = 0.2, color = "black") +
        scale_fill_manual(values = c(
          "Sea Level"   = "#B59A5E",
          "1000-2000m"  = "#011F4B",
          ">2000m"      = "#6CAEDD"
        )) +
        labs(
          x     = "Altitude Level",
          y     = paste("Mean", variable_titles[metric]),
          title = paste("Mean", variable_titles[metric], "± SD by Altitude Level")
        ) +
        theme_classic() +
        theme(
          plot.title      = element_text(size = 16, face = "bold"),
          axis.title      = element_text(size = 14),
          axis.text       = element_text(size = 12),
          legend.position = "none"
        )
    }
  })
  
  # 2) now render that reactive plot
  output$static_plot <- renderPlot({
    static_plot_gg()
  })
  
  # 3) download handler for exactly that same plot
  output$download_plot <- downloadHandler(
    filename = function() { paste0("plot-", Sys.Date(), ".png") },
    content  = function(file) {
      ggsave(file, plot = static_plot_gg(), width = 10, height = 7, dpi = 300)
    }
  )
  
  # Correlation plot ----------------------------------------------------------
  output$correlation_plot <- renderPlot({
    req(input$corr_vars)
    
    df <- filtered_data() %>%
      mutate(altitude_code = case_when(
        altitude_code == "Sea Level"   ~ 1,
        altitude_code == "1000-2000m"  ~ 2,
        altitude_code == ">2000m"      ~ 3
      )) %>%
      select(all_of(input$corr_vars), id) %>%
      na.omit()
    
    if (length(input$corr_vars) > 2) {
      # Correlation matrix
      M <- cor(df %>% select(-id), use = "pairwise.complete.obs")
      corrplot::corrplot(
        M,
        method      = "shade",
        addCoef.col = "black",
        tl.col      = "#011F4B",
        tl.srt      = 45,
        mar         = c(0, 0, 1, 0)
      )
      
    } else if (length(input$corr_vars) == 2) {
      # Scatterplot with trendline
      xvar <- input$corr_vars[[1]]
      yvar <- input$corr_vars[[2]]
      
      # For axis labels
      xlab <- if (xvar == "altitude_code") "Altitude" else variable_titles[[xvar]]
      ylab <- if (yvar == "altitude_code") "Altitude" else variable_titles[[yvar]]
      
      ggplot(df, aes_string(x = xvar, y = yvar)) +
        geom_line(aes(group = id), color = "gray80", alpha = 0.4) +
        geom_point(color = "#011F4B", alpha = 0.7) +
        geom_smooth(
          method = "lm",
          se     = TRUE,
          color  = "#011F4B",
          fill   = "#6CAEDD",
          size   = 0.8
        ) +
        theme_classic() +
        labs(
          x     = xlab,
          y     = ylab,
          title = paste0(ylab, " vs. ", xlab, " (n=", nrow(df), ")")
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text  = element_text(size = 10)
        )
      
    } else {
      # Fallback if fewer than 2 variables selected
      plot.new()
      text(0.5, 0.5, "Please select at least two metrics to correlate.",
           col = "#011F4B", cex = 1.2)
    }
  })
  
  
  # Box Plot Explanation depending on variable----------------------------------
  output$sorenessExplanation <- renderUI({
    selected_var <- input$yaxis_select
    
    # Create different explanations based on the selected variable
    if (selected_var == "soreness_1") {
      content <- paste0(
        "<p> Perceived muscle <strong>Soreness</strong> at different altitude levels.The plots show the <strong>Soreness</strong> distribution two days before match (MD-2), one day before match (MD-1), and on match day (MD)—for each altitude band (>2 000 m, 1 000–2 000 m, sea level). </p>",
        "<p><strong>Soreness:</strong> MD-2 and MD-1 show similar levels however there is a significant difference after the match. The scales were incorrectly reported, these plots should display soreness significantly higher. </p>",
        "<p>The strongest positive correlations >0.40 are Nutrition & Soreness: r = 0.44	and Mood & Soreness: r = 0.42.</p>"
      )
      advice <- "Soreness, Mood & Nutrition: Soreness goes hand-in-hand with both mood and nutrition scores, monitor these closely together to build a fuller picture of athlete well-being. Check individual player plots to find out which players specifically report these lower well-being scores. Ensure data collection is done correctly as we know soreness outcomes have been reported the opposite way round so the negative correlation may have shown a positive that altitude is associated with increased fatigue. "
    } 
    else if (selected_var == "player_load") {
      content <- paste0(
        "<p>These plots show <strong>Player Load</strong> at different altitude levels. The plots show <strong>Player Load</strong> distribution two days before match (MD-2), one day before match (MD-1), and on match day (MD)—for each altitude band (>2 000 m, 1 000–2 000 m, sea level). </p>",
        "<p><strong>Player Load:</strong> MD-1 shows a significant difference at high altitude. </p>",
        "<p> As expected the strongest correlation is between distance covered and player load r = 0.86. </p>"
      )
      advice <- "Use player-load or distance interchangeably when monitoring workload as they’re almost perfectly aligned. Above 2000 m, plan for the expected increase in external load either by scaling back prescribed workloads or by scheduling extra recovery to compensate."
    }
    else if (selected_var == "distance") {
      content <- paste0(
        "<p>These plots show <strong>Distance</strong> measurements at different altitude levels. <strong>Distance</strong> distribution two days before match (MD-2), one day before match (MD-1), and on match day (MD)—for each altitude band (>2 000 m, 1 000–2 000 m, sea level).</p>",
        "<p><strong>Distance:</strong> A significant difference can be seen from distance covered at altitude MD-1. The distance covered is not a performance max test instead just the total m covered. Higher distances at >2000 m likely reflects how training is organised (such as greater volume at low intensity) and how the athletes respond to hypoxia.</p>",
        "<p>As expected the strongest correlation is between distance covered and player load r = 0.86. A negative association between distance and altitude r = -0.33. </p>"
      )
      advice <- "Use player-load or distance interchangeably when monitoring workload as they’re almost perfectly aligned. Above 2000 m, plan for the expected increase in distance either by scaling back prescribed workloads or by scheduling extra recovery to compensate."
    }
    else if (selected_var == "mood_1") {
      content <- paste0(
        "<p>These plots show <strong>Mood</strong> measurements at different altitude levels.<strong>Mood</strong> distribution two days before match (MD-2), one day before match (MD-1), and on match day (MD)—for each altitude band (>2 000 m, 1 000–2 000 m, sea level).</p>",
        "<p><strong>Mood:</strong> A signficant drop in mood can be seen at >2000m altitude. </p>",
        "<p> The strongest positive correlation for mood >0.40 is between Mood & Soreness: r = 0.42.</p>"
      )
      advice <- "Soreness, Mood & Nutrition: Mood goes hand-in-hand with both soreness and nutrition scores, monitor these closely together to build a fuller picture of athlete well-being. Check individual player plots to find out which players specifically report these lower well-being scores. Ensure data collection is done correctly although a significant difference is shown there was multiple NA for this."
    }
    else if 
    (selected_var == "nutrition_1") {
      content <- paste0(
        "<p>These plots show <strong>Nutrition</strong> measurements at different altitude levels.<strong>Nutrition</strong> distribution two days before match (MD-2), one day before match (MD-1), and on match day (MD)—for each altitude band (>2 000 m, 1 000–2 000 m, sea level).</p>",
        "<p><strong>Nutrition:</strong> Slight changes can be seen at altitude >2000m but nothing signficant </p>",
        "<p> The strongest positive correlation is between	Nutrition & Soreness: r = 0.44. </p>"
      )
      advice <- "Nutrition goes hand-in-hand with both soreness and mood scores, monitor these closely together to build a fuller picture of athlete well-being. Check individual player plots to find out which players specifically report these lower well-being scores. Ensure data collection is done correctly although a significant difference is shown there was multiple NA for this.</p>"
    }
    
    # Combine the explanation and advice with proper styling
    HTML(paste0(
      "<div style='color: #000000; background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6; font-size: 16px; margin-bottom: 10px;'>",
      content,
      "</div>",
      "<div style='color: #000000; background-color: #e9ecef; padding: 15px; border-radius: 5px; border: 1px solid #ced4da; font-weight: 500; font-size: 16px;'>",
      "<strong>Recommendations:</strong> ", advice,
      "</div>"
    ))
  })
  
  # Team: Value boxes------------------------------------------------------------
  output$avg_value_box <- renderValueBox({
    v <- filtered_data() %>% summarise(v=round(mean(get(input$yaxis_select), na.rm=TRUE),1)) %>% pull(v)
    valueBox(v, paste("Average", variable_titles[input$yaxis_select]), icon=icon("chart-line"), color="navy")
  })
  output$median_value_box <- renderValueBox({
    v <- filtered_data() %>% summarise(v=round(median(get(input$yaxis_select), na.rm=TRUE),1)) %>% pull(v)
    valueBox(v, paste("Median", variable_titles[input$yaxis_select]), icon=icon("chart-bar"), color="aqua")
  })
  output$sd_value_box <- renderValueBox({
    v <- filtered_data() %>% summarise(v=round(sd(get(input$yaxis_select), na.rm=TRUE),1)) %>% pull(v)
    valueBox(v, paste("SD of", variable_titles[input$yaxis_select]), icon=icon("chart-area"), color="black")
  })
  
  
  # Team: Statistical analysis--------------------------------------------------
  observeEvent(input$run_stats, {
    w$show()
    
    output$stat_output <- renderPrint({
      metric_var <- input$yaxis_select
      df         <- filtered_data()
      
      if (input$stat_test == "ANOVA") {
        model   <- aov(reformulate("altitude_code", response = metric_var), data = df)
        ares    <- summary(model)
        posthoc <- pairwise.t.test(df[[metric_var]], df$altitude_code, p.adjust.method = "bonferroni")
        cat("### One-way ANOVA ###\n"); print(ares)
        cat("\n### Post Hoc (Bonferroni) ###\n"); print(posthoc)
      } else {
        model   <- lmer(as.formula(paste(metric_var, "~ altitude_code + (1|id)")), data = df)
        ares    <- anova(model)
        posthoc <- emmeans(model, pairwise ~ altitude_code, adjust = "bonferroni")
        cat("### Linear Mixed Model ###\n"); print(ares)
        cat("\n### Post Hoc (Bonferroni) ###\n"); print(posthoc)
      }
    })
    
    # Diagnostics plot----------------------------------------------------------
    output$diagnostics_plot <- renderPlot({
      metric_var <- input$yaxis_select
      df         <- filtered_data()
      
      if (input$stat_test == "ANOVA") {
        model_aov <- aov(reformulate("altitude_code", response = metric_var), data = df)
        par(mfrow = c(2, 2))
        plot(model_aov)
      } else {
        model_lmm <- lmer(as.formula(paste(metric_var, "~ altitude_code + (1|id)")), data = df)
        res <- resid(model_lmm)
        fit <- fitted(model_lmm)
        par(mfrow = c(2, 2))
        
        # 1) Residuals vs Fitted
        plot(fit, res,
             xlab = "Fitted values", ylab = "Residuals",
             main = "Residuals vs Fitted")
        abline(h = 0, lty = 2, col = "red")
        
        # 2) Normal Q‐Q
        qqnorm(res, main = "Normal Q-Q")
        qqline(res, col = "red")
        
        # 3) Scale‐Location
        sqrtRes <- sqrt(abs(res))
        plot(fit, sqrtRes,
             xlab = "Fitted values",
             ylab = expression(sqrt("|Residuals|")),
             main = "Scale-Location")
        
        # 4) Residuals histogram
        hist(res,
             main = "Histogram of Residuals",
             xlab = "Residuals",
             breaks = 10,
             col = "red", border = "white")
      }
    })
    
    output$stats_table <- renderDT({
      df <- filtered_data() %>%
        group_by(altitude_code) %>%
        summarise(
          Mean   = round(mean(get(input$yaxis_select), na.rm = TRUE), 2),
          SD     = round(sd(get(input$yaxis_select), na.rm = TRUE),   2),
          Median = round(median(get(input$yaxis_select), na.rm = TRUE), 2),
          Min    = round(min(get(input$yaxis_select), na.rm = TRUE),    2),
          Max    = round(max(get(input$yaxis_select), na.rm = TRUE),    2),
          N      = n()
        )
      datatable(df,
                options = list(pageLength = 10,
                               scrollX    = TRUE,
                               dom        = 'Bfrtip',
                               buttons    = c('copy','csv','excel')),
                caption = paste("Summary Statistics for", variable_titles[input$yaxis_select]))
    })
    
    output$emmeans_table_simple <- renderTable({
      metric_var <- input$yaxis_select
      df         <- filtered_data()
      if (input$stat_test == "ANOVA") {
        ee <- emmeans(aov(reformulate("altitude_code", response = metric_var), data = df), ~ altitude_code)
      } else {
        ee <- emmeans(lmer(as.formula(paste(metric_var, "~ altitude_code + (1|id)")), data = df), ~ altitude_code)
      }
      as.data.frame(summary(ee))[, c("altitude_code","emmean","SE","df","lower.CL","upper.CL")]
    }, rownames = FALSE)
    
    # Stats Interpretation-------------------------------------------------------
    output$interpretation <- renderUI({
      req(input$show_stats)
      metric <- input$yaxis_select
      
      if (metric == "soreness_1") {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Key Findings", style = "color: #000000; font-weight: bold;"),
          p(tags$strong("Main Effect:"), style = "color: #011F4B;",
            "The linear mixed‐effects model—with a random intercept for each player—reveals a highly significant effect of altitude on reported muscle soreness (F(2, 36) = 25.08, p < .001)."
          ),
          p(tags$strong("Differences:"), style = "color: #011F4B;",
            "Only the > 2000 m condition differs: mean soreness = 3.12 (95 % CI [2.30, 3.95])."
          ),
          p(tags$strong("No Difference:"), style = "color: #011F4B;",
            "There was no significant difference between medium altitude (1000-2000m) and sea level conditions."
          ),
          br(),
          h4("Analysis", style = "color: #000000; font-weight: bold;"),
          p(style = "color: #011F4B;",
            "Data quality means altitude can not be the only cause of soreness, but the strong correlation provided by the lmm accounts for the between player variability, so the fixed p-values are more reliable than a simple ANOVA."
          ),
          p(style = "color: #011F4B;",
            "The residual diagnostic plots show no major deviations from the normality or homoscedasticity, supporting the validity of the models’ conclusions."
          )
        )
        
      } else if (metric == "player_load") {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Key Findings", style = "color: #000000; font-weight: bold;"),
          p(tags$strong("Main Effect:"), style = "color: #011F4B;",
            "A linear mixed‐effects model revealed no significant main effect of altitude on Player Load, F(2, 31.8) = 0.68, p = 0.51."
          ),
          p(tags$strong("No Difference:"), style = "color: #011F4B;",
            "Pairwise contrasts with Bonferroni correction showed no significant differences between conditions"
          ),
          br(),
          h4("Analysis", style = "color: #000000; font-weight: bold;"),
          p(style = "color: #011F4B;",
            "PlayerLoad averaged 414.5 ± 247.8 (mean ± SD), ranging from 175.9 to 1 175.0 (median 347.4; CV = 59.8%)"
          ),
          p(style = "color: #011F4B;",
            "Due to the large spread in Player Load data between athletes and between sessions, the lmm captures athlete specific baselines and allows for more realistic variance and covariance structure so the preferred method over the ANOVA for such unbalanced data. Residual diagnostics indicated  that assumptions of linearity, homoscedasticity, and approximate normality were met."
          )
        )
        
      } else if (metric == "distance") {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Key Findings", style = "color: #000000; font-weight: bold;"),
          p(tags$strong("Main Effect:"), style = "color: #011F4B;",
            "A linear mixed‐effects model showed no significant effect of altitude on total distance covered, F(2, 29.7) = 1.31, p = 0.29."
          ),
          p(tags$strong("No Difference:"), style = "color: #011F4B;",
            "Bonferroni‐corrected pairwise contrasts confirmed no differences between any of the altitude conditions (all p > 0.53)."
          ),
          br(),
          h4("Analysis", style = "color: #000000; font-weight: bold;"),
          p(style = "color: #011F4B;",
            "Residual diagnostic plots revealed no serious deviations from linearity, homoscedasticity, or normality of residuals. The unbalanced design and variability between athletes and sessions, the LMM remains preferable to a traditional ANOVA, as it captures subject‐specific baselines and accommodates realistic variance–covariance structures in the data."
          )
        )
        
      } else if (metric == "mood_1") {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Key Findings", style = "color: #000000; font-weight: bold;"),
          p(tags$strong("Main Effect:"), style = "color: #011F4B;",
            "A linear mixed‐effects model revealed a significant effect of altitude on mood, F(2, 31.4) = 14.89, p < 0.001. "
          ),
          p(tags$strong("Differences:"), style = "color: #011F4B;",
            "Bonferroni‐corrected pairwise contrasts showed that mood was significantly lower at > 2 000 m compared with both 1 000–2 000 m (t(32.5) = –4.73, p = 0.0001) and sea level (t(31.2) = –5.06, p = 0.0001)"
          ),
          p(tags$strong("No Difference:"), style = "color: #011F4B;",
            "No difference between sea level and 1 000–2 000 m (t(29.7) = 0.08, p = 1.00)"
          ),
          br(),
          h4("Analysis", style = "color: #000000; font-weight: bold;"),
          p(style = "color: #011F4B;",
            "Your manual recommendations or caveats for Mood…"
          ),
          p(style = "color: #011F4B;",
            "Any further coach notes on Mood…"
          )
        )
        
      } else if (metric == "nutrition_1") {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Key Findings", style = "color: #000000; font-weight: bold;"),
          p(tags$strong("Main Effect:"), style = "color: #011F4B;",
            "Although the LMM found no significant main effect of altitude on nutrition, F(2, 33) = 2.32, p = 0.11, estimated marginal means indicate that nutrition scores were on average 1.53 points lower at > 2 000 m than at sea level (6.53 ± 0.37 vs. 5.00 ± 0.70; mean difference = –1.53, 95 % CI –3.17 to 0.11)"
          ),
          p(tags=strong("No Difference:"), style = "color: #011F4B;",
            "These contrasts did not reach significance (Bonferroni-adjusted p = 0.17 and p = 0.19, respectively)"
          ),
          br(),
          h4("Analysis", style = "color: #000000; font-weight: bold;"),
          p(style = "color: #011F4B;",
            "The direction and magnitude of the change suggest a small altitude-related decrement in nutrition score that may require further investigation with a larger sample.Residual diagnostics indicated that assumptions of linearity, homoscedasticity, and approximate normality were adequately met, but the LMM is still the preferred method over the ANOVA"
          )
        )
      }
    })
    
    w$hide()
  })
  
  # Individual: boxplot---------------------------------------------------------
  individual_filtered_data <- reactive({
    data %>% filter(id == input$player_id)
  })
  
  team_data <- reactive({
    data %>% filter(gd == input$gd_select)
  })
  
  # avg value‐box
  output$ind_avg_box <- renderValueBox({
    v <- individual_filtered_data() %>% 
      summarise(mean = round(mean(get(input$outcome_measure), na.rm = TRUE), 1)) %>% 
      pull(mean)
    valueBox(
      v, 
      paste("Avg", variable_titles[input$outcome_measure]), 
      icon = icon("chart-line"), 
      color = "navy"
    )
  })
  
  # sd value‐box
  output$ind_sd_box <- renderValueBox({
    v <- individual_filtered_data() %>% 
      summarise(sd = round(sd(get(input$outcome_measure), na.rm = TRUE), 1)) %>% 
      pull(sd)
    valueBox(
      v, 
      paste("SD", variable_titles[input$outcome_measure]), 
      icon = icon("chart-bar"), 
      color = "aqua"
    )
  })
  output$ind_trend_box <- renderValueBox({
    df <- individual_filtered_data()
    # simple linear trend of outcome versus date
    fit <- lm(get(input$outcome_measure) ~ as.numeric(date), data = df)
    slope <- round(coef(fit)[2]*30, 2)  # change per 30 days, say
    valueBox(
      paste0(ifelse(slope >= 0, "+", ""), slope),
      "30-day Trend",
      icon = icon("arrow-up"),
      color = if (slope >= 0) "green" else "red"
    )
  })
  
  output$individual_plot <- renderPlot({
    ggplot(individual_filtered_data(), aes_string(x = "altitude_code", y = input$outcome_measure)) +
      geom_boxplot(aes(fill = altitude_code), alpha = 0.7) +
      geom_jitter(color = "black", width = 0.3, size = 1.5, alpha = 0.7) +
      scale_fill_manual(values = c("#B59A5E", "#011F4B", "#6CAEDD")) +
      labs(
        x = "Altitude Levels (m)",
        y = variable_titles[input$outcome_measure],
        title = paste("Individual Player", variable_titles[input$outcome_measure], "at Different Altitude Levels")
      ) +
      theme_classic()+
      theme(
        plot.title      = element_text(size = 16, face = "bold"),
        axis.title      = element_text(size = 14),
        axis.text       = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # Individual: team comparison
  output$team_comparison_plot <- renderPlot({
    player_summary <- individual_filtered_data() %>%
      group_by(altitude_code) %>%
      summarise(
        mean_value = mean(get(input$outcome_measure), na.rm = TRUE),
        sd_value = sd(get(input$outcome_measure), na.rm = TRUE),
        n = n()
      ) %>%
      mutate(
        se_value = sd_value / sqrt(n),
        lower_ci = mean_value - qt(0.975, n - 1) * se_value,
        upper_ci = mean_value + qt(0.975, n - 1) * se_value
      )
    
    team_summary <- team_data() %>%
      group_by(altitude_code) %>%
      summarise(
        mean_value = mean(get(input$outcome_measure), na.rm = TRUE),
        sd_value = sd(get(input$outcome_measure), na.rm = TRUE),
        n = n()
      ) %>%
      mutate(
        se_value = sd_value / sqrt(n),
        lower_ci = mean_value - qt(0.975, n - 1) * se_value,
        upper_ci = mean_value + qt(0.975, n - 1) * se_value
      )
    
    ggplot() +
      geom_col(data = team_summary, aes(x = altitude_code, y = mean_value, fill = "Team"), position = position_dodge()) +
      geom_errorbar(data = team_summary, aes(x = altitude_code, ymin = lower_ci, ymax = upper_ci), position = position_dodge(0.9), width = 0.2, color = "#B59A5E") +
      geom_point(data = player_summary, aes(x = altitude_code, y = mean_value, color = "Player"), size = 3) +
      geom_errorbar(data = player_summary, aes(x = altitude_code, ymin = lower_ci, ymax = upper_ci, color = "Player"), width = 0.2) +
      scale_fill_manual(values = c("Team" = "#011F4B")) +
      scale_color_manual(values = c("Player" = "#6CAEDD")) +
      labs(
        x = "Altitude Levels",
        y = variable_titles[input$outcome_measure],
        title = paste("Comparison of", variable_titles[input$outcome_measure], "between Player and Team at Different Altitude Levels"),
        fill = "Group",
        color = "Group"
      ) +
      theme_classic()+
        theme(
          plot.title      = element_text(size = 16, face = "bold"),
          axis.title      = element_text(size = 14),
          axis.text       = element_text(size = 12),
          legend.position = "bottom"
        )
  })
  
}

# Run the app ----------------------------------------------------------------
shinyApp(ui, server)



