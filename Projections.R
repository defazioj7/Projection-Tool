library(shiny)
library(DT)
library(RPostgreSQL)
library(sqldf)
library(zoo)
library(xts)
library(scales)

drv <- dbDriver("PostgreSQL")

#create connection to the adreporting PostgreSQL database
con <- dbConnect(drv, dbname = "production",
                 host = "datawarehouse.corp.qc",
                 port = 5439,
                 user ="jdefazio",
                 password = "Ahb4yoo4Ie")
options(shiny.error = browser)

ui <- fluidPage(

  tabsetPanel(type = "tabs",
              tabPanel("Projections Part II",

                       sidebarPanel(width = 9,
                                    h3("Select Model Data & Review Performance",style="color:blue;font-size:26px"),

                                    selectInput(inputId = "FlightCampaign", label = "Select Basis",
                                                choices= c("Campaigns", "Flights"), selected = "Campaigns"),

                                    selectInput(inputId = "GoalType", label = "Goal type",
                                                choices = c("CPA", "ROAS"), selected = "CPA"),
                                    # textInput(inputId = "Country",
                                    #           label = "Optional: Enter country codes below (separated by spaces)"),
                                    conditionalPanel(
                                      condition = "input.FlightCampaign == 'Campaigns'",


                                      textInput(inputId = "Campaigns",
                                                label = "Enter current / most recent campaign name below:"),
                                      textInput(inputId = "PriorCampaigns",
                                                label = "Optional: Enter prior campaign names below for additional data (separated by spaces):")
                                      # textInput(inputId = "StartDate",
                                      #           label = "Data start date (YYYYY-MM-DD)"),
                                      # textInput(inputId = "EndDate",
                                      #           label = "Data end date (YYYYY-MM-DD)")
                                    ),

                                    conditionalPanel(
                                      condition = "input.FlightCampaign == 'Flights'",


                                      # selectInput(inputId = "Flights", label = "Select flight treatment:",
                                      #             choices = c("Combined flight projection",
                                      #                         "Individual flight projections"),
                                      #             selected = "Combined flight projection"),
                                      textInput(inputId = "Flights",
                                                label = "Enter current / most recent flight name(s) below (separated by spaces):"),
                                      textInput(inputId = "PriorFlights",
                                                label = "Optional: Enter prior flight names below for additional data (separated by spaces):")
                                      # textInput(inputId = "StartDate",
                                      #           label = "Data start date (YYYYY-MM-DD)"),
                                      # textInput(inputId = "EndDate",
                                      #           label = "Data end date (YYYYY-MM-DD)")
                                    ),

                                    actionButton(inputId = "submit", label = "Build Model"),
                                    br(),
                                    br(),
                                    mainPanel(dataTableOutput("CurrentPerformance"))

                       ),
                       sidebarPanel(width = 3,
                                    h3("Project Scenarios",style="color:blue;font-size:26px"),

                                    selectInput(inputId = "CurrentNew", label = "Current or New Campaign / Flight",
                                                choices= c("Current (Incremental)", "New"), selected = "New"),

                                    selectInput(inputId = "GoalBudget", label = "Define Budgets or Goal Seek",
                                                choices = c("Define Budgets", "Goal Seek (Coming Soon)"), selected = "Define Budgets"),

                                    conditionalPanel(condition = "input.CurrentNew == 'New'",

                                                     textInput(inputId = "NewStartDate", label = "New Campaign / Flight Start Date (YYYY-MM-DD)"),
                                                     textInput(inputId = "NewEndDate", label = "New Campaign / Flight End Date (YYYY-MM-DD)")),


                                    conditionalPanel(condition = "input.CurrentNew == 'Current (Incremental)'",

                                                     selectInput(inputId = "IncrementalPeriod", label = "Define Incremental Period",
                                                                 choices= c("Start today / end on schedule", "Custom Period"), selected = "Start today / end on schedule"),

                                                     conditionalPanel(condition = "input.IncrementalPeriod == 'Custom Period'",

                                                                      textInput(inputId = "NewStartDateCustom", label = "Incremental Start Date (YYYY-MM-DD)"),
                                                                      textInput(inputId = "NewEndDateCustom", label = "Incremental End Date (YYYY-MM-DD)"))),

                                    conditionalPanel(condition = "input.GoalBudget == 'Define Budgets'",

                                                     textInput(inputId = "DefinedBudget1",
                                                               label = "Budget Scenario 1"),
                                                     textInput(inputId = "DefinedBudget2",
                                                               label = "Budget Scenario 2"),
                                                     textInput(inputId = "DefinedBudget3",
                                                               label = "Budget Scenario 3"),
                                                     textInput(inputId = "DefinedBudget4",
                                                               label = "Budget Scenario 4")),

                                    conditionalPanel(condition = "input.GoalBudget == 'Goal Seek (Coming Soon)'",

                                                     textInput(inputId = "Goal1",
                                                               label = "Goal 1"),
                                                     textInput(inputId = "Goal2",
                                                               label = "Goal 2"),
                                                     textInput(inputId = "GOal3",
                                                               label = "Goal 3"),
                                                     textInput(inputId = "Goal4",
                                                               label = "Goal 4")),


                                    actionButton(inputId = "ScenarioSubmit", label = "Run this town")
                       )
              ),

              tabPanel("Model Summary", tableOutput("Summary"), tableOutput("table11"), tableOutput("table21"), tableOutput("table31"),
                       tableOutput("table41"), tableOutput("table51")),
              tabPanel("Model 1", tableOutput("table1")),
              tabPanel("Model 2", tableOutput("table2")),
              tabPanel("Model 3", tableOutput("table3")),
              tabPanel("Model 4", tableOutput("table4")),
              tabPanel("Model 5", tableOutput("table5")),
              tabPanel("Data",    tableOutput("DataTable"))
  ))


sidebarPanel(width = 7,
             h3("Run Projections",style="color:Orange;font-size:26px"),
             p("By default, all ad sizes across all devices are considered",style="color:Orange;font-size:13px"),
             p(" ",style="color:Orange;font-size:18px"),

             br(),

             actionButton(inputId = "estimate", label = "Run Scenarios"))


server <- function(input, output){


  ################################################################################################################################################
  #------------------------------------------------------Handling all variables from UI inputs----------------------------------------------------


  #------------------------------------------------------------------------------------------------------------------------------------------------

  #------------------------------------------calculate and display current / priro campaign performance---------------------------------------------
  performance <- eventReactive(input$submit,{

    # IncrementalPeriod  <- input$IncrementalPeriod
    # CurrentNew         <- input$CurrentNew
    # Campaigns          <- input$Campaigns
    # PriorCampaigns     <- input$PriorCampaigns
    # Flights            <- input$Flights
    # PriorFlights       <- input$PriorFlights
    # StartDate          <- input$StartDate
    # EndDate            <- input$EndDate
    GoalType             <- input$GoalType
    Basis                <- input$FlightCampaign

    if (Basis == "Campaigns"){

      Campaigns      <- input$Campaigns
      PriorCampaigns <- input$PriorCampaigns

    }else if(Basis == "Flights"){

      Campaigns        <- input$Flights
      PriorCampaigns   <- input$PriorFlights

    }


    CampaignsQ <- gsub(" ", "','", Campaigns)
    CampaignsQ <- gsub("\n", "", CampaignsQ)
    CampaignsQ <- fn$paste("('$CampaignsQ')")

    PriorCampaigns <- gsub(" ", "','", PriorCampaigns)
    PriorCampaigns <- gsub("\n", "", PriorCampaigns)
    PriorCampaigns <- fn$paste("('$PriorCampaigns')")

    # Flights <- paste(Flights, collapse = '\',\'')
    # Flights <- gsub(" ", "", Flights)
    # Flights <- fn$paste("('$Flights')")
    #
    # PriorFlights <- paste(PriorFlights, collapse = '\',\'')
    # PriorFlights <- gsub(" ", "", PriorFlights)
    # PriorFlights <- fn$paste("('$PriorFlights')")


    if(Basis == "Campaigns"){

      FCInfo <- fn$paste("SELECT distinct c.name AS CampaignName
                         , c.start_date
                         , c.end_date
                         , c.budget*c.exchange_rate
                         , sum(fd.budget_delivered*c.exchange_rate) AS Revenue
                         , sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions) AS Conversions
                         , sum(fd.reporting_verified_credited_client_revenue) AS ClientRevenue
                         , COALESCE(sum(fd.budget_delivered*c.exchange_rate) / NULLIF((sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions)), 0), 0) AS CPA
                         , COALESCE(sum(fd.reporting_verified_credited_client_revenue) / NULLIF(sum(fd.budget_delivered*c.exchange_rate), 0), 0) as ROAS
                         FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                         JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                         WHERE (c.name in $CampaignsQ or c.name in $PriorCampaigns)
                         GROUP BY 1, 2, 3, 4
                         ORDER BY 1;")

      DWQuery <- sqlInterpolate(con, FCInfo)
      FCInfo  <- dbGetQuery(con, DWQuery)

      CurrentPerfTable <- data.frame(matrix(ncol = 7, nrow = as.matrix(dim(FCInfo))[1, 1]))

      CurrentPerfTable[, 1] <- as.data.frame(FCInfo [, 1])
      CurrentPerfTable[, 2] <- as.data.frame(FCInfo [, 2])
      CurrentPerfTable[, 3] <- as.data.frame(FCInfo [, 3])
      CurrentPerfTable[, 4] <- as.data.frame(FCInfo [, 4])
      CurrentPerfTable[, 5] <- as.data.frame(FCInfo [, 5])

      if (GoalType == "CPA"){

        CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 6])
        CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 8])

        colnames(CurrentPerfTable) <- c("Campaign", "Start Date", "End Date",
                                        "Scheduled Budget", "Budget Delivered",
                                        "Conversions", "CPA")
      }else if (GoalType == "ROAS"){

        CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 7])
        CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 9])

        colnames(CurrentPerfTable) <- c("Campaign", "Start Date", "End Date",
                                        "Scheduled Budget", "Budget Delivered",
                                        "Client Revenue", "ROAS")


      }}else if(Basis == "Flights") {

        FCInfo <- fn$paste("SELECT distinct f.name AS FlightName
                           , f.start_date
                           , f.end_date
                           , f.budget*c.exchange_rate
                           , sum(fd.budget_delivered*c.exchange_rate) AS Revenue
                           , sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions) AS Conversions
                           , sum(fd.reporting_verified_credited_client_revenue) AS ClientRevenue
                           , COALESCE(sum(fd.budget_delivered*c.exchange_rate) / NULLIF((sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions)), 0), 0) AS CPA
                           , COALESCE(sum(fd.reporting_verified_credited_client_revenue) / NULLIF(sum(fd.budget_delivered*c.exchange_rate), 0), 0) as ROAS
                           FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                           JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                           WHERE (f.name in $CampaignsQ or f.name in $PriorCampaigns)
                           GROUP BY 1, 2, 3, 4
                           ORDER BY 1;")

        DWQuery <- sqlInterpolate(con, FCInfo)
        FCInfo  <- dbGetQuery(con, DWQuery)

        CurrentPerfTable <- data.frame(matrix(ncol = 7, nrow = as.matrix(dim(FCInfo))[1, 1]))

        CurrentPerfTable[, 1] <- as.data.frame(FCInfo [, 1])
        CurrentPerfTable[, 2] <- as.data.frame(FCInfo [, 2])
        CurrentPerfTable[, 3] <- as.data.frame(FCInfo [, 3])
        CurrentPerfTable[, 4] <- as.data.frame(FCInfo [, 4])
        CurrentPerfTable[, 5] <- as.data.frame(FCInfo [, 5])

        if (GoalType == "CPA"){

          CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 6])
          CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 8])

          colnames(CurrentPerfTable) <- c("Flight", "Start Date", "End Date",
                                          "Scheduled Budget", "Budget Delivered",
                                          "Conversions", "CPA")
        }else if (GoalType == "ROAS"){

          CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 7])
          CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 9])

          colnames(CurrentPerfTable) <- c("Flight", "Start Date", "End Date",
                                          "Scheduled Budget", "Budget Delivered",
                                          "Client Revenue", "ROAS")

        }}

    return(list(CurrentPerfTable))
  })
  output$CurrentPerformance <- renderDataTable({ performance()[[1]] })


  #---------------------------------------------------------Aggregating Data------------------------------------------------------

  data <- eventReactive(input$ScenarioSubmit,{


    IncrementalPeriod  <- input$IncrementalPeriod
    CurrentNew         <- input$CurrentNew
    # Campaigns          <- input$Campaigns
    # PriorCampaigns     <- input$PriorCampaigns
    # Flights            <- input$Flights
    # PriorFlights       <- input$PriorFlights
    GoalType           <- input$GoalType
    Basis              <- input$FlightCampaign

    if (Basis == "Campaigns"){

      Campaigns      <- input$Campaigns
      PriorCampaigns <- input$PriorCampaigns

    }else if(Basis == "Flights"){

      Campaigns        <- input$Flights
      PriorCampaigns   <- input$PriorFlights

    }

    CampaignsQ <- gsub(" ", "','", Campaigns)
    CampaignsQ <- gsub("\n", "", CampaignsQ)
    CampaignsQ <- fn$paste("('$CampaignsQ')")

    PriorCampaigns <- gsub(" ", "','", PriorCampaigns)
    PriorCampaigns <- gsub("\n", "", PriorCampaigns)
    PriorCampaigns <- fn$paste("('$PriorCampaigns')")

    # FlightsQ <- paste(Flights, collapse = '\',\'')
    # FlightsQ <- gsub(" ", "", Flights)
    # FlightsQ <- fn$paste("('$Flights')")

    # PriorFlights <- paste(PriorFlights, collapse = '\',\'')
    # PriorFlights <- gsub(" ", "", PriorFlights)
    # PriorFlights <- fn$paste("('$PriorFlights')")

    Scenario <- matrix(ncol = 1, nrow = 4)
    Scenario[1, 1] <- as.numeric(input$DefinedBudget1)
    Scenario[2, 1] <- as.numeric(input$DefinedBudget2)
    Scenario[3, 1] <- as.numeric(input$DefinedBudget3)
    Scenario[4, 1] <- as.numeric(input$DefinedBudget4)

    if(Basis == "Campaigns"){
      AugQuery <- fn$paste("SELECT c.name AS CampaignName
                           , f.name AS FlightName
                           , fd.date AS ReportDate
                           , fd.reporting_verified_external_impressions AS ExternalImpressions
                           , fd.reporting_verified_external_clicks AS ExternalClicks
                           , (fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions) AS Conversions
                           , fd.reporting_verified_credited_client_revenue AS ClientRevenue
                           , fd.budget_delivered*c.exchange_rate AS Revenue
                           , c.start_date
                           , c.end_date
                           , c.budget*c.exchange_rate
                           FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                           JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                           WHERE (c.name in $CampaignsQ or c.name in $PriorCampaigns)
                           ORDER BY 3;")

    }else if(Basis == "Flights") {

      AugQuery <- fn$paste("SELECT c.name AS CampaignName
                           , f.name AS FlightName
                           , fd.date AS ReportDate
                           , fd.reporting_verified_external_impressions
                           , fd.reporting_verified_external_clicks
                           , (fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions)
                           , fd.reporting_verified_credited_client_revenue
                           , fd.budget_delivered*c.exchange_rate
                           , f.start_date
                           , f.end_date
                           , f.budget*c.exchange_rate
                           FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                           JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                           WHERE (f.name in $CampaignsQ or f.name in $PriorCampaigns)
                           ORDER BY 3;")
    }

    DWQuery <- sqlInterpolate(con, AugQuery)
    GetData <- dbGetQuery(con, DWQuery)

    GetDataTable <- data.frame(matrix(ncol = 11, nrow = as.matrix(dim(GetData))[1, 1]))

    GetDataTable <- as.data.frame(GetData)

    DataLength   <- length(unique(GetData[, 3]))

    if(Basis == 'Campaigns'){

    FCInfo <- fn$paste("SELECT distinct c.name AS CampaignName
                       , c.start_date
                       , c.end_date
                       , c.budget*c.exchange_rate
                       , sum(fd.budget_delivered*c.exchange_rate) AS Revenue
                       , sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions) AS Conversions
                       , sum(fd.reporting_verified_credited_client_revenue) AS ClientRevenue
                       , COALESCE(sum(fd.budget_delivered*c.exchange_rate) / NULLIF((sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions)), 0), 0) AS CPA
                       , COALESCE(sum(fd.reporting_verified_credited_client_revenue) / NULLIF(sum(fd.budget_delivered*c.exchange_rate), 0), 0) as ROAS
                       FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                       JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                       WHERE c.name in $CampaignsQ
                       GROUP BY 1, 2, 3, 4
                       ORDER BY 1;")

    }else if(Basis == 'Flights'){

     FCInfo <- fn$paste("SELECT distinct f.name AS FlightName
                       , f.start_date
                       , f.end_date
                       , f.budget*c.exchange_rate
                       , sum(fd.budget_delivered*c.exchange_rate) AS Revenue
                       , sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions) AS Conversions
                       , sum(fd.reporting_verified_credited_client_revenue) AS ClientRevenue
                       , COALESCE(sum(fd.budget_delivered*c.exchange_rate) / NULLIF((sum(fd.reporting_verified_credited_view_conversions + fd.reporting_verified_credited_click_conversions)), 0), 0) AS CPA
                       , COALESCE(sum(fd.reporting_verified_credited_client_revenue) / NULLIF(sum(fd.budget_delivered*c.exchange_rate), 0), 0) as ROAS
                       FROM  flights f JOIN campaigns c ON c.id = f.campaign_id
                       JOIN flights_daily_metrics fd ON f.id = fd.flight_id
                       WHERE f.name in $CampaignsQ
                       GROUP BY 1, 2, 3, 4
                       ORDER BY 1;")

    }
    DWQuery <- sqlInterpolate(con, FCInfo)
    FCInfo  <- dbGetQuery(con, DWQuery)

    CurrentPerfTable <- data.frame(matrix(ncol = 7, nrow = as.matrix(dim(FCInfo))[1, 1]))

    CurrentPerfTable[, 1] <- as.data.frame(FCInfo [, 1])
    CurrentPerfTable[, 2] <- as.data.frame(FCInfo [, 2])
    CurrentPerfTable[, 3] <- as.data.frame(FCInfo [, 3])
    CurrentPerfTable[, 4] <- as.data.frame(FCInfo [, 4])
    CurrentPerfTable[, 5] <- as.data.frame(FCInfo [, 5])


    if (DataLength >= 28){

      ModelData           <- read.zoo(GetDataTable[, 3:8], header = TRUE)
      ep                  <- endpoints(ModelData, on = 'weeks')
      ModelData           <- as.matrix(period.apply(ModelData, ep, colSums))

      PeriodLength        <- 7

      ModelDataLength     <- as.matrix(dim(ModelData))[1, 1]


    }else if (DataLength <28) {

      ModelData           <- read.zoo(GetDataTable[, 3:8], header = TRUE)
      ep                  <- endpoints(ModelData, 'days', k = 3)
      ModelData           <- as.matrix(period.apply(ModelData, ep, colSums))
      # FirstWeekAdjustment <- (3 / (DataLength %% 3))
      PeriodLength        <- 3
      ModelDataLength     <- as.matrix(dim(ModelData))[1, 1]

      # if(FirstWeekAdjustment != Inf){

      #   AdjustedWeek      <- FirstWeekAdjustment * ModelData[1, ]

      #   ModelData[1, ]    <- AdjustedWeek

      # }
    }

    ModelRange          <- 1:(as.matrix(dim(ModelData))[1, 1] - 4)
    SummarizedModels    <- data.frame(matrix(ncol = 9, nrow = 0))
    colnames(ModelData) <- c("Impressions", "Clicks", "Conversions", "Client Revenue", "Budget Delivered")

    CI <- 0.7
    ProjectionModels  <- data.frame(matrix(ncol = 7, nrow = 0))

    #--------------------------------------------------------------------CPA--------------------------------------------------------


    if(GoalType == "CPA"){

      CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 6])
      CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 8])

      colnames(CurrentPerfTable) <- c("Campaign", "Start Date", "End Date","Scheduled Budget", "Budget Delivered", "Conversions", "CPA")

      for (i in ModelRange){

        CurrentData           <- as.data.frame(ModelData[i:ModelDataLength, c(3, 5)])
        CurrentData[, 3]      <- log(CurrentData[, 2])
        colnames(CurrentData) <- c("conversions", "revenue", "lnrevenue")

        AdjustedConvs   <- mean(CurrentData$conversions[CurrentData$revenue > 0])
        AdjustedSpend   <- mean(CurrentData$revenue[CurrentData$revenue > 0])
        AdjustedLNSPend <- log(AdjustedSpend)

        UpdatedRows     <- as.matrix(c(AdjustedConvs, AdjustedSpend, AdjustedLNSPend), ncol = 3, nrow = 1)

        UpdatedRange    <- as.array(which(CurrentData$revenue < 1))

        if (length(UpdatedRange) > 0 ){

          for (j in UpdatedRange){

            CurrentData[j, ] <- t(UpdatedRows)
          }
        }

        CurrentModel          <- lm(conversions ~ lnrevenue, data = CurrentData, na.action = na.exclude)
        PredictedPerformance  <- coef(CurrentModel)[1] + coef(CurrentModel)[2] * log(mean(CurrentData[, 2]))
        ActualPerformance     <- mean(CurrentData[, 1])
        PerformanceAdjustment <- ActualPerformance / PredictedPerformance

        SummarizedModels[i, 1] <- i
        SummarizedModels[i, 2] <- summary(CurrentModel)$r.squared
        SummarizedModels[i, 3] <- coef(CurrentModel)[1]
        SummarizedModels[i, 4] <- coef(CurrentModel)[2]
        SummarizedModels[i, 5] <- summary(CurrentModel)$sigma
        SummarizedModels[i, 6] <- var(CurrentData[, 3]) * (ModelDataLength - i)
        SummarizedModels[i, 7] <- mean(CurrentData[, 3])
        SummarizedModels[i, 8] <- PerformanceAdjustment
        SummarizedModels[i, 9] <- ModelDataLength - i + 1
      }

      colnames(SummarizedModels) <- c("Index", "R-Squared", "Intercept", "Slope", "Res. Std. Error",
                                      "Dev X", "Mean X", "Performance Adjustment", "n")
      SummarizedModels <- SummarizedModels[rev(order(SummarizedModels$`R-Squared`)), ]


      if (CurrentNew == "New"){

        NewStartDate  <- as.Date(input$NewStartDate)
        NewEndDate    <- as.Date(input$NewEndDate)

        ProjectionPeriods <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength

        for (i in 1:5){

          for (j in 1:4){

            ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1]
            Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
            StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(Scenario[j, 1] / ProjectionPeriods) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))


            ExpectedConvs <- (SummarizedModels[i, 3] + log((Scenario[j, 1] / ProjectionPeriods)) * SummarizedModels[i, 4])
            LowerConvs    <- (ExpectedConvs - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
            UpperConvs    <- (ExpectedConvs + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

            ExpectedConvs <- SummarizedModels[i, 8] * ExpectedConvs
            LowerConvs    <- SummarizedModels[i, 8] * LowerConvs
            UpperConvs    <- SummarizedModels[i, 8] * UpperConvs

            ExpectedConvs <- ProjectionPeriods * ExpectedConvs
            LowerConvs    <- ProjectionPeriods * LowerConvs
            UpperConvs    <- ProjectionPeriods * UpperConvs

            ExpectedCPA   <- Scenario[j, 1] / ExpectedConvs
            LowerCPA      <- Scenario[j, 1] / UpperConvs
            UpperCPA      <- Scenario[j, 1] / LowerConvs

            ProjectionModels[(i - 1) * 4 + j, 2] <- LowerConvs
            ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedConvs
            ProjectionModels[(i - 1) * 4 + j, 4] <- UpperConvs
            ProjectionModels[(i - 1) * 4 + j, 5] <- LowerCPA
            ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedCPA
            ProjectionModels[(i - 1) * 4 + j, 7] <- UpperCPA

          }

        }
      }else if (CurrentNew == "Current (Incremental)"){


        if(IncrementalPeriod == "Start today / end on schedule"){

          # CurrentSpend   <- as.numeric(CurrentPerfTable[CurrentPerfTable$Campaign == Campaigns, 5])
          # CurrentConvs   <- as.numeric(CurrentPerfTable[CurrentPerfTable$Campaign == Campaigns, 6])

          CurrentSpend   <- sum(CurrentPerfTable[, 5])
          CurrentConvs   <- sum(CurrentPerfTable[, 6])

          ScheduledBudget      <- sum(CurrentPerfTable[, 4])
          RemainingBudget      <- ScheduledBudget - CurrentSpend

          #ProjectionPeriods    <- 4
          ProjectionPeriods    <- as.numeric(((as.Date(CurrentPerfTable[1, 3]) - Sys.Date()) + 1) /PeriodLength)

          #Conversion amount to date

          for (i in 1:5){

            for (j in 1:4){

              ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log((Scenario[j, 1] + RemainingBudget) / ProjectionPeriods) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))


              ExpectedConvs <- (SummarizedModels[i, 3] + log(((Scenario[j, 1] + RemainingBudget) / ProjectionPeriods)) * SummarizedModels[i, 4])
              LowerConvs    <- (ExpectedConvs - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperConvs    <- (ExpectedConvs + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

              ExpectedConvs <- SummarizedModels[i, 8] * ExpectedConvs
              LowerConvs    <- SummarizedModels[i, 8] * LowerConvs
              UpperConvs    <- SummarizedModels[i, 8] * UpperConvs

              ExpectedConvs <- ProjectionPeriods * ExpectedConvs
              LowerConvs    <- ProjectionPeriods * LowerConvs
              UpperConvs    <- ProjectionPeriods * UpperConvs

              #Adding in current performance

              ExpectedConvs <- CurrentConvs + ExpectedConvs
              LowerConvs    <- CurrentConvs + LowerConvs
              UpperConvs    <- CurrentConvs + UpperConvs

              TotalBudget   <- CurrentSpend + Scenario[j, 1] + RemainingBudget

              ExpectedCPA   <- TotalBudget / ExpectedConvs
              LowerCPA      <- TotalBudget / UpperConvs
              UpperCPA      <- TotalBudget / LowerConvs

              ProjectionModels[(i - 1) * 4 + j, 2] <- LowerConvs
              ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedConvs
              ProjectionModels[(i - 1) * 4 + j, 4] <- UpperConvs
              ProjectionModels[(i - 1) * 4 + j, 5] <- LowerCPA
              ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedCPA
              ProjectionModels[(i - 1) * 4 + j, 7] <- UpperCPA
            }
          }
        }else if(IncrementalPeriod == "Custom Period"){

          NewStartDate  <- as.Date(input$NewStartDateCustom)
          NewEndDate    <- as.Date(input$NewEndDateCustom)

          ScheduledEndDate     <- as.Date(CurrentPerfTable[1, 3])
          ProjectionModelsP1   <- data.frame(matrix(ncol = 7, nrow = 0))

          if(ScheduledEndDate <= NewStartDate){

            ScheduledStartDate   <- as.Date(CurrentPerfTable[1, 2])
            ScheduledBudget      <- sum(CurrentPerfTable[, 4])
            CurrentSpend         <- sum(CurrentPerfTable[, 5])
            CurrentConvs         <- sum(CurrentPerfTable[, 6])

            RemainingBudget      <- ScheduledBudget - CurrentSpend
            DaysUntilIncremental <- min(as.numeric(as.Date(ScheduledEndDate) - Sys.Date()) + 1, as.numeric(as.Date(NewStartDate) - Sys.Date()) + 1)

            ProjectionPeriods1 <- DaysUntilIncremental / PeriodLength
            ProjectionPeriods2 <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength

            #Conversion amount to date



            for (i in 1:5){

              ProjectionModelsP1[i, 1] <- RemainingBudget
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(RemainingBudget / ProjectionPeriods1) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))


              ExpectedConvsP1 <- (SummarizedModels[i, 3] + log((RemainingBudget / ProjectionPeriods1)) * SummarizedModels[i, 4])
              LowerConvsP1    <- (ExpectedConvsP1 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperConvsP1    <- (ExpectedConvsP1 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

              ExpectedConvsP1 <- SummarizedModels[i, 8] * ExpectedConvsP1
              LowerConvsP1    <- SummarizedModels[i, 8] * LowerConvsP1
              UpperConvsP1    <- SummarizedModels[i, 8] * UpperConvsP1

              ExpectedConvsP1 <- ProjectionPeriods1 * ExpectedConvsP1
              LowerConvsP1    <- ProjectionPeriods1 * LowerConvsP1
              UpperConvsP1    <- ProjectionPeriods1 * UpperConvsP1

              ProjectionModelsP1[i, 2] <- LowerConvsP1
              ProjectionModelsP1[i, 3] <- ExpectedConvsP1
              ProjectionModelsP1[i, 4] <- UpperConvsP1

            }

            for (i in 1:5){

              for(j in 1:4){

                ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
                Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
                StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(Scenario[j, 1] / ProjectionPeriods2) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))


                ExpectedConvsP2 <- (SummarizedModels[i, 3] + log((Scenario[j, 1] / ProjectionPeriods2))
                                    * SummarizedModels[i, 4])

                LowerConvsP2    <- (ExpectedConvsP2 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
                UpperConvsP2    <- (ExpectedConvsP2 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

                ExpectedConvsP2 <- SummarizedModels[i, 8] * ExpectedConvsP2
                LowerConvsP2    <- SummarizedModels[i, 8] * LowerConvsP2
                UpperConvsP2    <- SummarizedModels[i, 8] * UpperConvsP2

                ExpectedConvsP2 <- ProjectionPeriods2 * ExpectedConvsP2
                LowerConvsP2    <- ProjectionPeriods2 * LowerConvsP2
                UpperConvsP2    <- ProjectionPeriods2 * UpperConvsP2

                ExpectedConvs <- ProjectionModelsP1[i, 3] + ExpectedConvsP2
                LowerConvs    <- ProjectionModelsP1[i, 2] + LowerConvsP2
                UpperConvs    <- ProjectionModelsP1[i, 4] + UpperConvsP2

                TotalBudget   <- RemainingBudget + Scenario[j, 1]

                ExpectedCPA   <- TotalBudget / ExpectedConvs
                LowerCPA      <- TotalBudget / UpperConvs
                UpperCPA      <- TotalBudget / LowerConvs

                ProjectionModels[(i - 1) * 4 + j, 2] <- LowerConvs
                ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedConvs
                ProjectionModels[(i - 1) * 4 + j, 4] <- UpperConvs
                ProjectionModels[(i - 1) * 4 + j, 5] <- LowerCPA
                ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedCPA
                ProjectionModels[(i - 1) * 4 + j, 7] <- UpperCPA

              }
            }
          }else if (ScheduledEndDate > NewStartDate){

            ScheduledStartDate   <- as.Date(CurrentPerfTable[1, 2])
            ScheduledBudget      <- sum(CurrentPerfTable[, 4])
            CurrentSpend         <- sum(CurrentPerfTable[, 5])
            CurrentConvs         <- sum(CurrentPerfTable[, 6])

            RemainingBudget      <- ScheduledBudget - CurrentSpend

            AverageDailySpend    <- CurrentSpend / (as.numeric(Sys.Date() - as.Date(ScheduledStartDate)) + 1)
            DaysUntilIncremental <- min(as.numeric(as.Date(ScheduledEndDate) - Sys.Date()) + 1, as.numeric(as.Date(NewStartDate) - Sys.Date()) + 1)

            EstSpendTillInc      <- min((AverageDailySpend * DaysUntilIncremental), RemainingBudget)
            LeftOverBudget       <- ScheduledBudget - CurrentSpend - EstSpendTillInc


            ProjectionPeriods1 <- DaysUntilIncremental / PeriodLength
            ProjectionPeriods2 <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength
            ProjectionModels   <- data.frame(matrix(ncol = 7, nrow = 0))



            for (i in 1:5){

              ProjectionModelsP1[i, 1] <- EstSpendTillInc
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(EstSpendTillInc / ProjectionPeriods1) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

              ExpectedConvsP1 <- (SummarizedModels[i, 3] + log((EstSpendTillInc / ProjectionPeriods1))
                                  * SummarizedModels[i, 4])
              LowerConvsP1    <- (ExpectedConvsP1 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperConvsP1    <- (ExpectedConvsP1 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

              ExpectedConvsP1 <- SummarizedModels[i, 8] * ExpectedConvsP1
              LowerConvsP1    <- SummarizedModels[i, 8] * LowerConvsP1
              UpperConvsP1    <- SummarizedModels[i, 8] * UpperConvsP1

              ExpectedConvsP1 <- ProjectionPeriods1 * ExpectedConvsP1
              LowerConvsP1    <- ProjectionPeriods1 * LowerConvsP1
              UpperConvsP1    <- ProjectionPeriods1 * UpperConvsP1

              ProjectionModelsP1[i, 2] <- LowerConvsP1
              ProjectionModelsP1[i, 3] <- ExpectedConvsP1
              ProjectionModelsP1[i, 4] <- UpperConvsP1

            }

            for (i in 1:5){

              for(j in 1:4){

                ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
                Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
                StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log((Scenario[j, 1] + LeftOverBudget)/ ProjectionPeriods2) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

                ExpectedConvsP2 <- (SummarizedModels[i, 3] + log(((Scenario[j, 1] + LeftOverBudget) / ProjectionPeriods2)) * SummarizedModels[i, 4])
                LowerConvsP2    <- (ExpectedConvsP2 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
                UpperConvsP2    <- (ExpectedConvsP2 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

                ExpectedConvsP2 <- SummarizedModels[i, 8] * ExpectedConvsP2
                LowerConvsP2    <- SummarizedModels[i, 8] * LowerConvsP2
                UpperConvsP2    <- SummarizedModels[i, 8] * UpperConvsP2

                ExpectedConvsP2 <- ProjectionPeriods2 * ExpectedConvsP2
                LowerConvsP2    <- ProjectionPeriods2 * LowerConvsP2
                UpperConvsP2    <- ProjectionPeriods2 * UpperConvsP2

                ExpectedConvs <- ProjectionModelsP1[i, 3] + ExpectedConvsP2
                LowerConvs    <- ProjectionModelsP1[i, 2] + LowerConvsP2
                UpperConvs    <- ProjectionModelsP1[i, 4] + UpperConvsP2

                TotalBudget   <- LeftOverBudget + Scenario[j, 1]

                ExpectedCPA   <- TotalBudget / ExpectedConvs
                LowerCPA      <- TotalBudget / UpperConvs
                UpperCPA      <- TotalBudget / LowerConvs

                ProjectionModels[(i - 1) * 4 + j, 2] <- LowerConvs
                ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedConvs
                ProjectionModels[(i - 1) * 4 + j, 4] <- UpperConvs
                ProjectionModels[(i - 1) * 4 + j, 5] <- LowerCPA
                ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedCPA
                ProjectionModels[(i - 1) * 4 + j, 7] <- UpperCPA

              }
            }
          }
        }
      }
      colnames(ProjectionModels) <- c("Total Budget", "Conv. Low", "Expected Conv",
                                      "Conv. High", "CPA Low", "Expected CPA", "CPA High")
      #------------------------------------------------------------------ROAS---------------------------------------------------------------
    }else if(GoalType == "ROAS"){
browser()
      CurrentPerfTable[, 6] <- as.data.frame(FCInfo [, 7])
      CurrentPerfTable[, 7] <- as.data.frame(FCInfo [, 9])

      colnames(CurrentPerfTable) <- c("Campaign", "Start Date", "End Date", "Scheduled Budget", "Budget Delivered", "Client Revenue", "ROAS")

      for (i in ModelRange){

        CurrentData           <- as.data.frame(ModelData[i:ModelDataLength, c(4, 5)])
        CurrentData[, 3]      <- log(CurrentData[, 2])
        colnames(CurrentData) <- c("ClientRevenue", "revenue", "lnrevenue")

        AdjustedRev     <- mean(CurrentData$ClientRevenue[CurrentData$revenue > 0])
        AdjustedSpend   <- mean(CurrentData$revenue[CurrentData$revenue > 0])
        AdjustedLNSPend <- log(AdjustedSpend)

        UpdatedRows     <- as.matrix(c(AdjustedRev, AdjustedSpend, AdjustedLNSPend), ncol = 3, nrow = 1)

        UpdatedRange    <- as.array(which(CurrentData$revenue < 1))

        if (length(UpdatedRange) > 0 ){
          for (j in UpdatedRange){

            CurrentData[j, ] <- t(UpdatedRows)
          }
        }
        CurrentModel          <- lm(ClientRevenue ~ lnrevenue, data = CurrentData, na.action = na.exclude)
        PredictedPerformance  <- coef(CurrentModel)[1] + coef(CurrentModel)[2] * log(mean(CurrentData[, 2]))
        ActualPerformance     <- mean(CurrentData[, 1])
        PerformanceAdjustment <- ActualPerformance / PredictedPerformance

        SummarizedModels[i, 1] <- i
        SummarizedModels[i, 2] <- summary(CurrentModel)$r.squared
        SummarizedModels[i, 3] <- coef(CurrentModel)[1]
        SummarizedModels[i, 4] <- coef(CurrentModel)[2]
        SummarizedModels[i, 5] <- summary(CurrentModel)$sigma
        SummarizedModels[i, 6] <- var(CurrentData[, 3]) * (ModelDataLength - i)
        SummarizedModels[i, 7] <- mean(CurrentData[, 3])
        SummarizedModels[i, 8] <- PerformanceAdjustment
        SummarizedModels[i, 9] <- ModelDataLength - i + 1

      }

      colnames(SummarizedModels) <- c("Index", "R-Squared", "Intercept", "Slope", "Res. Std. Error",
                                      "Dev X", "Mean X", "Performance Adjustment", "n")
      SummarizedModels <- SummarizedModels[rev(order(SummarizedModels$`R-Squared`)), ]

      if (CurrentNew == "New"){

        NewStartDate  <- as.Date(input$NewStartDate)
        NewEndDate    <- as.Date(input$NewEndDate)

        ProjectionPeriods <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength

        for (i in 1:5){

          for (j in 1:4){


            # ProjectionPeriods    <- as.numeric(((as.Date(CurrentPerfTable[CurrentPerfTable$Campaign == Campaigns, 3]) - Sys.Date()) + 1) /PeriodLength)
            ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1]
            Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
            StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(Scenario[j, 1] / ProjectionPeriods) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))


            ExpectedRev <- (SummarizedModels[i, 3] + log((Scenario[j, 1] / ProjectionPeriods)) * SummarizedModels[i, 4])
            LowerRev    <- (ExpectedRev - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
            UpperRev    <- (ExpectedRev + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

            ExpectedRev <- SummarizedModels[i, 8] * ExpectedRev
            LowerRev    <- SummarizedModels[i, 8] * LowerRev
            UpperRev    <- SummarizedModels[i, 8] * UpperRev

            ExpectedRev <- ProjectionPeriods * ExpectedRev
            LowerRev    <- ProjectionPeriods * LowerRev
            UpperRev    <- ProjectionPeriods * UpperRev

            ExpectedROAS   <- ExpectedRev / Scenario[j, 1]
            LowerROAS      <- LowerRev / Scenario[j, 1]
            UpperROAS      <- UpperRev / Scenario[j, 1]

            ProjectionModels[(i - 1) * 4 + j, 2] <- LowerRev
            ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedRev
            ProjectionModels[(i - 1) * 4 + j, 4] <- UpperRev
            ProjectionModels[(i - 1) * 4 + j, 5] <- LowerROAS
            ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedROAS
            ProjectionModels[(i - 1) * 4 + j, 7] <- UpperROAS

          }
        }
      }else if (CurrentNew == "Current (Incremental)"){

        if(IncrementalPeriod == "Start today / end on schedule"){

            CurrentSpend         <- sum(CurrentPerfTable[, 5])
            CurrentRev           <- sum(CurrentPerfTable[, 6])


          ScheduledBudget      <- sum(CurrentPerfTable[, 4])
          RemainingBudget      <- ScheduledBudget - CurrentSpend

          ProjectionPeriods    <- as.numeric(((as.Date(CurrentPerfTable[1, 3]) - Sys.Date()) + 1) /PeriodLength)
          #ProjectionPeriods    <- 4
          #Conversion amount to date


          for (i in 1:5){

            for (j in 1:4){

              ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log((Scenario[j, 1] + RemainingBudget) / ProjectionPeriods) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

              ExpectedRev <- (SummarizedModels[i, 3] + log(((Scenario[j, 1] + RemainingBudget) / ProjectionPeriods)) * SummarizedModels[i, 4])
              LowerRev    <- (ExpectedRev - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperRev    <- (ExpectedRev + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              ExpectedRev <- SummarizedModels[i, 8] * ExpectedRev
              LowerRev    <- SummarizedModels[i, 8] * LowerRev
              UpperRev    <- SummarizedModels[i, 8] * UpperRev

              ExpectedRev <- ProjectionPeriods * ExpectedRev
              LowerRev    <- ProjectionPeriods * LowerRev
              UpperRev    <- ProjectionPeriods * UpperRev

              #Adding in current performance

              ExpectedRev <- CurrentRev + ExpectedRev
              LowerRev    <- CurrentRev + LowerRev
              UpperRev    <- CurrentRev + UpperRev

              TotalBudget   <- CurrentSpend + Scenario[j, 1] + RemainingBudget

              ExpectedROAS   <- ExpectedRev / TotalBudget
              LowerROAS      <- LowerRev / Scenario[j, 1]
              UpperROAS      <- UpperRev / Scenario[j, 1]

              ProjectionModels[(i - 1) * 4 + j, 2] <- LowerRev
              ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedRev
              ProjectionModels[(i - 1) * 4 + j, 4] <- UpperRev
              ProjectionModels[(i - 1) * 4 + j, 5] <- LowerROAS
              ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedROAS
              ProjectionModels[(i - 1) * 4 + j, 7] <- UpperROAS
            }
          }
        }else if(IncrementalPeriod == "Custom Period"){

          NewStartDate  <- as.Date(input$NewStartDateCustom)
          NewEndDate    <- as.Date(input$NewEndDateCustom)

          ScheduledEndDate     <- as.Date(CurrentPerfTable[1, 3])
          ProjectionModelsP1  <- data.frame(matrix(ncol = 7, nrow = 0))

          if(ScheduledEndDate <= NewStartDate){

            ScheduledStartDate   <- as.Date(CurrentPerfTable[1, 2])
            ScheduledBudget      <- sum(CurrentPerfTable[, 4])
            CurrentSpend         <- sum(CurrentPerfTable[, 5])
            CurrentRev           <- sum(CurrentPerfTable[, 6])

            RemainingBudget      <- ScheduledBudget - CurrentSpend
            DaysUntilIncremental <- min(as.numeric(as.Date(ScheduledEndDate) - Sys.Date()) + 1, as.numeric(as.Date(NewStartDate) - Sys.Date()) + 1)

            ProjectionPeriods1 <- DaysUntilIncremental / PeriodLength
            ProjectionPeriods2 <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength

            #Conversion amount to date



            for (i in 1:5){

              ProjectionModelsP1[i, 1] <- RemainingBudget
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(RemainingBudget / ProjectionPeriods1) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

              ExpectedRevP1 <- (SummarizedModels[i, 3] + log((RemainingBudget / ProjectionPeriods1)) * SummarizedModels[i, 4])
              LowerRevP1    <- (ExpectedRevP1 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperRevP1    <- (ExpectedRevP1 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

              ExpectedRevP1 <- SummarizedModels[i, 8] * ExpectedRevP1
              LowerRevP1    <- SummarizedModels[i, 8] * LowerRevP1
              UpperRevP1    <- SummarizedModels[i, 8] * UpperRevP1

              ExpectedRevP1 <- ProjectionPeriods1 * ExpectedRevP1
              LowerRevP1    <- ProjectionPeriods1 * LowerRevP1
              UpperRevP1    <- ProjectionPeriods1 * UpperRevP1

              ProjectionModelsP1[i, 2] <- LowerRevP1
              ProjectionModelsP1[i, 3] <- ExpectedRevP1
              ProjectionModelsP1[i, 4] <- UpperRevP1

            }

            for (i in 1:5){

              for(j in 1:4){

                ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
                Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
                StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(Scenario[j, 1] / ProjectionPeriods2) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

                ExpectedRevP2 <- (SummarizedModels[i, 3] + log((Scenario[j, 1] / ProjectionPeriods2)) * SummarizedModels[i, 4])
                LowerRevP2    <- (ExpectedRevP2 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
                UpperRevP2    <- (ExpectedRevP2 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

                ExpectedRevP2 <- SummarizedModels[i, 8] * ExpectedRevP2
                LowerRevP2    <- SummarizedModels[i, 8] * LowerRevP2
                UpperRevP2    <- SummarizedModels[i, 8] * UpperRevP2

                ExpectedRevP2 <- ProjectionPeriods2 * ExpectedRevP2
                LowerRevP2    <- ProjectionPeriods2 * LowerRevP2
                UpperRevP2    <- ProjectionPeriods2 * UpperRevP2

                ExpectedRev   <- ProjectionModelsP1[i, 3] + ExpectedRevP2
                LowerRev      <- ProjectionModelsP1[i, 2] + LowerRevP2
                UpperRev      <- ProjectionModelsP1[i, 4] + UpperRevP2

                TotalBudget   <- RemainingBudget + Scenario[j, 1]

                ExpectedROAS  <- ExpectedRev / TotalBudget
                LowerROAS      <- LowerRev / Scenario[j, 1]
                UpperROAS      <- UpperRev / Scenario[j, 1]

                ProjectionModels[(i - 1) * 4 + j, 2] <- LowerRev
                ProjectionModels[(i - 1) * 4 + j, 3] <- ExpectedRev
                ProjectionModels[(i - 1) * 4 + j, 4] <- UpperRev
                ProjectionModels[(i - 1) * 4 + j, 5] <- LowerROAS
                ProjectionModels[(i - 1) * 4 + j, 6] <- ExpectedROAS
                ProjectionModels[(i - 1) * 4 + j, 7] <- UpperROAS

              }
            }
          }else if (ScheduledEndDate > NewStartDate){


            ScheduledStartDate   <- as.Date(CurrentPerfTable[1, 2])
            ScheduledBudget      <- sum(CurrentPerfTable[, 4])
            CurrentSpend         <- sum(CurrentPerfTable[, 5])
            CurrentRev           <- sum(CurrentPerfTable[, 6])

            RemainingBudget      <- ScheduledBudget - CurrentSpend

            AverageDailySpend    <- CurrentSpend / (as.numeric(Sys.Date() - as.Date(ScheduledStartDate)) + 1)
            DaysUntilIncremental <- min(as.numeric(as.Date(ScheduledEndDate) - Sys.Date()) + 1, as.numeric(as.Date(NewStartDate) - Sys.Date()) + 1)

            EstSpendTillInc      <- min((AverageDailySpend * DaysUntilIncremental), RemainingBudget)
            LeftOverBudget       <- ScheduledBudget - CurrentSpend - EstSpendTillInc


            ProjectionPeriods1 <- DaysUntilIncremental / PeriodLength
            ProjectionPeriods2 <- as.numeric(as.Date(NewEndDate) - as.Date(NewStartDate)) / PeriodLength
            ProjectionModels   <- data.frame(matrix(ncol = 7, nrow = 0))

            for (i in 1:5){
              ProjectionModelsP1[i, 1] <- EstSpendTillInc
              Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
              StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log(EstSpendTillInc / ProjectionPeriods1) - SummarizedModels[i, 7]) ** 2) / SummarizedModels[i, 6]))

              ExpectedRevP1 <- (SummarizedModels[i, 3] + log((EstSpendTillInc / ProjectionPeriods1)) * SummarizedModels[i, 4])
              LowerRevP1    <- (ExpectedRevP1 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
              UpperRevP1    <- (ExpectedRevP1 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

              ExpectedRevP1 <- SummarizedModels[i, 8] * ExpectedRevP1
              LowerRevP1    <- SummarizedModels[i, 8] * LowerRevP1
              UpperRevP1    <- SummarizedModels[i, 8] * UpperRevP1

              ExpectedRevP1 <- ProjectionPeriods1 * ExpectedRevP1
              LowerRevP1    <- ProjectionPeriods1 * LowerRevP1
              UpperRevP1    <- ProjectionPeriods1 * UpperRevP1

              ProjectionModelsP1[i, 2] <- LowerRevP1
              ProjectionModelsP1[i, 3] <- ExpectedRevP1
              ProjectionModelsP1[i, 4] <- UpperRevP1


            }

            for (i in 1:5){

              for(j in 1:4){

                ProjectionModels[(i - 1) * 4 + j, 1] <- Scenario[j, 1] + ScheduledBudget
                Critical_T <- qt(1 - ((1 - CI) / 2), SummarizedModels[i, 9])
                StandardError_CI <- sqrt((1 / SummarizedModels[i, 9]) + (((log((Scenario[j, 1] + LeftOverBudget) / ProjectionPeriods2) - SummarizedModels[i, 7])) ** 2 / SummarizedModels[i, 6]))

                ExpectedRevP2 <- (SummarizedModels[i, 3] + log(((Scenario[j, 1] + LeftOverBudget) / ProjectionPeriods2)) * SummarizedModels[i, 4])
                LowerRevP2    <- (ExpectedRevP2 - (Critical_T * SummarizedModels[i, 5] * StandardError_CI))
                UpperRevP2    <- (ExpectedRevP2 + (Critical_T * SummarizedModels[i, 5] * StandardError_CI))

                ExpectedRevP2 <- SummarizedModels[i, 8] * ExpectedRevP2
                LowerRevP2    <- SummarizedModels[i, 8] * LowerRevP2
                UpperRevP2    <- SummarizedModels[i, 8] * UpperRevP2

                ExpectedRevP2 <- ProjectionPeriods2 * ExpectedRevP2
                LowerRevP2    <- ProjectionPeriods2 * LowerRevP2
                UpperRevP2    <- ProjectionPeriods2 * UpperRevP2

                ExpectedRev   <- ProjectionModelsP1[i, 3] + ExpectedRevP2
                LowerRev      <- ProjectionModelsP1[i, 2] + LowerRevP2
                UpperRev      <- ProjectionModelsP1[i, 4] + UpperRevP2

                TotalBudget   <- RemainingBudget + Scenario[j, 1]

                ExpectedROAS  <- ExpectedRev / TotalBudget
                LowerROAS      <- LowerRev / Scenario[j, 1]
                UpperROAS      <- UpperRev / Scenario[j, 1]

                ProjectionModels[(i - 1) * 4 + j, 2] <-  LowerRev
                ProjectionModels[(i - 1) * 4 + j, 3] <-  ExpectedRev
                ProjectionModels[(i - 1) * 4 + j, 4] <-  UpperRev
                ProjectionModels[(i - 1) * 4 + j, 5] <-  LowerROAS
                ProjectionModels[(i - 1) * 4 + j, 6] <-  ExpectedROAS
                ProjectionModels[(i - 1) * 4 + j, 7] <-  UpperROAS

              }
            }
          }
        }
      }



      colnames(ProjectionModels) <- c("Total Budget", "Rev. Low", "Expected Rev.",
                                      "Rev. High", "ROAS Low", "Expected ROAS", "ROAS High")
    }


    #If (ProjectionType == 'New Flight'){


    return(list(ProjectionModels[1:4, ], ProjectionModels[5:8, ], ProjectionModels[9:12, ],
                ProjectionModels[13:16, ], ProjectionModels[17:20, ], SummarizedModels[1:5, ], ModelData))


    })

  #output$table1 <- DT::renderDataTable({ datatable(ProjectionModels) })

  output$table1    <- renderTable({ data()[[1]] })
  output$table2    <- renderTable({ data()[[2]] })
  output$table3    <- renderTable({ data()[[3]] })
  output$table4    <- renderTable({ data()[[4]] })
  output$table5    <- renderTable({ data()[[5]] })

  output$table11   <- renderTable({ data()[[1]] })
  output$table21   <- renderTable({ data()[[2]] })
  output$table31   <- renderTable({ data()[[3]] })
  output$table41   <- renderTable({ data()[[4]] })
  output$table51   <- renderTable({ data()[[5]] })

  output$Summary   <- renderTable({ data()[[6]]})

  output$DataTable <- renderTable({ data()[[7]] })

  }

shinyApp(ui=ui, server=server)



