# ============================================================
# Data Science Tools and Techniques
# Technical Analysis Using R
# Visualization Phase
# ============================================================

# ------------------------------------------------------------
# Install and load required packages
# ------------------------------------------------------------

required_packages <- c(
  "shiny",
  "ggplot2",
  "quantmod"
)

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(shiny)
library(ggplot2)
library(quantmod)


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "Portfolio Technical Analysis Dashboard"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "stock_symbol",
        "Stock Symbol:",
        value = "AAPL"
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c(
          "Daily" = "daily",
          "Weekly" = "weekly",
          "Monthly" = "monthly"
        ),
        selected = "daily"
      ),
      
      selectInput(
        "chart_type",
        "Chart Type:",
        choices = c(
          "Line Chart" = "line",
          "Area Chart" = "area",
          "Candlestick Chart" = "candlestick"
        ),
        selected = "line"
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = "Moving Averages"
      ),
      
      checkboxInput(
        "show_signals",
        "Show Buy/Sell Signals",
        value = TRUE
      ),
      
      actionButton(
        "update",
        "Update Chart"
      )
    ),
    
    mainPanel(
      
      h3("Stock Price Chart"),
      
      plotOutput(
        "stock_chart",
        height = "600px"
      ),
      
      hr(),
      
      h3("RSI"),
      
      plotOutput(
        "rsi_chart",
        height = "250px"
      ),
      
      hr(),
      
      h3("MACD"),
      
      plotOutput(
        "macd_chart",
        height = "250px"
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Fetch stock data from Yahoo Finance
  # ----------------------------------------------------------
  
  stock_data <- eventReactive(
    input$update,
    {
      
      req(input$stock_symbol)
      req(input$date_range)
      
      tryCatch({
        
        data <- getSymbols(
          input$stock_symbol,
          src = "yahoo",
          from = input$date_range[1],
          to = input$date_range[2],
          auto.assign = FALSE
        )
        
        data
        
      }, error = function(e) {
        
        showNotification(
          paste(
            "Unable to retrieve stock data:",
            e$message
          ),
          type = "error"
        )
        
        NULL
      })
    },
    ignoreNULL = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Prepare stock data
  # ----------------------------------------------------------
  
  stock_dataframe <- reactive({
    
    data <- stock_data()
    
    req(data)
    
    df <- data.frame(
      Date = index(data),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data)),
      Volume = as.numeric(Vo(data))
    )
    
    df
    
  })
  
  
  # ==========================================================
  # MAIN STOCK PRICE CHART
  # ==========================================================
  
  output$stock_chart <- renderPlot({
    
    df <- stock_dataframe()
    
    req(nrow(df) > 0)
    
    
    # --------------------------------------------------------
    # Calculate Moving Averages
    # --------------------------------------------------------
    
    df$MA20 <- SMA(
      df$Close,
      n = 20
    )
    
    df$MA50 <- SMA(
      df$Close,
      n = 50
    )
    
    
    # --------------------------------------------------------
    # Generate trading signals
    # --------------------------------------------------------
    
    df$Signal <- "Hold"
    
    for (i in 2:nrow(df)) {
      
      if (
        !is.na(df$MA20[i]) &&
        !is.na(df$MA50[i]) &&
        !is.na(df$MA20[i - 1]) &&
        !is.na(df$MA50[i - 1])
      ) {
        
        # Moving average crossover = BUY
        
        if (
          df$MA20[i] > df$MA50[i] &&
          df$MA20[i - 1] <= df$MA50[i - 1]
        ) {
          
          df$Signal[i] <- "Buy"
          
        }
        
        # Moving average crossunder = SELL
        
        else if (
          df$MA20[i] < df$MA50[i] &&
          df$MA20[i - 1] >= df$MA50[i - 1]
        ) {
          
          df$Signal[i] <- "Sell"
          
        }
      }
    }
    
    
    # --------------------------------------------------------
    # Create base chart
    # --------------------------------------------------------
    
    if (input$chart_type == "line") {
      
      p <- ggplot(
        df,
        aes(
          x = Date,
          y = Close
        )
      ) +
        
        geom_line(
          color = "steelblue",
          linewidth = 1
        )
    }
    
    
    # --------------------------------------------------------
    # Area chart
    # --------------------------------------------------------
    
    else if (input$chart_type == "area") {
      
      p <- ggplot(
        df,
        aes(
          x = Date,
          y = Close
        )
      ) +
        
        geom_area(
          fill = "steelblue",
          alpha = 0.35
        ) +
        
        geom_line(
          color = "steelblue",
          linewidth = 1
        )
    }
    
    
    # --------------------------------------------------------
    # Candlestick chart
    # --------------------------------------------------------
    
    else {
      
      df$Direction <- ifelse(
        df$Close >= df$Open,
        "Up",
        "Down"
      )
      
      p <- ggplot(
        df,
        aes(x = Date)
      ) +
        
        geom_segment(
          aes(
            xend = Date,
            y = Low,
            yend = High
          ),
          color = "black"
        ) +
        
        geom_linerange(
          aes(
            ymin = Open,
            ymax = Close,
            color = Direction
          ),
          linewidth = 4
        ) +
        
        scale_color_manual(
          values = c(
            "Up" = "forestgreen",
            "Down" = "red"
          )
        )
    }
    
    
    # --------------------------------------------------------
    # Add Moving Averages
    # --------------------------------------------------------
    
    if (
      "Moving Averages" %in%
      input$technical_indicators
    ) {
      
      p <- p +
        
        geom_line(
          aes(y = MA20),
          color = "orange",
          linewidth = 0.9,
          na.rm = TRUE
        ) +
        
        geom_line(
          aes(y = MA50),
          color = "purple",
          linewidth = 0.9,
          na.rm = TRUE
        )
    }
    
    
    # --------------------------------------------------------
    # Add BUY annotations
    # --------------------------------------------------------
    
    if (input$show_signals) {
      
      buy_data <- df[
        df$Signal == "Buy",
      ]
      
      sell_data <- df[
        df$Signal == "Sell",
      ]
      
      
      if (nrow(buy_data) > 0) {
        
        p <- p +
          
          geom_point(
            data = buy_data,
            aes(
              x = Date,
              y = Close
            ),
            color = "green",
            size = 4,
            shape = 24,
            fill = "green"
          ) +
          
          geom_text(
            data = buy_data,
            aes(
              x = Date,
              y = Close,
              label = "BUY"
            ),
            color = "green",
            vjust = -1.2,
            fontface = "bold"
          )
      }
      
      
      if (nrow(sell_data) > 0) {
        
        p <- p +
          
          geom_point(
            data = sell_data,
            aes(
              x = Date,
              y = Close
            ),
            color = "red",
            size = 4,
            shape = 25,
            fill = "red"
          ) +
          
          geom_text(
            data = sell_data,
            aes(
              x = Date,
              y = Close,
              label = "SELL"
            ),
            color = "red",
            vjust = 1.8,
            fontface = "bold"
          )
      }
    }
    
    
    # --------------------------------------------------------
    # Chart formatting
    # --------------------------------------------------------
    
    p <- p +
      
      labs(
        title = paste(
          input$stock_symbol,
          "Technical Analysis"
        ),
        x = "Date",
        y = "Price"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          size = 18,
          face = "bold"
        ),
        legend.position = "bottom"
      )
    
    
    print(p)
    
  })
  
  
  # ==========================================================
  # RSI CHART
  # ==========================================================
  
  output$rsi_chart <- renderPlot({
    
    req("RSI" %in% input$technical_indicators)
    
    df <- stock_dataframe()
    
    req(df)
    
    # Calculate RSI
    rsi_values <- RSI(
      df$Close,
      n = 14
    )
    
    df$RSI <- as.numeric(rsi_values)
    
    p <- ggplot(
      df,
      aes(
        x = Date,
        y = RSI
      )
    ) +
      
      geom_line(
        color = "darkblue",
        linewidth = 1
      ) +
      
      geom_hline(
        yintercept = 70,
        color = "red",
        linetype = "dashed"
      ) +
      
      geom_hline(
        yintercept = 30,
        color = "green",
        linetype = "dashed"
      ) +
      
      labs(
        title = "Relative Strength Index (RSI)",
        x = "Date",
        y = "RSI"
      ) +
      
      coord_cartesian(
        ylim = c(0, 100)
      ) +
      
      theme_minimal()
    
    print(p)
    
  })
  
  
  
  # ==========================================================
  # MACD CHART
  # ==========================================================
  
  output$macd_chart <- renderPlot({
    
    req("MACD" %in% input$technical_indicators)
    
    df <- stock_dataframe()
    
    req(df)
    
    # Calculate MACD
    macd_values <- MACD(
      df$Close,
      nFast = 14,
      nSlow = 25,
      nSig = 7,
      maType = "EMA"
    )
    
    macd_df <- data.frame(
      Date = df$Date,
      MACD = macd_values[, 1],
      Signal = macd_values[, 2]
    )
    
    macd_df$Histogram <-
      macd_df$MACD -
      macd_df$Signal
    
    p <- ggplot(
      macd_df,
      aes(x = Date)
    ) +
      
      geom_col(
        aes(y = Histogram),
        fill = "gray",
        alpha = 0.7
      ) +
      
      geom_line(
        aes(
          y = MACD,
          color = "MACD"
        ),
        linewidth = 1
      ) +
      
      geom_line(
        aes(
          y = Signal,
          color = "Signal"
        ),
        linewidth = 1
      ) +
      
      scale_color_manual(
        values = c(
          "MACD" = "blue",
          "Signal" = "red"
        )
      ) +
      
      geom_hline(
        yintercept = 0,
        color = "black"
      ) +
      
      labs(
        title = "Moving Average Convergence Divergence (MACD)",
        x = "Date",
        y = "Value",
        color = "Line"
      ) +
      
      theme_minimal()
    
    print(p)
    
  })
  
  
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)