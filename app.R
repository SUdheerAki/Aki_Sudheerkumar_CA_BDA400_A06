# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Sudheer Kumar Aki
# Run: install.packages(c("shiny","ggplot2","quantmod","TTR","zoo"))
# Then open this file in RStudio and click Run App.

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(zoo)

# TASK 1: Yahoo Finance data, historical warm-up, and error handling
fetch_prices <- function(symbol, from, to) {
  validate(need(grepl("^[A-Za-z0-9.^_-]{1,18}$", symbol), "Invalid ticker symbol"))
  validate(need(from < to, "Start date must be before end date"))
  tryCatch({
    x <- suppressWarnings(getSymbols(toupper(symbol), src="yahoo",
      from=as.Date(from)-400, to=as.Date(to)+1, auto.assign=FALSE))
    x <- na.omit(x)
    validate(need(NROW(x)>55, "Insufficient historical stock records"))
    x
  }, error=function(e) {
    validate(need(FALSE, paste("Yahoo Finance retrieval failed:",conditionMessage(e))))
  })
}

# TASK 2: Daily, weekly, monthly OHLC aggregation
resample_prices <- function(x, frequency) {
  if (frequency=="Daily") return(x)
  if (frequency=="Weekly") return(to.weekly(x, indexAt="lastof", drop.time=TRUE))
  to.monthly(x, indexAt="lastof", drop.time=TRUE)
}

# TASK 3: Technical indicators and MA crossover trading rule
compute_metrics <- function(x, short_n, long_n, rsi_n, fast_n, slow_n, signal_n) {
  close <- as.numeric(Cl(x))
  d <- data.frame(date=as.Date(index(x)),open=as.numeric(Op(x)),
    high=as.numeric(Hi(x)),low=as.numeric(Lo(x)),close=close)
  d$short_ma <- as.numeric(SMA(close, n=short_n))
  d$long_ma <- as.numeric(SMA(close, n=long_n))
  d$rsi <- as.numeric(RSI(close, n=rsi_n))
  m <- MACD(close,nFast=fast_n,nSlow=slow_n,nSig=signal_n,maType="EMA",percent=FALSE)
  d$macd <- as.numeric(m[,1])
  d$macd_signal <- as.numeric(m[,2])
  d$histogram <- d$macd-d$macd_signal
  above <- d$short_ma > d$long_ma
  previous <- c(NA,head(above,-1))
  d$signal <- ifelse(!is.na(above) & !is.na(previous) & above & !previous,
    "Buy",ifelse(!is.na(above) & !is.na(previous) & !above & previous,"Sell","Hold"))
  d
}

# TASK 4: Shiny interface and interactive controls
ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard | BDA400 A06"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol","Yahoo Finance ticker","AAPL"),
      dateRangeInput("dates","Date range",start=Sys.Date()-730,end=Sys.Date()),
      selectInput("frequency","Time frame",c("Daily","Weekly","Monthly")),
      selectInput("style","Chart type",c("Line","Candlestick","Area")),
      checkboxGroupInput("overlays","Technical indicators",
        c("Moving Averages","RSI","MACD"),selected=c("Moving Averages","RSI","MACD")),
      checkboxInput("show_signals","Annotate Buy/Sell signals",TRUE),
      h4("Adjustable technical settings"),
      numericInput("short_n","Short SMA",20,min=2,max=200),
      numericInput("long_n","Long SMA",50,min=3,max=300),
      numericInput("rsi_n","RSI period",14,min=2,max=100),
      numericInput("fast_n","MACD fast EMA",12,min=2,max=100),
      numericInput("slow_n","MACD slow EMA",26,min=3,max=200),
      numericInput("signal_n","MACD signal EMA",9,min=2,max=100),
      actionButton("refresh","Refresh Yahoo Finance data"),
      helpText("Education only; moving-average crossover signals are not financial advice.")
    ),
    mainPanel(
      h4(textOutput("summary")),
      plotOutput("stock_chart",height="430px"),
      conditionalPanel("input.overlays.includes('RSI')",plotOutput("rsi_chart",height="200px")),
      conditionalPanel("input.overlays.includes('MACD')",plotOutput("macd_chart",height="230px")),
      h4("Latest Buy/Sell crossovers"),
      tableOutput("signal_table"),
      downloadButton("export","Download analysis CSV")
    )
  )
)

# TASK 5: Reactive data, visualization overlays and annotations
server <- function(input,output,session) {
  raw_prices <- eventReactive(input$refresh,{
    req(input$dates)
    fetch_prices(trimws(input$symbol),input$dates[1],input$dates[2])
  },ignoreNULL=FALSE)

  analysed <- reactive({
    req(raw_prices(),input$dates)
    validate(
      need(input$short_n<input$long_n,"Short SMA must be less than long SMA"),
      need(input$fast_n<input$slow_n,"MACD fast EMA must be less than slow EMA")
    )
    x <- resample_prices(raw_prices(),input$frequency)
    validate(need(NROW(x)>max(input$long_n,input$slow_n+input$signal_n,input$rsi_n)+2,
      "Not enough bars; select Daily or reduce indicator periods"))
    d <- compute_metrics(x,input$short_n,input$long_n,input$rsi_n,
                         input$fast_n,input$slow_n,input$signal_n)
    d[d$date>=as.Date(input$dates[1]) & d$date<=as.Date(input$dates[2]),,drop=FALSE]
  })

  output$summary <- renderText({
    d <- analysed()
    validate(need(nrow(d)>0,"No prices in selected range"))
    s <- d$signal[d$signal!="Hold"]
    paste(toupper(input$symbol),"|",nrow(d),"bars | Latest close:",
      sprintf("$%.2f",tail(d$close,1)),"| Latest crossover:",
      if(length(s)) tail(s,1) else "None in selected range")
  })

  output$stock_chart <- renderPlot({
    d <- analysed()
    validate(need(nrow(d)>0,"No prices in selected range"))
    p <- ggplot(d,aes(x=date)) +
      labs(title=paste(toupper(input$symbol),"Stock Price & Crossover Signals"),
           x="Date",y="USD",caption="Yahoo Finance | SMA crossover demonstration") +
      theme_minimal()
    if(input$style=="Line")
      p <- p+geom_line(aes(y=close),color="#274c77",linewidth=.7)
    if(input$style=="Area")
      p <- p+geom_area(aes(y=close),fill="#8ecae6",alpha=.45) +
        geom_line(aes(y=close),color="#274c77")
    if(input$style=="Candlestick") {
      d$direction <- ifelse(d$close>=d$open,"Up","Down")
      w <- if(input$frequency=="Monthly") 13 else if(input$frequency=="Weekly") 3 else .45
      p <- p+geom_linerange(data=d,aes(ymin=low,ymax=high,color=direction))+
        geom_rect(data=d,aes(xmin=date-w,xmax=date+w,
          ymin=pmin(open,close),ymax=pmax(open,close),fill=direction),alpha=.85)+
        scale_fill_manual(values=c(Up="#198754",Down="#c0392b"))
      if(!("Moving Averages" %in% input$overlays))
        p <- p+scale_color_manual(values=c(Up="#198754",Down="#c0392b"))
    }
    if("Moving Averages" %in% input$overlays) {
      # Fixed direct colors keep the candlestick's up/down colours independent
      # of the moving-average legend, avoiding conflicting color scales.
      p <- p+geom_line(aes(y=short_ma),color="#e69f00",linewidth=.8,na.rm=TRUE)+
        geom_line(aes(y=long_ma),color="#6f42c1",linewidth=.8,na.rm=TRUE)
      if(input$style=="Candlestick") p <- p+scale_color_manual(values=c(Up="#198754",Down="#c0392b"))
    }
    if(isTRUE(input$show_signals)) {
      s <- d[d$signal %in% c("Buy","Sell"),,drop=FALSE]
      if(nrow(s)>0) p <- p+
        geom_point(data=s,aes(y=close,shape=signal),size=3,color="black")+
        geom_text(data=s,aes(y=close,label=signal),vjust=-1.1,size=3,check_overlap=TRUE)
    }
    print(p)
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$overlays)
    ggplot(analysed(),aes(date,rsi))+
      geom_line(color="#7b2cbf",na.rm=TRUE)+
      geom_hline(yintercept=c(30,70),linetype="dashed",color="#777777")+
      coord_cartesian(ylim=c(0,100))+
      labs(title="Relative Strength Index",x="Date",y="RSI") + theme_minimal()
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$overlays)
    ggplot(analysed(),aes(date))+
      geom_col(aes(y=histogram),fill="#9ecae1",na.rm=TRUE)+
      geom_line(aes(y=macd,color="MACD"),na.rm=TRUE)+
      geom_line(aes(y=macd_signal,color="Signal"),na.rm=TRUE)+
      scale_color_manual(values=c(MACD="#1d3557",Signal="#e76f51"))+
      labs(title="MACD & Signal Line",x="Date",y="MACD",color="Series")+theme_minimal()
  })

  output$signal_table <- renderTable({
    d <- analysed()
    s <- d[d$signal!="Hold",c("date","close","short_ma","long_ma","signal"),drop=FALSE]
    if(nrow(s)==0) return(data.frame(Message="No crossovers in selected range"))
    tail(s,10)
  },digits=2)

  output$export <- downloadHandler(
    filename=function() paste0(toupper(input$symbol),"_analysis.csv"),
    content=function(file) write.csv(analysed(),file,row.names=FALSE)
  )
}
shinyApp(ui,server)
