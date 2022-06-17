library(shiny)
library(forecast)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sistema de Análise e Previsão de Séries Temporais"),
    
    fluidRow(
        column(4,fileInput("arquivo","Escolha o arquivo:",multiple = F,accept = c(".csv")),
               helpText("Observação: O arquivo deve conter apenas uma coluna, sem nome no cabeçalho. A frequência deve ser mensal")),
        column(4,dateRangeInput("datas",label="Período da série",format="mm/yyyy",
                                language="pt",start="2000-01-01",end="2013-12-31",
                                startview = "year", separator = " até "),
               helpText("Observação: Para definir mês e ano, selecione um dia qualquer")),
        column(4,numericInput("PeriodoPrevisao","Informe quantos meses quer prever:",
                              12, min = 1, max = 48),
               actionButton("Processar","Processar"))
    ),
    fluidRow(
        column(6,plotOutput("GrafSerie")),
        column(6,plotOutput("GrafHist"))
    ),
    fluidRow(
        column(6,plotOutput("GrafBox")),
        column(6,plotOutput("GrafDec"))
    ),
    hr(),
    fluidRow(
        column(6,plotOutput("GrafPrev")),
        column(2,h1(textOutput("llower")),tableOutput("lower")),
        column(2,h1(textOutput("lmean")),tableOutput("mean")),
        column(2,h1(textOutput("lupper")),tableOutput("upper"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$Processar, {
        file1 = input$arquivo
        data = read.csv(file1$datapath,header=F)
        anoinic = as.integer(substr(input$datas[1],1,4))
        mesinic = as.integer(substr(input$datas[1],6,7))
        anofim = as.integer(substr(input$datas[2],1,4))
        mesfim = as.integer(substr(input$datas[2],6,7))
        
        data = ts(data,start=c(anoinic,mesinic),end=c(anofim,mesfim),
                  frequency=12)
        
        output$GrafSerie = renderPlot({ autoplot(data,main="Série Original") })
        output$GrafHist = renderPlot({ hist(data,main="Histograma dos dados") })
        output$GrafBox = renderPlot({ boxplot(data,main="Boxplot dos dados") })
        dec = decompose(data)
        output$GrafDec = renderPlot({ autoplot(dec,main="Decomposição") })
        
        modelo = auto.arima(data)
        valr = input$PeriodoPrevisao
        previsao = forecast(modelo,h=valr)
        
        output$lower = renderTable({previsao$lower})
        output$mean = renderTable({previsao$mean})
        output$upper = renderTable({previsao$upper})
        
        output$llower = renderText({"Lower"})
        output$lmean = renderText({"Mean"})
        output$lupper = renderText({"Upper"})
        
        output$GrafPrev = renderPlot({ autoplot(previsao,main="Previsão da série") })
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
