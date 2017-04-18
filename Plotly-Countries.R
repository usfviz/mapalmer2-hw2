require(shiny)
require(reshape)
require(plyr)
require(ggplot2)
require(dplyr)
require(plotly)

pdf(NULL)

life <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv",skip=4, header=T)
life <- life[,-c(2,3,4,60,61,62)]
colnames(life) <- c("Country", seq(1960, 2014))

fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv",skip=4, header=T)
fertility <- fertility[,-c(2,3,4,60,61,62)]
colnames(fertility) <- c("Country", seq(1960, 2014))

pop <- read.csv("API_SP.POP.1564.TO.ZS_DS2_en_csv_v2.csv",skip=4, header=T)
pop <- pop[,-c(2,3,4,60,61,62)]
colnames(pop) <- c("Country", seq(1960, 2014))

regions <- read.csv("Metadata_Country.csv", na.strings = "")
regions <- regions %>% select(TableName, Region)
colnames(regions) <- c("Country", "Region")


l <- melt(life)
colnames(l) <- c("Country", "Year", "Life")

f <- melt(fertility)
colnames(f) <- c("Country", "Year", "Fertility")

p <- melt(pop)
colnames(p) <- c("Country", "Year", "Population")

full <- merge(l, f, by = c("Country", "Year"), all = TRUE)
full <- merge(full, p, by = c("Country", "Year"), all = TRUE)
full <- merge(x = full, y = regions, by = "Country", all.x = TRUE)


full <- na.omit(full)


ui <- fluidPage(
  headerPanel("Countries"),
  sidebarPanel(  
    selectInput("reg", "Region:",
                choices = c(" ",
                            "South Asia",
                            "Europe & Central Asia",
                            "Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Latin American and Caribbean",
                            "East Asia & Pacific",
                            "North America"))),
  
  mainPanel(
    plotlyOutput("plot1"),
    uiOutput("dynamic"),
    sliderInput("year", "Year",
                min = 1960, max = 2014,
                value = 1960, 
                animate = animationOptions(loop = FALSE, interval = 700), sep="", step = 1,
                width = "10cm", ticks = FALSE),
    verbatimTextOutput("info")
  )
)


server <- function(input, output) {
  regData <- reactive({
    switch(input$reg, 
           " " = full,
           "South Asia" = full[grep("South Asia", full$Region), ],
           "Europe & Central Asia" = full[grep("Europe & Central Asia", full$Region), ],
           "Middle East & North Africa" = full[grep("Middle East & North Africa", full$Region), ],
           "Sub-Saharan Africa" = full[grep("Sub-Saharan Africa", full$Region), ],
           "Latin American and Caribbean" = full[grep("Latin American and Caribbean", full$Region), ],
           "East Asia & Pacific" = full[grep("East Asia & Pacific", full$Region), ],
           "North America" = full[grep("North America", full$Region), ])
  })
  
  reactiveData <- reactive({
    regData() %>% filter(c(Year == input$year, 
                           Country == input$country, Life == input$life,
                           Fertility == input$fertility, Population == input$population)) 
  })

  colors <- c("deeppink1", "mediumblue", "red2", "orange", "forestgreen", "darkmagenta", "cyan2")
  
  output$plot1 <- renderPlotly({
    data <- reactiveData()
    
    p <- plot_ly(data, x = ~Life, y = ~Fertility, 
                 color = ~Region, size = ~Population, 
                 colors = colors,
             type = 'scatter', mode = 'markers', sizes = c(7, 
                                                           35),
             marker = list(symbol = 'circle', sizemode = 'diameter',
                           line = list(width = 2, color = '#FFFFFF')),
             text = ~paste('Country:', Country, '<br>Life Expectancy:', 
                           Life, '<br>Fertility:', Fertility,
                           '<br>Pop.:', Population)) %>%
  layout(title = ' ',
         xaxis = list(title = 'Life Expectancy',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(10, 90),
                      #type = 'log',
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwidth = 2),
         yaxis = list(title = 'Fertility Rate',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 9),
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(255, 255, 255)',
         plot_bgcolor = 'rgb(255, 255, 255)')
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
