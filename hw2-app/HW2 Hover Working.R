library(shiny)
library(reshape)
library(plyr)
library(ggplot2)
library(dplyr)

setwd("~/Documents/Data Visualization/Better HW2")

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
    #sliderInput("pop", "Population:",
    #            min = 1, 10, value = 3),
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
    plotOutput("plot1", hover = hoverOpts(id ="plot_hover")),
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
  #reactiveData <- reactive({
  #  full %>% filter(c(Year == input$year, Region == input$region, 
  #                    Country == input$country, Life == input$life,
  #                    Fertility == input$fertility, Population == input$population)) 
  #})
  
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
  
  output$plot1 <- renderPlot({
    data <- reactiveData()
    
    theme1 <- theme(
      axis.text = element_text(size=13),
      legend.position = "right",
      #legend.key.size = unit(3,"point"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      plot.title = element_text(lineheight=.8, face="bold"),
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )
    plot <- ggplot(data, aes(x= Life, y= Fertility, fill = Region)) + 
      geom_point(aes(size = Population), alpha = 0.3) +
      geom_point(colour="black", aes(size = Population), shape = 21) +
      guides(shape = 22) +
      theme_bw() + ylab("Fertility rate") + xlab("Life expectancy") +
      theme1 + scale_x_continuous(limits = c(10,90), breaks = seq(10,95, by = 10)) + 
      scale_y_continuous(limits = c(0.5,9), breaks = seq(1,9, by = 1)) +
      scale_fill_manual(values=colors) + 
      guides(shape=guide_legend(override.aes=list(size=10)))
    plot
  })
  
  output$info <- renderText({""})
  
  observeEvent(input$plot_hover, {
    data <- reactiveData()
    
    hover_data <- nearPoints(data, input$plot_hover) %>% 
      mutate(label = paste(Country))
    
    hover_summary <- paste0("Country: ", hover_data$label,
                            "\n Life Expectancy: ", round(input$plot_hover$x, 2), 
                            "\n Fertility Rate: ", round(input$plot_hover$y, 2))#,
                            #"\n Population: ", data[grep(hover_data$label, Country), Population])
    
    output$plot1 <- renderPlot({
      data <- reactiveData()
      
      theme1 <- theme(
        axis.text = element_text(size=13),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(lineheight=.8, face="bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank()
      )
      plot <- ggplot(data, aes(x= Life, y= Fertility, fill = Region)) + 
        geom_point(aes(size = Population), alpha = 0.3) +
        geom_point(colour="black", aes(size = Population), shape = 21) +
        guides(shape = 22) +
        theme_bw() + ylab("Fertility rate") + xlab("Life expectancy") +
        theme1 + scale_x_continuous(limits = c(10,90), breaks = seq(10,95, by = 10)) + 
        scale_y_continuous(limits = c(0.5,9), breaks = seq(1,9, by = 1)) +
        scale_fill_manual(values=colors) + 
        guides(shape=guide_legend(override.aes=list(size=10)))
      
      if(nrow(hover_data) == 1) {
        plot +
          geom_label(data = hover_data, aes(label = label), nudge_x = 0.2)
      } else plot
    })
    
    output$info <- renderText({
      if(nrow(hover_data) == 1) {
        paste("Summary: \n", hover_summary)
      } else ""
    })
  })
}

#shinyApp(ui = ui, server = server)

#shiny::runGitHub("class-code", "usfviz", subdir = "intro-shiny/ggvis-app-1")
#shiny::runGitHub("mapalmer2-hw2", "usfviz", subdir = "HW2 Hover Working.R")
