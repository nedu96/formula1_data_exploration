#----------------------------------------------------PROJECT: Formula 1------------------------------------------------------------
#----------------------------------------------------AUTHOR: Nedunchezia Pandia Rajan(31251528)-------------------------------------------------------------------
#----------------------------------------------------CREATE DATE: 01-06-2021-------------------------------------------------------------------
#----------------------------------------------------LAST MODIFIED: 09-06-2021-----------------------------------------------------------
#----------------------------------------------------MODIFICATION: DOCUMENTATION---------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------Libraries-------------------------------------------------------------------

#This library is used to develop and build interactive web application using R
library(shiny)


#this library is used to build data vizualization for plotting graph
#install.packages("ggplot2")
library(ggplot2)

#this libraty is used for data formatting
#install.packages("dplyr")
library(dplyr)

#this library is used add aesthetics and also to graph plotted using ggplot
#install.packages("ggthemes")
library(ggthemes)

#This is used in identifying different color panels
#install.packages("RColorBrewer")
library(RColorBrewer)

#this library provides circle options
#install.packages("circlize")
library(circlize)

#this package is used in places of colorblindness
#install.packages("viridis")
library(viridis)

#this package provide different themes  through which it os being done
#install.packages("shinythemes")
library(shinythemes)


#this library i used in statistical analysis
#install.packages("HH")
library(HH)

#this library is used to identify plots
#install.packages("plotly")
library(plotly)

#this library is used in accesing table content
#install.packages("data.table")
library(data.table)

#this library helps in providing alert message  at the beginning
#install.packages('shinyalert')
library(shinyalert)

#this package helps in the use of symbols for compilation
#install.packages("magrittr")
library(magrittr) 

#this package is used in implementing different widgets
#install.packages("shinyWidgets")
library(shinyWidgets)


#--------------------------------------------------Set Directory-------------------------------------------------------------------

#current directory is identified to check the working directory
getwd()

#directory moved in required format and changed as current working directory
setwd("project_files")

#--------------------------------------------------Read required files-------------------------------------------------------------------

#required data for driver graph and plot are loaded into driver_data
drivers_data<-read.csv('drivers_data.csv',stringsAsFactors=F)

#driver data are stored in temp_data variable and are filtred
temp_val<-data.frame(
  drivers_data %>% filter(positionOrder== 1) %>% group_by(name, driverRef) %>%  summarize(count=n()) %>% filter(count>5) %>% na.omit())

#random colors are received to implement with chord diagram
color_panel <- c('#5D3A9B',
                 '#f0e442',
                 '#4B0092',
                 '#1AFF1A',
                 "#006400",
                 '#E66100',
                 '#E1BE6A',
                 '#40B0A6',
                 '#1E90FF',
                 '#D35FB7',
                 '#FEFE62',
                 "#006CD1",
                 "#0C7BDC",
                 '#005AB5',
                 '#D41159',
                 "gray50",
                 "#FFC20A",
                 "#994F00",
                 '#DC3220',
                 '#1A85FF')

#unique driver names are sorted and are stored in driver names variable
drivers_names<-sort(unique(temp_val$name))




#vales that are required are stored as in cal_value variable
col_value<-data.frame(name = drivers_names,color_panel)



#densiity plot values are retrieved from csv  file
density_data <- read.csv('circuit_details.csv',stringsAsFactors = F)


#plot data are received from plot_data function and ar stored in  plot table
plot_data = read.csv('filter_drivers.csv',stringsAsFactors = F)
plot_table <- data.table(plot_data)

#chord diagram to display the acquired value from data frame
chord_data<-data.frame(left_join(temp_val, col_value, by='name'))

#data for generating barpot is retrieved
bar_plot<-read.csv('driver_wins.csv',sep=',',stringsAsFactors=F)

#data used in the last tab for trend analysis is used
trend_data<-read.csv('pit_stops.csv',stringsAsFactors=F)

#-------------------------------------------------UI-------------------------------------------------------------------

#user interface screen for the shiny dash board part is used in here
#reference: https://shiny.rstudio.com/reference/shiny/1.5.0/
ui <- navbarPage("FORMULA ONE - Drive to Survive",
                 theme = shinytheme("slate"), 
                 tabPanel("Drivers and Constructors",
                          useShinyalert(),
                          sidebarLayout(position = "right",
                                        #side Panel details and instruction for the use of graph
                            sidebarPanel(h6(strong("USER MANUAL")),
                                         p("Select the driver that you want to compare"),
                                         p("Sebastian Vettel and Lewis Hamilton are chosen as default drivers"),
                                         p("You can also Select other drivers to compare "),
                                          p("You can use the mouse pointer over bar for more details"),
                                         
#data inputs are manually entered for the dropdown present 

selectInput(inputId = 'drivers',
            label = 'Select Drivers', 
            c('RICCIARDO Daniel' = 'ricciardo',
              'VETTEL Sebastian' = 'vettel',
              'HILL Damon' = 'damon_hill',
              'RAIKKONEN Kimi' = 'raikkonen',
              'ROSBERG Nico' = 'rosberg',
              'PROST Alain' = 'prost',
              'MASSA Felipe ' = 'massa',
              'BOTTAS Valtteri' = 'bottas',
              'SENNA Ayrton' = 'senna',
              'SCHUMACHER Michael' = 'michael_schumacher',
              'MAX Verstappen' = 'max_verstappen',
              'PIQUET Nelson' = 'piquet',
              'HAKKINEN Mika' = 'hakkinen',
              'STEWART Jackie' = 'stewart',
              'MANSELL Nigel' = 'mansell',
              'BUTTON Jenson' = 'button',
              'BARRICHELLO Rubens' = 'barrichello',
              'LAUDA Niki' = 'lauda',
              'WEBBER Mark' = 'webber',
              'ALONSO Fernando' = 'alonso',
              'HAMILTON Lewis' = 'hamilton'),
            
            multiple = TRUE,
            
#default drivers that are selected
selected = c('HAMILTON Lewis'='hamilton','VETTEL Sebastian'='vettel'))
                            ),

#datas and plots that are to be included in the main panel are represented in order two bar graph and a single chored diagram is made present in here.
                            mainPanel(
                              
                              #drivers details are identified and compared using differnt selection option avaialable
                              tags$h3("Comparing the stats of drivers"),
                              tags$p("Different driver stats are compared to identify who has been in this particular sports for a longer time and also the best driver among the selection can be identified by the total wins"),
                              #id to map with the graph from the server
                              plotlyOutput(outputId = 'bar_plot1'),
                              
                              #chord graph diagram helps in identifying the key detils on th driver links
                              tags$p("The Chord diagram represent the relation between differnt drivers and also the connection between differnt teams"),
                              #id to map with the graph from the server
                              plotOutput(outputId = 'chord_plot'),
                              
                              #drivers details are identified and compared using differnt selection option avaialable
                              tags$p("This Bar graph explains the differnt comparison between home wins and away race win of the drivers, this also provides differnt driver comparison to identify the"),
                              #id to map with the graph from the server
                              plotOutput(outputId = 'bar_plot2')
                              
                              
                            )
                          )
                 ),
#second tab is initialized and required details are added
                 tabPanel("Circuits and Pit stops",
                          sidebarLayout(position = "right",
                            sidebarPanel(h6(strong("USER MANUAL")),
                                         p("Select the track that you want to compare"),
                                         p("Silverstone is chosen as default track"),
                                         p("You can also Select other tracks to compare "),
                                         p("You can use the mouse ponter over bar and density for more details"),
                                         selectInput(inputId = 'circuits', 
                                                     #different tracks that are required are aligned and suitable changes are made to it by manually enteering the required data
                                                     label = 'Select a Grand Prix circuit', c(
'	Silverstone' = '9',
'	Circuit de Barcelona-Catalunya' = '4',
'	Circuit of the Americas' = '69',
'Albert Park' = '1',
'Hockenheimring ' = '10',
'Sepang International' = '2',
'Bahrain International' = '3',
'	Yas Marina Circuit' = '24',
'	Circuit Paul Ricard' = '34',
'	Marina Bay Street ' = '15',
'	Shanghai International' = '17',
'Hungaroring' = '11',
'	Circuit de Monaco' = '6',
'	Buddh International' = '68',
'	Istanbul Park' = '5',
'	Valencia Street'= '12',
'	Baku City' = '73',
'Autodromo Nazionale di Monza' = '14',
'	Circuit Gilles Villeneuve' = '7',
'Suzuka Circuit' = '22',
'Autodromo Jose Carlos Pace' = '18',
'Nurburgring'= '20',
'	Circuit de Spa-Francorchamps' = '13',
'	Red Bull Ring' = '70'))

                            ),
#mainpanel required data are added
                            mainPanel(
                              #plot 1 for secondtab
                              tags$h3("Circuit and pit-stop details"),
                              
                              tags$p("The heat map, shows the that each team takes differnt time to work on the pit stop and its function, the fastest and slowest pit stop can be represented using those pit stops"),
                              #id to map with the graph from the server
                              plotlyOutput(outputId = 'pit_plot2'),
                              
                              #plot @ for second tab
                              
                              tags$p("Teams will aim for each of their vehicles to pit following a planned schedule, with the number of stops determined by many factors, such as fuel capacity, tyre lifespan, and the trade-off between time lost in the pits versus time gained on the track due to the benefits of pit stops. Choosing the optimum pit strategy of how many stops to make and when to make them is crucial in having a successful race."),
                              #id to map with the graph from the server
                              plotlyOutput(outputId = 'pit_plot1'),
                              
                             
                              
                              
                            )
                          )
                 ),

#second tab is initialized and required details are added
                 tabPanel("Trends",
                          #sidebar layout is set to right
                          sidebarLayout(position = "right",
                            sidebarPanel(h6(strong("USER MANUAL")),
                                         
                                         p("You can adjust the range to compare data from differnt years"),
                                         
                                         p("Slide over graph for differnt controls."),
                                         
                                         sliderInput(inputId = "year", "Slide the year range",
                                                     min = 2003, max = 2021,
                                                     value = c(2018,2020))
                            ),
                            mainPanel(
                              #plot data 
                              h6("Differnt Circits and datas providing fastest laps and Trends can be identified "),
                              #id to map with the graph from the server
                              plotlyOutput(outputId = 'lap_plot1'),
                              
                              #pl
                              tags$p("Identify differnt evolution that has happend to formula 1 to identify the differnt aspects of the fastest time and slowest time for laps"),
                              #id to map with the graph from the server
                              plotlyOutput(outputId = 'lap_plot2')
                            )
                          )
                 ),setBackgroundImage(src = "https://www.formula1.com/content/dam/fom-website/sutton/2020/AbuDhabi/Sunday/1291013771.jpg"
                 ))

#--------------------------------------------------SERVER-------------------------------------------------------------------

server <- function(input, output) {
  
  shinyalert("Hello Motorsport Fans! Welocome to F1"
             , "The Formula One series originated with the European Championship of Grand Prix motor racing (q.v. for pre-1947 history) of the 1920s and 1930s. The formula consists of a set of rules that all participants' cars must meet. Formula One was a new formula agreed upon during 1946 after World War II, with the first non-championship races taking place that year. The first Formula 1 race was the 1946 Turin Grand Prix. A number of Grand Prix racing organisations had laid out rules for a world championship before the war, but due to the suspension of racing during the conflict, the World Drivers' Championship did not become formalised until 1947. The first world championship race took place at Silverstone in the United Kingdom in 1950. A championship for constructors followed in 1958. National championships existed in South Africa and the UK in the 1960s and 1970s. Non-championship Formula One events were held by promoters for many years, but due to the increasing cost of competition, the last of these occurred in 1983",confirmButtonText = "It's Lights out and away we go!!")
  
  #create a bar plot which access the retried csv file to perform required actions.
  
  #reference: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
  output$bar_plot1 <- renderPlotly({
    #ggplot to design a bar graph
    
    ggplot(bar_plot[bar_plot$driverRef %in% input$drivers,],
           #set aews value
           aes(factor(driverRef), count, fill = Matches)) + 
      #set geometry
      geom_bar(stat="identity", position = "dodge") + 
      
      #pallatte colour x-axis and y-axis representation
      scale_fill_brewer(palette = "Set2") + xlab("Driver")+ ylab("Race")+
      
      #add theme 
      theme(axis.text.x = element_text(angle = 60,hjust = 1))
  })
  
  #chord_plot
  
  #reference: https://cran.r-project.org/web/packages/circlize/
  #reference : https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html
  
  #render plot and circos library is used to design te chord diagram
  output$chord_plot <- renderPlot({ chordDiagram(chord_data[,c(1:2)],transparency = 0.1, col= as.character(chord_data$color), annotationTrack = "grid", preAllocateTracks = 1)
    
    #circos plot
    circos.trackPlotRegion( track.index = 1, panel.fun = function(x, y) { xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    
    #set circos text value and specification
    circos.text( mean(xlim),
                 ylim[1], 
                 sector.name,
                 facing = "clockwise", 
                 niceFacing = TRUE, 
                 adj = c(0, 0.25), 
                 cex=.7)
    
    #set circos axis value and specification
    circos.axis( h = "top", 
                 labels.cex = 0.5,
                 major.tick.percentage = 0.2,  sector.index = sector.name,
                 track.index = 2)
    }, 
    
    #border
    
    bg.border = NA)  
    
  })
  
  #horizontal bar_plot
  #reference: https://www.r-graph-gallery.com/202-barplot-for-likert-type-items.html
  
  output$bar_plot2 <- renderPlot({GreyOrange <- likertColor(nc=2, ReferenceZero=NULL,colorFunction="sequential_hcl",colorFunctionArgs= list(h=c(20, 100), c=190, l=c(65,30), power=1.5))
  
  
  #usign likert to design a two way horizontal graph
  likert(plot_table[plot_table$DriverRef %in% input$drivers], horizontal = T,ylab = 'Drivers',col=GreyOrange,main = 'Total Wins Vs Home win', ColorSet(4, 2), auto.key = list(space = "right", columns = 1,
                                                                                                                                                                              reverse = TRUE),reference.line.col="#FF0000", col.strip.background="gray97")
  })
  
  
  
  #heat map plot
  
  output$pit_plot2 <- renderPlotly({
    
    #get values from density_data csv , circuit id and circuits are retried
    data_heat <- density_data[density_data$circuitId == input$circuits,]
    
    #data1 variable is filtered
    data_heat %>%
      filter( raceId == max(raceId) ) %>%
      
      #ggplot is used to plot data
      ggplot(aes(x = lap, y = driverRef)) +
      
      #set gemetric tile
      geom_tile(aes(fill = milliseconds), 
                #set color and legends
                colour = 'blue') +scale_fill_gradient(low = "pink", 
                                                      high = "red", 
                                                      na.value = NA)+
      #insert title and axis
      theme_clean(base_size = 10) + ggtitle("Pit-stop time analysis") + ylab("  Drivers  ")
    
  })
  
  #desity graph to represent data
  
  #pit plot data is used
  
  output$pit_plot1 <- renderPlotly({
    
    #heat density data is used with filter from csv
    heat <- density_data[density_data$circuitId == input$circuits,]
    
    
    graph <- heat %>%
      
      #ggplot to plot data
      ggplot( aes(x=lap, fill=circuitRef)) +
      
      #geom desity is used to render data and provide title and axis
      geom_density(color="#ABB065", alpha=1)+ ggtitle("Pit-stop lap analysis")
    
    #plot graph
    ggplotly(graph)
  })
  
  #reference: https://ggplot2.tidyverse.org/reference/facet_grid.html
  
  #facet_grid plot to identify and compare differnt values to identify trends
  output$lap_plot1 <- renderPlotly({
    
    #filter trend data value
    trend_data %>% 
      
      #dplyr used for query filtration
      dplyr::filter(trend_data$year>=input$year[1] & trend_data$year<=input$year[2] ) %>%  dplyr::group_by(circuitRef,year) %>% 
      
      #getting required values
      summarize(medianFastestLapTimeNum = median(fastestLapTimeNum,na.rm=T)) %>% 
      
      #using ggplot to plot data
      ggplot(aes(x=factor(year),y= medianFastestLapTimeNum, color=medianFastestLapTimeNum)) + geom_point() + theme_gdocs() + 
      scale_color_gradientn(name="",colours=rev(viridis::magma(30))) +
      
      #adding theme and axis values
      theme(
        #set axis value and size
        axis.text.x = element_text(size=6.5,angle=90),
        #set strip value
        strip.text.x = element_text(size = 9)) + facet_wrap(~circuitRef,ncol=6) + xlab("Year")+ ylab("")+labs(title=' lap time', subtitle='time') + guides(color=FALSE)
  })
  
  #reference: https://ggplot2.tidyverse.org/reference/facet_grid.html
  
  #facet_grid plot to identify and compare differnt values to identify trends
  output$lap_plot2 <- renderPlotly({
    
    #filter trend data value
    trend_data %>% 
      
      #dplyr used for query filtration
      dplyr::filter(trend_data$year>=input$year[1] & trend_data$year<=input$year[2] ) %>%  dplyr::group_by(circuitRef,year) %>% 
      
      #getting required values
      summarize(medianFastestLapSpeed = median(fastestLapSpeed,na.rm=T)) %>% 
      
      #using ggplot to plot data
      ggplot(aes(x=factor(year),y= medianFastestLapSpeed,color=medianFastestLapSpeed)) +  geom_point() + theme_gdocs() +scale_color_gradientn(name="",colours=rev(viridis::magma(30))) +
      
      #adding theme and axis values
      theme( axis.text.x = element_text(size=6.5,angle=90),
             #strip text
             strip.text.x = element_text (size = 9)) +
      #use face wrap
      facet_wrap(~circuitRef,ncol=6) + 
      #set x and y axis
      xlab("Year")+ ylab("Speed in km/h")+ labs(title=' Lap speed ',
                                                #add subs
                                                subtitle='Speed') + 
      guides(color=FALSE)
  })
  
}

#--------------------------------------------------APPLICATION-------------------------------------------------------------------


#launch application
shinyApp(ui = ui, server = server)
