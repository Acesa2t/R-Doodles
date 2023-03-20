#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

#Using a simplified version of the TBI prevalence files (NOT for manuscript use) to practice using interactive data visualization tools

rm(list = ls(all.names = TRUE))

library(tidyverse)
library(plotly)
library(gganimate)
library(png)
library(gifski)

tbi_2001 <- readRDS("/Users/ajordan/Documents/GitHub/R-Doodles/estimates_2001_v4.rds")
tb_inc <- readRDS("/Users/ajordan/Documents/GitHub/R-Doodles/incidence.rds")
colnames(tb_inc) <- c("ISO3", "WHO_Region", "e_inc_100k")
tb_inc <- tb_inc%>%select(-WHO_Region)

grouping_function <- function(df){
  
  tbi_data <- merge(df, tb_inc, by = "ISO3")
  # Less bands
  tbi_data_bands <- tbi_data%>%
    mutate(inc_band_per100k = ifelse(
      e_inc_100k < 50, "0-49 per 100,000 persons", ifelse(
        e_inc_100k >= 50 & e_inc_100k < 100, "50-99 per 100,000 persons", ifelse(
          e_inc_100k >= 100 & e_inc_100k < 200, "100-199 per 100,000 persons",ifelse(
            e_inc_100k >= 200 & e_inc_100k <= 1000, "200+ per 100,000 persons", "No value")))))
  
  tbi_data_bands$inc_band_per100k <- factor(tbi_data_bands$inc_band_per100k,
                                            levels = c("0-49 per 100,000 persons",
                                                       "50-99 per 100,000 persons",
                                                       "100-199 per 100,000 persons",
                                                       "200+ per 100,000 persons"))
  
  tbi_age_groups <- tbi_data_bands%>%
    mutate(AGE_BANDS = ifelse(
      AGEP < 15, "0-14", ifelse(
        AGEP >= 15 & AGEP < 35, "15-34", ifelse(
          AGEP >= 35 & AGEP < 55, "35-54",ifelse(
            AGEP >= 55 & AGEP < 75, "55-74",ifelse(
              AGEP >= 75 & AGEP <= 200, "75+", "No value"))))),
      AARP = YARP-YOBP)
  
  
  
  tbi_graph_aarp <- tbi_age_groups%>%
    mutate(AARP_BANDS = ifelse(
      AARP < 15, "AARP 0-14", ifelse(
        AARP >= 15 & AARP < 35, "AARP 15-34", ifelse(
          AARP >= 35 & AARP < 55, "AARP 35-54",ifelse(
            AARP >= 55 & AARP <= 75, "AARP 55-74", ifelse(
              AARP >= 75 & AARP <= 200, "AARP 75+", "No value"))))))%>%
    mutate(BPLP = recode(BPLP, `China, People's Republic of`="China"))%>%
    mutate(BPLP = ifelse(str_detect(BPLP, "Hong Kong"), "Hong Kong", 
                         ifelse(str_detect(BPLP, "United States"), "United States of America", BPLP)))
  
  tbi_graph_aarp
}

tbi_2001_grouped <- grouping_function(tbi_2001)

tbi_2001_grouped[tbi_2001_grouped=="United States of America"] <- "United States"
tbi_2001_grouped$YARP <- as.Date(paste(tbi_2001_grouped$YARP, 7, 1, sep = "-"))

filter_vec <- tbi_2001_grouped%>%
  group_by(BPLP)%>%
  summarize(NUMP = sum(NUMP))%>%
  arrange(desc(NUMP))%>%
  slice_head(n=10)


tbi_top10 <- tbi_2001_grouped%>%filter(BPLP%in%c(filter_vec$BPLP))

tbi_top10_grouped <- tbi_top10%>%group_by(BPLP,YARP)%>%summarize(NUMP = sum(NUMP))
all_top10 <- tbi_top10%>%group_by(YARP)%>%summarize(NUMP = sum(NUMP))%>%mutate(BPLP = 'All')
tbi_top10_grouped <- rbind(tbi_top10_grouped, all_top10)

View(tbi_top10_grouped)
# Define UI for application that draws a histogram
ui <- navbarPage( "Place of Origin Immigration Data",

    # Application title
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 h3("This application allows you to explore data specific to the foreign-born population in Canada in 2001. Click on the different tabs to see where permanent residents and foreign-born Canadian citizens came from and when they immigrated")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 img(src="immigration_pop_appears_2001.gif")
               )
             )),
    
    tabPanel("Number Living in Canada",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("Number Living in Canada in 2001"),
        h5("Hover the cursor over each bar to see the"),
        h5("number of people from each place of origin"),
        h5("during the 2001 census year"),
               #      selectInput('dataset', 'Census Year',"Census 2001")
                    
      ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("barPlot")
        )
    )),
    
    tabPanel("Year of Immigration",
             sidebarLayout(
               sidebarPanel(
                 h4("See how many people living in Canada had immigrated during a particular year. Click the radio buttons to select different places of birth."),
                 radioButtons("placeType",
                              "Place of Birth",
                              c("All 10" = "All", 
                                "China" = "China",
                                "Germany" = "Germany",
                                "Hong Kong",
                                "India",
                                "Italy",
                                "Philippines",
                                "Poland",
                                "Portugal",
                                "United Kingdom",
                                "United States"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("linePlot")
               )
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    x <- reactive({
      input$dataset
    })
    
    output$barPlot <- renderPlotly({

      fig <- plot_ly(
        data = tbi_top10_grouped%>%
          group_by(BPLP)%>%
          summarize(NUMP = round(sum(NUMP)),0),
        x = ~BPLP,
        y = ~NUMP,
        type = "bar")%>%
        layout(
          xaxis = list(title = "Place of Origin"),
          yaxis = list(title = "Number Living in Canada"))
      
    })
    
    output$linePlot <- renderPlot({
      
      ggplot(subset(tbi_top10_grouped, BPLP==input$placeType), aes(x = YARP, y = NUMP, color = input$placeType))+
        geom_line()+
        labs(x = "Year of Immigration to Canada",
             y = "Permanent Residents and Foreign-Born \nCitizens Living in Canada",
             color = "Country of Origin",
             title = str_c("Number from ", input$placeType))+
        scale_y_continuous(limits = c(0,90000), breaks = round(seq(0, 90000, by = 5000),0))+
        theme_bw()+
        theme(text = element_text(size = 13, face = "bold"))+
        theme(axis.text.x = element_text(angle = 40))+
        theme(axis.text = element_text(size = 12))+
        theme(plot.title = element_text(size=13))+
        theme(legend.position="none")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
