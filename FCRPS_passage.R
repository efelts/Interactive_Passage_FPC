#load necessary libraries

library(tidyverse)
library(lubridate)
library(gghighlight)
library(plotly)
library(ggthemes)
library(viridis)
library(shiny)
library(shinydashboard)


#define FPC url to be able to download

#right now bringing in data manually. This will change to pull from FPC daily updates

lgr_daily <- read_csv("data/LGR_daily.csv") %>% 
  mutate(ChinookTotal=ChinookAdult+ChinookJack,
         CohoTotal=CohoAdult+CohoJack,
         date = mdy(Date)) %>% 
  dplyr::select(Dam,date,ChinookTotal,ChinookAdult,ChinookJack,
                CohoTotal,CohoAdult,CohoJack,Steelhead,UnclippedSteelhead,
                Sockeye,Pink,Chum,Lamprey,Shad)

#pivot longer and make a dummy date column for plotting purposes

lgr_long <- pivot_longer(lgr_daily,3:15,names_to="Species",values_to = "count") %>% 
  mutate(julian=yday(date),
         wk=week(date),
         yr=year(date)) %>% 
  mutate(spp=ifelse(Species=="ChinookTotal"&
                      julian<230,"SpSuChinook",
                    ifelse(Species=="ChinookAdult"&
                             julian<230,"SpSuChinook",
                           ifelse(Species=="ChinookJack"&
                                    julian<230,"SpSuChinook",
                                  ifelse(Species=="ChinookTotal"&
                                           julian>229,"FallChinook",
                                         ifelse(Species=="ChinookAdult"&
                                                  julian>229,"FallChinook",
                                                ifelse(Species=="ChinookJack"&
                                                         julian>229,"FallChinook",Species))))))) %>% 
  
  mutate(spp=ifelse(str_starts(Species,"Coho"),"Coho",spp)) %>% 
  mutate(spp=ifelse(str_ends(Species,"Steelhead"),"Steelhead",spp)) %>% 
  mutate(Origin=ifelse(str_starts(Species,"Unclipped"),"Unclipped","Total")) %>% 
  filter(!is.na(count)) %>% 
  mutate(dummy_yr=2050,
         d=day(date),
         m=month(date),
         dummy_date=paste(m,d,dummy_yr,sep="-")) %>% 
  mutate(dummyd=mdy(dummy_date)) %>% 
  dplyr::select(Dam,date,Species,spp,Origin,count,julian,wk,yr,dummyd) %>% 
  mutate(Maturity=ifelse(str_ends(Species,"Jack"),"Jack",
                         ifelse(str_ends(Species,"Adult"),"Adult","Total")))

#get annual totals by species so they can be joined back in to calculate
#proportion of total run by date

lgr_annual <- lgr_long %>% 
  filter(yr<2021) %>% 
  group_by(spp,Maturity,Origin,yr) %>% 
  summarize(annual_total=sum(count))


lgr_daily.df <- left_join(lgr_long,lgr_annual,by=c("spp","Maturity","Origin","yr")) %>% 
  mutate(prop_total=count/annual_total) %>% 
  mutate(select_spp=ifelse(spp=="SpSuChinook","Spring-Summer Chinook",
                           ifelse(spp=="FallChinook","Fall Chinook",
                                  spp)))

#get summaries for last 5 years by species so they can be joined back in
#as a reference 

lgr_last5 <- lgr_daily.df %>% 
  filter(yr>2015&yr<2021) %>% 
  group_by(julian,select_spp,Maturity,Origin) %>% 
  summarize(count=mean(count),
          dummyd=first(dummyd)) %>% 
  mutate(Year="Average of Last 5 Years")


#put daily counts with 5-year summaries and rename some columns for better 
#handling in plotly outputs

lgr.dat <- lgr_daily.df %>% 
  filter(yr>2019) %>% 
  mutate(Year=as.factor(yr)) %>% 
  bind_rows(lgr_last5) %>% 
  mutate(Day=format(as.Date(dummyd),"%m-%d")) %>% 
  mutate(Number_Passed=round(count))


#test out internals of shiny function
date1 <- format(as.character(Sys.Date(),"%m-%d")) 
date_plot <- as.Date(paste("2050",date1,sep="-"))



#work out running counts

cumulative_dat <- lgr.dat %>% 
  arrange(select_spp,Year,Origin,Maturity,dummyd) %>% 
  group_by(Year,select_spp,Origin,Maturity) %>% 
  mutate(Total_To_Date=cumsum(Number_Passed),
         percent_to_date=Total_To_Date/annual_total*100) 


#write a function for the base daily plot that can be slightly modified

daily_base.f <- function(dat=plotdat){
  
  p <- dat %>% 
    ggplot(aes(dummyd,Number_Passed,label=Day))+
    geom_line(aes(color=Year))+
    scale_color_fivethirtyeight()+
    theme_fivethirtyeight()+
    labs(x="",y="Number Passing Lower Granite Dam")
}


x_min <- min(plotdat$dummyd)
x_max <- max(plotdat$dummyd)

#write a function for the base cumulative plot that can be slightly modified

cumulative_base.f <- function(dat=plotdat){
  
  x_min <- min(plotdat$dummyd)
  x_max <- max(plotdat$dummyd)
  
  p <- dat %>% 
    ggplot(aes(dummyd,Total_To_Date,label=Day))+
    geom_line(aes(color=Year))+
    scale_color_fivethirtyeight()+
    theme_fivethirtyeight()+
    labs(x="",y="Cumulative Number Passed, Lower Granite Dam")+
    scale_x_date(limits=c(x_min,x_max),
                  date_breaks="1 month",date_labels="%B")
}

#start making a shiny app with these data, just Chinook for now

#build the UI

ui <- dashboardPage(
  
 dashboardHeader(title="Daily Passage Counts"),
 
 dashboardSidebar(width=300,
                  sidebarMenu(
                    menuItem("Daily Counts",tabName="main",icon=icon("chart-line"))
                  )),
 
 dashboardBody(
   
   tabItems(
     
     #Define UI for daily counts
     
     tabItem(tabName="main",
             
             column(2,selectInput("species",
                                  label="Choose a Species:",
                                  choices=c("Spring-Summer Chinook",
                                            "Fall Chinook",
                                            "Steelhead",
                                            "Sockeye",
                                            "Coho",
                                            "Lamprey"))),
             
             column(4,offset=1,uiOutput("slider")),
             
             column(2,uiOutput("secondSelection")),
             
             fluidRow(
             box(6,plotlyOutput("daily_plot",height=500))
             
             ))
   )
             
 )
)


server <- function(input,output,session){
  
  
  #make a secondary select input dependent on initial selection of species
  
  output$secondSelection <- renderUI({
    
    if(input$species=="Spring-Summer Chinook")
      return(selectInput("group_select",
             label="Choose a group to plot",
             choices=c("Adult","Jack","Total"),
             selected="Adult"))
    
    if(input$species=="Fall Chinook")
      return(selectInput("group_select",
                         label="Choose a group to plot",
                         choices=c("Adult","Jack","Total"),
                         selected="Adult"))
    
    if(input$species=="Coho")
      return(selectInput("group_select",
                         label="Choose a group to plot",
                         choices=c("Adult","Jack","Total"),
                         selected="Adult"))
    
    if(input$species=="Steelhead")
      return(selectInput("group_select",
                         label="Choose a group to plot",
                         choices=c("Total","Unclipped"),
                         selected="Total"))
    
  })
  
  output$slider <- renderUI({
    
    if(input$species=="Spring-Summer Chinook")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-03-01","%Y-%m-%d"),
                         max=as.Date("2050-08-17","%Y-%m-%d"),
                         value=c(as.Date("2050-03-01","%Y-%m-%d"),
                                 as.Date("2050-08-17","%Y-%m-%d")),
                         timeFormat="%m-%d"))
    
    if(input$species=="Fall Chinook")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-08-17","%Y-%m-%d"),
                         max=as.Date("2050-12-31","%Y-%m-%d"),
                         value=c(as.Date("2050-08-17","%Y-%m-%d"),
                                 as.Date("2050-12-31","%Y-%m-%d")),
                         timeFormat="%m-%d"))
    
    
    if(input$species=="Coho")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-09-01","%Y-%m-%d"),
                         max=as.Date("2050-12-31","%Y-%m-%d"),
                         value=c(as.Date("2050-09-01","%Y-%m-%d"),
                                 as.Date("2050-12-31","%Y-%m-%d")),
                         timeFormat="%m-%d"))
    
    if(input$species=="Steelhead")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-03-01","%Y-%m-%d"),
                         max=as.Date("2050-12-31","%Y-%m-%d"),
                         value=c(as.Date("2050-03-01","%Y-%m-%d"),
                                 as.Date("2050-12-31","%Y-%m-%d")),
                         timeFormat="%m-%d"))
    
    if(input$species=="Sockeye")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-07-01","%Y-%m-%d"),
                         max=as.Date("2050-12-31","%Y-%m-%d"),
                         value=c(as.Date("2050-07-01","%Y-%m-%d"),
                                 as.Date("2050-12-31","%Y-%m-%d")),
                         timeFormat="%m-%d"))
    
    if(input$species=="Lamprey")
      return(sliderInput("dates",
                         label="Choose a date range",
                         min=as.Date("2050-03-01","%Y-%m-%d"),
                         max=as.Date("2050-12-31","%Y-%m-%d"),
                         value=c(as.Date("2050-03-01","%Y-%m-%d"),
                                 as.Date("2050-12-31","%Y-%m-%d")),
                         timeFormat="%m-%d"))

    
  })
  
  #make the data  for the plots react to user inputs
  
  reactive_dat <- reactive({
    
    x_min <- min(input$dates-1)
    x_max <- max(input$dates+1)

    dat_salmon <- cumulative_dat%>%
      filter(select_spp==input$species) %>% 
      filter(Maturity==input$group_select) %>% 
      filter(dummyd > x_min&
               dummyd < x_max)
    
    dat_sthd <- cumulative_dat %>% 
      filter(select_spp==input$species) %>% 
      filter(Origin==input$group_select)
    
    dat_others <- cumulative_dat %>% 
      filter(select_spp==input$species)
    
    if(input$species=="Spring-Summer Chinook"|
       input$species=="Fall Chinook"|
       input$species=="Coho")
      return(dat_salmon)
    
    if(input$species=="Steelhead")
      return(dat_sthd)
    
    if(input$species=="Sockeye"|
       input$species=="Lamprey")
      return(dat_others)
    
    })
  
  reactive_plot <- reactive({
    plotdat=reactive_dat()
      
    x_min <- min(plotdat$dummyd)
    x_max <- max(plotdat$dummyd)
    
    sp_su_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=c(x_min,x_max),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Spring-Summer Chinook Count at Lower Granite Dam")
    
    fall_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=as_date(c("2050-08-17","2050-12-31")),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Fall Chinook Count at Lower Granite Dam")
      
    sthd_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=as_date(c("2050-07-1","2050-12-31")),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Steelhead Count at Lower Granite Dam")
    
    sock_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=as_date(c("2050-06-1","2050-12-31")),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Sockeye Count at Lower Granite Dam")
    
    coho_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=as_date(c("2050-09-01","2050-12-31")),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Coho Count at Lower Granite Dam")
    
    lam_plot <- daily_base.f(dat=plotdat)+
      scale_x_date(limits=as_date(c("2050-03-01","2050-12-31")),
                   date_breaks="1 month",date_labels="%B")+
      ggtitle("Daily Lamprey Count at Lower Granite Dam")
      
    if(input$species=="Spring-Summer Chinook")
      return(sp_su_plot)
    
    if(input$species=="Fall Chinook")
      return(fall_plot)
    
    if(input$species=="Steelhead")
      return(sthd_plot)
    
    if(input$species=="Sockeye")
      return(sock_plot)
    
    if(input$species=="Coho")
      return(coho_plot)
    
    if(input$species=="Lamprey")
      return(lam_plot)

  })
  
  
  output$daily_plot <- renderPlotly({
    req(reactive_plot())
    plotdat=reactive_dat()
    ggplotly(reactive_plot(),tooltip=c("Day","Year","Number_Passed")) %>% 
      layout(hovermode="x unified") 
  })
  

  

}

shinyApp(ui,server)
