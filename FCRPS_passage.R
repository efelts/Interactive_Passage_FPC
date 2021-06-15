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
         Percent_To_Date=Total_To_Date/annual_total*100,
         lt=ifelse(Year=="Average of Last 5 Years","average","annual")) 




#mess with steelhead dates/season

todays_date <- Sys.Date()
current_year <- year(todays_date)

sthd_alter <- cumulative_dat %>% 
  filter(select_spp=="Steelhead") %>% 
  mutate(add_yr=dummyd+years(x=1),
         dam_month=month(dummyd),
         spawn_year=ifelse(dam_month<7,yr,(yr+1)),
         run_yr=if_else(dam_month>6,yr,(yr-1)),
         year_cat=paste(run_yr,spawn_year,sep="-"),
         dummyd=if_else(dam_month<7,add_yr,dummyd),
         year_cat=ifelse(is.na(run_yr),"Average of Last 5 Years",year_cat))


st_test <- daily_base.f(dat=sthd_alter)
st_test

sthd_alter %>% ggplot(aes(dummyd,Number_Passed,label=Day))+
  geom_line(aes(color=year_cat))+
  scale_color_fivethirtyeight()+
  theme_fivethirtyeight()+
  labs(x="",y="Number Passing Lower Granite Dam")+
  scale_x_date(date_breaks="1 month",date_labels="%B")

#write a function for the base daily plot that can be slightly modified

daily_base.f <- function(dat=plotdat){
  
  x_min <- min(dat$dummyd)
  x_max <- max(dat$dummyd)
  
  p <- dat %>% 
    ggplot(aes(dummyd,Number_Passed,label=Day))+
    geom_line(aes(color=Year))+
    scale_color_fivethirtyeight()+
    theme_fivethirtyeight()+
    labs(x="",y="Number Passing Lower Granite Dam")+
    scale_x_date(limits=c(x_min,x_max),
                 date_breaks="1 month",date_labels="%B")
}

test.dat <- cumulative_dat %>% 
  filter(select_spp=="Sockeye")


test <- daily_base.f(dat=test.dat)
test

x_min <- min(plotdat$dummyd)
x_max <- max(plotdat$dummyd)

#write a function for the base cumulative plot that can be slightly modified

cumulative_base.f <- function(dat=plotdat){
  
  x_min <- min(dat$dummyd)
  x_max <- max(dat$dummyd)
  
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

#work on building the components separately (easier to debug)

header <- dashboardHeader(title="Passage at Dams")
sidebar <- dashboardSidebar(width=500,
  sidebarMenu(
    menuSubItem(icon = NULL,
                selectInput("species",
                            label="Choose a Species:",
                            choices=c("Spring-Summer Chinook",
                                      "Fall Chinook",
                                      "Steelhead",
                                      "Sockeye",
                                      "Coho",
                                      "Lamprey"))),
    sidebarMenuOutput("secondSelection"),
    sidebarMenuOutput("slider")
  )
)

body <- dashboardBody(
  fluidRow(
  box(plotlyOutput("cumulative_plot",height=700)),
  box(plotlyOutput("daily_plot",height=700))
  )
)


ui <- dashboardPage(header,sidebar,body)
    
  
  

server <- function(input,output,session){

  
  output$secondSelection <- renderMenu({
    if(input$species=="Spring-Summer Chinook"|
       input$species=="Fall Chinook"|
       input$species=="Coho")
      return( menuSubItem(icon=NULL,
                selectInput("group_select",
                            label="Choose a group to plot",
                            choices=c("Adult","Jack","Total"),
                            selected="Adult")))
  
    if(input$species=="Steelhead")
      return( menuSubItem(icon=NULL,
                          selectInput("group_select",
                                      label="Choose a group to plot",
                                      choices=c("Total","Unclipped"),
                                      selected="Total")))
    if(input$species=="Lamprey"|
       input$species=="Sockeye")
      return(menuSubItem(icon=NULL,
                         selectInput("group_select",
                                     label="Choose a group to plot",
                                     choices=c("Total"),
                                     selected="Total")))
  })
  
  output$slider <- renderMenu({
    
    if(input$species=="Spring-Summer Chinook")
      return(menuSubItem(icon=NULL,
                sliderInput("dates",
                            label="Choose a date range",
                            min=as.Date("2050-03-01","%Y-%m-%d"),
                            max=as.Date("2050-08-17","%Y-%m-%d"),
                            value=c(as.Date("2050-03-01","%Y-%m-%d"),
                                    as.Date("2050-08-17","%Y-%m-%d")),
                            timeFormat="%m-%d")))
    
    if(input$species=="Fall Chinook")
      return(menuSubItem(icon=NULL,
                         sliderInput("dates",
                                     label="Choose a date range",
                                     min=as.Date("2050-08-17","%Y-%m-%d"),
                                     max=as.Date("2050-12-31","%Y-%m-%d"),
                                     value=c(as.Date("2050-08-17","%Y-%m-%d"),
                                             as.Date("2050-12-31","%Y-%m-%d")),
                                     timeFormat="%m-%d")))
    
    if(input$species=="Coho")
      return(menuSubItem(icon=NULL,
                         sliderInput("dates",
                                     label="Choose a date range",
                                     min=as.Date("2050-09-01","%Y-%m-%d"),
                                     max=as.Date("2050-12-31","%Y-%m-%d"),
                                     value=c(as.Date("2050-09-01","%Y-%m-%d"),
                                             as.Date("2050-12-31","%Y-%m-%d")),
                                     timeFormat="%m-%d")))
    
    if(input$species=="Steelhead")
    return(menuSubItem(icon=NULL,
                       sliderInput("dates",
                                   label="Choose a date range",
                                   min=as.Date("2050-03-01","%Y-%m-%d"),
                                   max=as.Date("2050-12-31","%Y-%m-%d"),
                                   value=c(as.Date("2050-03-01","%Y-%m-%d"),
                                           as.Date("2050-12-31","%Y-%m-%d")),
                                   timeFormat="%m-%d")))
    
   if(input$species=="Sockeye")
    return(menuSubItem(icon=NULL,
                       sliderInput("dates",
                                   label="Choose a date range",
                                   min=as.Date("2050-07-01","%Y-%m-%d"),
                                   max=as.Date("2050-12-31","%Y-%m-%d"),
                                   value=c(as.Date("2050-07-01","%Y-%m-%d"),
                                           as.Date("2050-12-31","%Y-%m-%d")),
                                   timeFormat="%m-%d")))
    
    if(input$species=="Lamprey")
    return(menuSubItem(icon=NULL,
                       sliderInput("dates",
                                   label="Choose a date range",
                                   min=as.Date("2050-01-01","%Y-%m-%d"),
                                   max=as.Date("2050-12-31","%Y-%m-%d"),
                                   value=c(as.Date("2050-01-01","%Y-%m-%d"),
                                           as.Date("2050-12-31","%Y-%m-%d")),
                                   timeFormat="%m-%d")))
    
    
  })
  
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
      filter(Origin==input$group_select) %>% 
      filter(dummyd > x_min&
               dummyd < x_max)
    
    dat_others <- cumulative_dat %>% 
      filter(select_spp==input$species) %>% 
      filter(dummyd > x_min&
               dummyd < x_max)
    
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
      ggtitle("Daily Spring-Summer Chinook Count at Lower Granite Dam")
    
    fall_plot <- daily_base.f(dat=plotdat)+
      ggtitle("Daily Fall Chinook Count at Lower Granite Dam")
    
    sthd_plot <- daily_base.f(dat=plotdat)+
      ggtitle("Daily Steelhead Count at Lower Granite Dam")
    
    sock_plot <- daily_base.f(dat=plotdat)+
      ggtitle("Daily Sockeye Count at Lower Granite Dam")
    
    coho_plot <- daily_base.f(dat=plotdat)+
      ggtitle("Daily Coho Count at Lower Granite Dam")
    
    lam_plot <- daily_base.f(dat=plotdat)+
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
  
  reactive_cumulative_plot <- reactive({
    
    plotdat=reactive_dat()
    
    x_min <- min(plotdat$dummyd)
    x_max <- max(plotdat$dummyd)
    
    sp_su_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Spring-Summer Chinook Count at Lower Granite Dam")
    
    
    fall_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Fall Chinook Count at Lower Granite Dam")
    
    sthd_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Steelhead Count at Lower Granite Dam")
    
    sock_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Sockeye Count at Lower Granite Dam")
    
    coho_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Coho Count at Lower Granite Dam")
    
    lam_plot <- cumulative_base.f(dat=plotdat)+
      ggtitle("Cumulative Pacific Lamprey Count at Lower Granite Dam")
    
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
    ggplotly(reactive_plot(),tooltip=c("Day","Year","Number_Passed")) %>% 
      layout(hovermode="x unified") 
  })
  
  output$cumulative_plot <- renderPlotly({
    req(reactive_cumulative_plot())
    ggplotly(reactive_cumulative_plot(),tooltip=c("Day","Year","Total_To_Date")) %>% 
      layout(hovermode="x unified") 
    
  })
}

shinyApp(ui,server)

