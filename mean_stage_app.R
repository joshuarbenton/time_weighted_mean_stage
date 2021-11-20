#Author: Joshua R Benton

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(lubridate)
library(shiny)
library(ggplot2)
library(dataRetrieval)


ui <- fluidPage(
  
  titlePanel("Time-Weighted Mean Stage Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("Site","Input NWIS Site Number", "02198950") ,
      textInput("date","Measurement Date (YYYY-MM-DD)",as.character(Sys.Date())) ,
      textInput("start", "Measurement Start Time (hh:mm:ss) EST", "00:05:00") ,
      textInput("end", "Measurement End Time (hh:mm:ss) EST", "03:02:00") ,
      actionButton("NWIS", "Pull NWIS Data") ,
    
    ),
    
    mainPanel(
      plotOutput("stageplot", height = "400") , 
      textOutput("text") ,
      textOutput("text2") ,
      textOutput("text3") ,
      textOutput("text4") ,
      textOutput("text5")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$NWIS, {
    parameterCd <- "00065"
    
    siteNumber = input$Site
    
    siteINFO <- readNWISsite(siteNumber)
    siteNAME = siteINFO$station_nm
   
  
    startDate <- input$date
    
    stage <- readNWISuv(siteNumber, 
                        parameterCd, startDate, startDate)
    
    stage$dateTime =  as.POSIXct(stage$dateTime, format = "%Y-%m-%d %H:%M:%S") - hours(5)
    
    starttime = input$start
    endtime = input$end
    start_datetime = as.POSIXct(paste(startDate, starttime, sep = " "), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    end_datetime = as.POSIXct(paste(startDate, endtime, sep = " "), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    #The 15-min NWIS data point preceding the measurement
    dateTime_1 = floor_date(start_datetime, unit = "15 min")
    
    #The 15-min NWIS data point following the measurement
    dateTime_2 = ceiling_date(end_datetime, unit = "15 min")
    
    #NWIS stage during measurement
    stage = subset(stage, stage$dateTime >=  dateTime_1 & stage$dateTime <= dateTime_2)
    
   
    #For measurements greater than 15 minutes
    if (nrow(stage) > 2) {
      
      #Creates a data frame with two additional rows for the measurement start time, end time, and interpolated stage values
      
      meas = stage[0,]
      meas[1:(nrow(stage)+2),] = NA
      meas[1,] = stage[1,]
      meas$dateTime[2] = start_datetime
      meas[3:(nrow(stage)),] = stage[2:(nrow(stage)-1),]
      meas$dateTime[(nrow(meas)-1)] = end_datetime
      meas[nrow(meas),] = stage[nrow(stage),]
      meas$time =as.numeric(meas$dateTime)
    
      #Linear interpolation of measurement start and end stages based upon preceding and subsequent NWIS data points 
    
      lm1 = lm(stage$X_00065_00000[1:2] ~ as.numeric(stage$dateTime[1:2]))
      lm2 = lm(stage$X_00065_00000[nrow(stage):(nrow(stage)-1)] ~ as.numeric(stage$dateTime[nrow(stage):(nrow(stage)-1)]))
      
      stage_start = lm1$coefficients[2]*as.numeric(start_datetime) + lm1$coefficients[1]
      stage_end = lm2$coefficients[2]*as.numeric(end_datetime) + lm2$coefficients[1]
      
      #Including interpolated values into data-frame
      meas$X_00065_00000[2] = stage_start
      meas$X_00065_00000[nrow(meas)-1] = stage_end
      
      #Change in time for assigning weights
      meas$dt = NA
      
      #Change in stage
      meas$ms = NA
      
      for (i in 2:(nrow(meas)-2)) {
        meas$dt[i] = meas$time[i+1] - meas$time[i]
      }
      
      for (i in 2:(nrow(meas)-2)) { 
        meas$ms[i] = (meas$X_00065_00000[i+1] + meas$X_00065_00000[i])/2
      }
      
      meas$X_00065_00000_cd = "NWIS"
      meas$X_00065_00000_cd[2] = "Qm"
      meas$X_00065_00000_cd[nrow(meas)-1] = "Qm"
      
      
      #Time weighted mean
      mean_stage_tw = round(sum(na.omit(meas$dt*meas$ms))/sum(na.omit(meas$dt)), digits = 2)
      #Arithmetic mean
      mean_stage = round(mean(meas$ms[2:(nrow(meas)-2)]), digits = 2)
      
      line_segment = stage[1:2,]
      line_segment2 = stage[(nrow(stage)-1):nrow(stage),]
      
      #For measurements less than 12 minutes
    } else  {
      
      
      meas = stage[0,]
      meas[1:(nrow(stage)+2),] = NA
      meas[1,] = stage[1,]
      meas[4,] = stage[2,]
      meas$dateTime[2] = start_datetime
      meas$dateTime[3] = end_datetime
      
      #Linear interpolation
      stage$time = as.numeric(stage$dateTime)
      lm1 = lm(stage$X_00065_00000 ~ stage$dateTime)
      
      stage_start = lm1$coefficients[2]*as.numeric(start_datetime) + lm1$coefficients[1]
      stage_end = lm1$coefficients[2]*as.numeric(end_datetime) + lm1$coefficients[1]
      
      
      meas$X_00065_00000[2] = stage_start
      meas$X_00065_00000[3] = stage_end
      
      meas$X_00065_00000_cd = "NWIS"
      meas$X_00065_00000_cd[2:3] = "Qm"
      
      #For measurements < 15 minutes the time-weighted mean equals arithmetic mean
      mean_stage_tw = round(mean(c(stage_start,stage_end)), digits = 2)
      mean_stage = mean_stage_tw
      
      line_segment = stage[1:2,]
      line_segment2 = line_segment
      
    } 
    
    
    time_change = ((hour(end_datetime)*60 + minute(end_datetime)) - (hour(start_datetime)*60 + minute(start_datetime)))/60
    time_change = round(time_change, digits = 2)
    stage_change = round((stage_end - stage_start), digits = 2)
    
    text = paste("Time-weighted mean stage =", mean_stage_tw, sep = " ")
    text2 = paste("Arithmetic mean stage =", mean_stage, sep = " ")
    text3 = paste("Change in stage =", stage_change, "in", time_change, "hours", sep = " ")
    
    
    output$stageplot <- renderPlot({
      
      
      ggplot(meas, aes(dateTime,X_00065_00000 )) + geom_point(size = 2) + theme_bw(15) +
        geom_vline(xintercept = start_datetime, color = "red", linetype = "solid") +
        geom_vline(xintercept = end_datetime, color = "red", linetype = "solid") +
        geom_point(aes(x = end_datetime, y = stage_end), color = "gray", size = 2) +
        geom_point(aes(x = start_datetime, y = stage_start), color = "gray", size = 2) +
        xlab("Time (EST)") + ylab("Stage (ft)") +
        labs(color = "") +
        ggtitle(siteNAME) +
        scale_color_manual(values=c("black","red")) +
        geom_line(data = line_segment, aes(dateTime,X_00065_00000), color = "black", lty = 2) +
        geom_line(data = line_segment2, aes(dateTime,X_00065_00000), color = "black", lty = 2) 
      
     
    } )
  
    output$text <- renderText({text})
    output$text2 <- renderText({text2})
    output$text3 <- renderText({text3})
 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
