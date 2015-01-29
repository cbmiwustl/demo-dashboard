####################################
# ui.R
#
# Executive Producer: Leslie McIntosh
# Developer: Connie Zabarovskaya
# Project Manager: Mary Uhlmansiek
# Center for Biomedical Informatics
# Washington University in St. Louis
####################################

require(rCharts)
library(reshape2)
library(plyr)
#the line below is run in local RStudio only to pull the latest jQuery libraries for rCharts. 
#if a conflict occurs and some charts don't display set this to FALSE both here and in ui.R
#do not run this line if you upload to Shinyapps.io, no plots will appear
#options(rcharts.cdn = TRUE)

#read in Harry Potter Hogwarts data 
hpdata <- read.csv("data/hpdata.csv", stringsAsFactors = FALSE)

#read in butterbeer sales data
butterbeer <- read.csv("data/butterbeerSales.csv", stringsAsFactors = FALSE)

#change the date columns format to Date
hpdata$startdate <- as.Date(hpdata$startdate, format = "%m/%d/%Y")
hpdata$updated <- as.Date(hpdata$updated, format = "%m/%d/%Y")

#Ministry of Magic data is read in via global.R file

#main Shiny Server function for server.R
shinyServer(
  function(input, output) {
    
############################### First tab of Hogwarts data
    
          #reactive (-R) data set for chart1, total # of Professors value (totalNumofProf), and for download
          HoursByProfR <- reactive({
            timeData <- subset(hpdata, hpdata$startdate >= input$startDate & hpdata$startdate <= input$endDate)
            HoursbyProf <- aggregate(round(timeData$hoursworked, digits = 1), 
                                     by=list(Professor=timeData$Professor),FUN=sum, na.rm=TRUE)
            names(HoursbyProf) <- c("Professor", "Hours")
            HoursbyProf <- HoursbyProf[order(-HoursbyProf$Hours),]
            HoursbyProf
          })
    
          #conditional output for chart1 of Hogwarts data: data will be displayed only if 
          #input is valid and there's enough data to display
          output$chart1and13withCondition <- renderUI({
            #the condition for date input is 15 days difference between start and end
            #and that there's data in the subset of original dataset to be used in the chart
            #there is no need for subject dataset verification, because if there's data for
            #professors, there will be data for subjects (no missing data in the source files)
            if (as.Date(input$endDate) - as.Date(input$startDate) > 15 & 
                  nrow(subset(hpdata, hpdata$startdate >= input$startDate & hpdata$startdate <= input$endDate)) > 0) {
              list(
                fluidRow(
                  h4(textOutput("totalNumofProf")),
                  HTML("<hr>")),
                fluidRow(
                  column(width = 5, style="min-width:600px;max-width:600px;margin-right:100px",            
                         p(downloadButton('downloadHoursByProfR', 'Download'), align = "right"),
                         showOutput("chart1", "highcharts")
                  ),
                  column(width = 5, style="min-width:600px;max-width:600px",
                         p(downloadButton('downloadHoursBySubjR', 'Download'), align = "right"),
                         showOutput("chart13", "highcharts")
                  ))
                )
            } else {
              strong("Several dashboard elements are hidden, since not enough data 
                     are available based on provided date input.")
            }
          })
                    
          #bar chart that shows hours that students spent on assignments for specific Professors
          output$chart1 <- renderChart({
            HoursbyProfPlot <- Highcharts$new()
            HoursbyProfPlot$series(data = HoursByProfR()$Hours, type = "bar") 
            HoursbyProfPlot$title(text ="Total Hours Students Spent on Assignments for Professors")
            HoursbyProfPlot$chart(height = 500, width = 600)
            HoursbyProfPlot$plotOptions(bar = list(groupPadding = 0, borderWidth = 2))
            HoursbyProfPlot$yAxis(title = list(text = "Hours Spent"))  
            HoursbyProfPlot$xAxis(type = "category", categories = HoursByProfR()$Professor, 
                                  labels = list(step = 1, style = list(fontSize= '12px')))
            HoursbyProfPlot$tooltip( formatter = "#! function() {return '<b>' + this.x + '</b>' +  ': ' + 
                                                  this.y;} !#")
            HoursbyProfPlot$legend(enabled = FALSE)
            HoursbyProfPlot$addParams(dom = 'chart1')
            return(HoursbyProfPlot)
          })
          
          #text that shows above chart1: the number of Professors during the specified period
          output$totalNumofProf <- renderText({
            paste0("Total number of Professors that students worked on assignments 
                   for within the specified period: ", nrow(HoursByProfR()))
          })
          
          #code for downloading the csv file of HoursByProfR dataset
          output$downloadHoursByProfR <- downloadHandler(
            filename = function() { 
              paste('Hours by Professor','-',input$startDate, '-', input$endDate, '.csv', sep='') 
            },
            content = function(file) {
              write.csv(HoursByProfR(), file, row.names = FALSE)
            }
          )
    
          #reactive (-R) data set for chart13, and for download
          HoursBySubjR <- reactive({
            timeData <- subset(hpdata, hpdata$startdate >= input$startDate & hpdata$startdate <= input$endDate)
            HoursbySubj <- aggregate(timeData$hoursworked, by=list(Subject=timeData$Subject),FUN=mean, na.rm=TRUE)
            names(HoursbySubj) <- c("Subject", "Hours")
            HoursbySubj$Hours <- round(HoursbySubj$Hours, digit = 2)
            HoursbySubj <- HoursbySubj[order(-HoursbySubj$Hours),]
            HoursbySubj
          })

                    
          #bar chart showing average number of hours spent on assignments for a specific Subject
          output$chart13 <- renderChart({
            HoursbySubjPlot <- Highcharts$new()
            HoursbySubjPlot$series(data = HoursBySubjR()$Hours, type = "bar") 
            HoursbySubjPlot$title(text ="Average Number of Hours Spent on Assignments (by Subject)")
            HoursbySubjPlot$chart(width = 600, zoomType = "xy")
            HoursbySubjPlot$plotOptions(bar = list(color = "#666699", groupPadding = 0, borderWidth = 2))
            HoursbySubjPlot$yAxis(title = list(text = "Hours Spent"))  
            HoursbySubjPlot$xAxis(type = "category", categories = HoursBySubjR()$Subject, 
                                  labels = list(step = 1, style = list(fontSize= '12px')))
            HoursbySubjPlot$tooltip( formatter = "#! function() {return '<b>' + this.x + '</b>' +  ': ' + 
                                                  this.y;} !#")
            HoursbySubjPlot$legend(enabled = FALSE)
            HoursbySubjPlot$addParams(dom = 'chart13')
            return(HoursbySubjPlot)
          })
          
          #code for downloading the csv file of HoursBySubjR dataset
          output$downloadHoursBySubjR <- downloadHandler(
            filename = function() { 
              paste('Hours by Subject','-',input$startDate, '-', input$endDate, '.csv', sep='') 
            },
            content = function(file) {
              write.csv(HoursBySubjR(), file, row.names = FALSE)
            }
          )

          
############################### Second tab for Hogwarts data

          #reactive (-R) data set with hours certain Student spent on assignments
          HoursByStudentR <- reactive({
            timeData <- subset(hpdata, hpdata$startdate >= input$startDate2 & hpdata$startdate <= input$endDate2)
            HoursByStudent <- aggregate(round(timeData$hoursworked, digits = 1), by=list(Student=timeData$Student_A),FUN=sum, na.rm=TRUE)
            names(HoursByStudent) <- c("Student", "Hours")
            HoursByStudent <- HoursByStudent[order(-HoursByStudent$Hours),]
            HoursByStudent
          })

          #conditional output for chart3 of Hogwarts data: data will be displayed only if 
          #input is valid and there's enough data to display
          output$chart3withCondition <- renderUI({
            #the condition for date input is 15 days difference between start and end, and
            #also that there's enough data in the processed reactive dataset
            if (as.Date(input$endDate2) - as.Date(input$startDate2) > 15 & 
                  nrow(subset(hpdata, hpdata$startdate >= input$startDate2 & hpdata$startdate <= input$endDate2)) > 0) {
              list(
                fluidRow(
                    column(width = 6, style="min-width:800px;max-width:1000px;margin-right:20px",
                           p(downloadButton('downloadHoursByStudentR', 'Download'), align = "right"),
                           showOutput("chart3", "highcharts")))
                )
            } else {
              strong("Several dashboard elements are hidden, since not enough data 
                     are available based on provided date input.")
            }
          })


          #conditional output for zeroHours table and chart4 of Hogwarts data, data will be displayed only if 
          #input is valid and there's enough data to display
          output$chart4withCondition <- renderUI({
            #the condition for date input is 15 days difference between start and end, and
            #also that there's enough data in the processed reactive dataset for the datatable
            if (as.Date(input$endDate2) - as.Date(input$startDate2) > 15 & 
                  nrow(subset(hpdata, hpdata$updated >= input$startDate2 & hpdata$updated <= input$endDate2 
                              & hpdata$Status == "Completed")) > 0) {
              list(
                fluidRow(
                  column(width = 6, style="min-width:800px;max-width:1000px;margin-right:20px",
                         br(), br(),
                         h4("Completed Assignments with Zero Hours Spent ('cheated work')"),
                         dataTableOutput('hourstable'),
                         br(), br(),
                         h4(textOutput("numforchart4")),
                         br(), br(),
                         showOutput("chart4", "highcharts"))))
            } else {
              strong("There were no instances of 'cheated work' during the specified period of time.")
            }
          })

          #bar chart that plots hours Students spent on assignments
          output$chart3 <- renderChart({
            HoursByStudentPlot <- Highcharts$new()
            HoursByStudentPlot$series(data = HoursByStudentR()$Hours, type = "bar") 
            HoursByStudentPlot$title(text ="Total Hours Students Spent on Assignments")
            HoursByStudentPlot$xAxis(type = "category", categories = HoursByStudentR()$Student) 
            HoursByStudentPlot$yAxis(title = list(text = "Hours Spent"))
            HoursByStudentPlot$plotOptions(bar = list(color = "#669900",
                                                  groupPadding = 0, borderWidth = 3))
            HoursByStudentPlot$tooltip( formatter = "#! function() {return '<b>' + this.x + '</b>' +  ': ' + 
                                                  this.y;} !#")
            HoursByStudentPlot$legend(enabled = FALSE)
            HoursByStudentPlot$chart(height = 500)
            HoursByStudentPlot$addParams(dom = 'chart3')
            return(HoursByStudentPlot)
          })

          #code for downloading the csv file of HoursByStudentR dataset
          output$downloadHoursByStudentR <- downloadHandler(
            filename = function() { 
              paste('HoursByStudent','-',input$startDate2, '-', input$endDate2, '.csv', sep='') 
            },
            content = function(file) {
              write.csv(HoursByStudentR(), file, row.names = FALSE)
            }
          )

          #reactive (-R) data set about hours spent on assignments for hourstable, numforchart4 value and chart4 plot
          HoursByStatusR <- reactive({
            hoursbystatus <- subset(hpdata, hpdata$updated >= input$startDate2 & hpdata$updated <= input$endDate2)
            hoursbystatus <- aggregate(round(hoursbystatus$hoursworked, digits = 1), by=list(AssignID=hoursbystatus$AssignId),FUN=sum, na.rm=TRUE)
            hoursbystatus <- subset(hoursbystatus, hoursbystatus$x == 0)
            hoursbystatus<- merge(hpdata, hoursbystatus, by.x="AssignId", by.y="AssignID")
            hoursbystatus
          })
          
          #interactive table that shows completed assignments with zero hours spent (cheated assignments)
          output$hourstable <- renderDataTable({
            ZeroHoursAssignTable <- subset(HoursByStatusR(), HoursByStatusR()$Status=="Completed")
            ZeroHoursAssignTable <- data.frame(paste0(ZeroHoursAssignTable$House, "-", ZeroHoursAssignTable$Assignment),
                                               ZeroHoursAssignTable$Subject, ZeroHoursAssignTable$Student, format(ZeroHoursAssignTable$updated, format = "%m/%d/%Y"), 
                                               ZeroHoursAssignTable$Status)
            names(ZeroHoursAssignTable) <- c("Assignment ID", "Subject", "Student", "Last Updated", "Status")
            ZeroHoursAssignTable
          }, options = list(bSortClasses = TRUE, iDisplayLength = 10))
          
          
          #text that shows total number of assignments, which students spent zero hours on
          output$numforchart4 <- renderText({
            paste0("Total number of assignments with zero hours spent ('cheated work' or still in progress): ", nrow(HoursByStatusR()))
            })
          
          #pie chart that plots number of assignments with zero hours spent grouped by their status
          output$chart4 <- renderChart({
              final <- as.data.frame(table(HoursByStatusR()$Status))
              final <- subset(final, final$Freq != 0)
              zha <- hPlot(x="Var1", y = "Freq", type="pie", data=final, title = "All Assignments with Zero Hours Spent")
              zha$plotOptions(pie = list(innerSize = "50%", startAngle = -90, endAngle = 90, center = list("50%", "60%")))
              zha$addParams(height = 300, dom = 'chart4')
              return(zha)
             
          })   
          
#################################### Ministry of Magic Charts 

        #reactive data set about Ministry of Magic data used in chart 10 
        obliviate_summaryR <- reactive({
          #get the total number of times Obliviate spell per time period by department
          obliviate_summary <- subset(ministry, Spell == "Obliviate" & DateUsed>=input$ministryStartDate & DateUsed <= input$ministryEndDate)
          if (input$MMgroupBy == "Department") {
            #summarize the data by department
            obliviate_summary <- ddply(obliviate_summary, c("Department", "MemberType"), summarise, 
                                       TotalTimesUsed = sum(TimesUsed))
            obliviate_summary <- melt(dcast(obliviate_summary, Department ~ MemberType, value.var = "TotalTimesUsed", sum), id="Department")
            #clean up the names of columns
            names(obliviate_summary) <- c("Variable", "MoralType", "TotalTimesUsed")
          } else {
            #summarize the data by Official
            obliviate_summary <- ddply(obliviate_summary, c("Member", "MemberType"), summarise, 
                                       TotalTimesUsed = sum(TimesUsed))
            obliviate_summary <- melt(dcast(obliviate_summary, Member ~ MemberType, value.var = "TotalTimesUsed", sum), id="Member")
            #clean up the names of columns
            names(obliviate_summary) <- c("Variable", "MoralType", "TotalTimesUsed")
          }
          obliviate_summary
        })
                
        output$chart10withCondition <- renderUI({
          #the condition for date input is 15 days difference between start and end, and
          #also that there's enough data in the processed reactive dataset
          if (as.Date(input$ministryEndDate) - as.Date(input$ministryStartDate) > 15 
              & nrow(subset(ministry, Spell == "Obliviate" & DateUsed>=input$ministryStartDate & DateUsed <= input$ministryEndDate)) > 0) {
            showOutput("chart10", "highcharts")       
          } else {
            strong("A dashboard element is hidden, since not enough data 
                     are available based on provided date input.")
          }
        })

        #bar chart showing total number of times Obliviate spell was used during specified period, by department/official and their moral type 
        output$chart10 <- renderChart({
          #get the total number of times Obliviate spell was used during the user-specified period
          obliviate_summary <- obliviate_summaryR()
                    
          #create the chart object
          ministryPlot <- Highcharts$new()
          ministryPlot$xAxis(categories = obliviate_summary$Variable)
          
          #conditionally add the four data series, so that in case data doesn't exist, 
          #the chart doesn't throw an error  
          if (nrow(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Chaotic Evil")) > 0) {
            ministryPlot$series(name = "Chaotic Evil", data = toJSONArray2(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Chaotic Evil"), 
                                                                           json = F, names = F), type = "bar", color = "#75D1FF", legendIndex = 2)
          }
          if (nrow(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Chaotic Good")) > 0) {
            ministryPlot$series(name = "Chaotic Good", data = toJSONArray2(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Chaotic Good"), 
                                                                           json = F, names = F), type = "bar", color = "#85AD33", legendIndex = 3)
          }
          if (nrow(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Purely Evil")) > 0) {
            ministryPlot$series(name = "Purely Evil", data = toJSONArray2(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Purely Evil"), 
                                                                          json = F, names = F), type = "bar", color = "#4282A3", legendIndex = 1)
          }  
          if (nrow(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Purely Good")) > 0) {
            ministryPlot$series(name = "Purely Good", data = toJSONArray2(subset(obliviate_summary[,c(1,3)], obliviate_summary$MoralType == "Purely Good"), 
                                                                          json = F, names = F), type = "bar", color = "#4A6E00", legendIndex = 4)
          }
          #add the rest of chart parameters  
          ministryPlot$title(text =paste0("Number of Times Oblivate Spell was used by ", input$MMgroupBy))
          ministryPlot$tooltip(formatter = "#! function() { return '<b>' + this.point.name + '</b>' + '<br />' + 'For ' + this.series.name + ': ' + 
                                  this.point.y + '<br />' + 'Total for Department: '
                                  + this.total  + '<br />'; } !#")
          ministryPlot$chart(height = 500)
          ministryPlot$yAxis(title = list(text = "Number of Times Used"), minTickInterval = 1)
          ministryPlot$plotOptions(bar = list(stacking = "normal", groupPadding = 0, borderWidth = 2))
          ministryPlot$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
          ministryPlot$addParams(dom = 'chart10')
          return(ministryPlot)
        })

        #chart depicting trends in using different spells by specified departments (see ui.R for inputs)
          output$chart12 <- renderChart({
            #get the data for the specified Department (see user input in ui.R)
            ministryTrend <- subset(ministry, Department == input$MinDepartment)
            #convert the date format to JSON-friendly
            ministryTrend$DateUsed <- to_jsdate2(as.Date(ministryTrend$DateUsed))
            #plot a line chart
            ministryTrendPlot <- Highcharts$new()
            #since for this dashboard the data was simulated, every spell will appear for every department, 
            #so there shouldn't be any NA's when we subset data as below
            #the code below creates a chart series for each spell, which allows us to control 
            #their properties, like color and type
            ministryTrendPlot$series(name = "Bombarda Maxima", data = toJSONArray2(
              subset(ministryTrend[,5:6], ministryTrend$Spell == "Bombarda Maxima"), 
              json = F, names = F), type = "line", color = "#4DFFFF", marker = list(enabled = FALSE))
            ministryTrendPlot$series(name = "Deletrius", data = toJSONArray2(
              subset(ministryTrend[,5:6], ministryTrend$Spell == "Deletrius"), 
              json = F, names = F), type = "line", color = "#6699FF", marker = list(enabled = FALSE))
            ministryTrendPlot$series(name = "Levicorpus", data = toJSONArray2(
              subset(ministryTrend[,5:6], ministryTrend$Spell == "Levicorpus"), 
              json = F, names = F), type = "line", color = "#33CC33", marker = list(enabled = FALSE))
            ministryTrendPlot$series(name = "Obliviate", data = toJSONArray2(
              subset(ministryTrend[,5:6], ministryTrend$Spell == "Obliviate"), 
              json = F, names = F), type = "line", color = "#006B8F", marker = list(enabled = FALSE))
            ministryTrendPlot$title(text = "Historical Spell Use Trend (Grouped by Department)")
            ministryTrendPlot$xAxis(type='datetime', title = list(text = "Time"))
            ministryTrendPlot$yAxis(title = list(text = "Number of Times Spell Was Used"), 
                              labels = list(style=list(color= '#000066', fontWeight= 'bold')),
                              min = 0)
            ministryTrendPlot$plotOptions(series = list(color = "#333366"))
            ministryTrendPlot$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
            ministryTrendPlot$addParams(dom = 'chart12')
            return(ministryTrendPlot)
          })

############################### rCharts for the main page

          #html output object for the main page containing the 3 charts below
          output$chart5and6and2 <- renderUI({
            #using renderUI in this case was hardly justified, however, deemed necessary due to the fact
            #that putting this code in ui.R for some reason didn't work
            list(
              fluidRow(
                column(width = 3, style="min-width:350px;max-width:375px;margin-left:340px",
                       showOutput("chart5", "highcharts")
                ),
                column(width = 3, style="min-width:350px;max-width:375px",
                       showOutput("chart6", "highcharts")
                )),
              fluidRow(
                column(width = 6, offset = 3, style="min-width:760px;max-width:760px;margin-left:340px",
                       showOutput("chart2", "highcharts") 
                )))
          })
          
          #column chart that plots good vs. evil professors at Hogwarts during 1991-1998
          output$chart5 <- renderChart({
            #single out the professors and their moral type
            professorTypes <- join(data.frame(Professor = unique(hpdata$Professor)),
                 hpdata[,c("Professor", "ProfType")], by = "Professor", 
                 type = "left", match = "first")
            #count the number of each moral type professors
            professorTypes <- as.data.frame(table(as.character(professorTypes$ProfType)))
            #rename the columns
            names(professorTypes) <- c("MoralType", "Freq")
            professorTypesPlot <- Highcharts$new()
            professorTypesPlot$series(data = professorTypes$Freq, type = "column") 
            professorTypesPlot$title(text ="Hogwarts Professors (1991-1998)")
            professorTypesPlot$xAxis(type = "category", categories = professorTypes$MoralType) 
            professorTypesPlot$yAxis(title = list(text = "Number"))
            professorTypesPlot$plotOptions(column = list(color = "#669900",
                                                      groupPadding = 0, borderWidth = 3))
            professorTypesPlot$tooltip(formatter = "#! function() {return '<b>' + this.x + '</b>' +  ': ' + 
                                     this.y;} !#")
            professorTypesPlot$legend(enabled = FALSE)
            professorTypesPlot$chart(height = 300, width = 375)
            professorTypesPlot$addParams(dom = 'chart5')
            return(professorTypesPlot)
            })
          
            #column chart showing top 3 most difficult Subjects (based on average 
            #type spent on homework assignments)
            output$chart6 <- renderChart({
              #aggregate the data
              topHardestSubj <- ddply(hpdata, "Subject",summarise, 
                                      Hours = round(mean(hoursworked, na.rm = TRUE), digit = 2))
              #sort in descending order
              topHardestSubj <- topHardestSubj[order(-topHardestSubj$Hours),]
              #build the chart
              topHardestSubjPlot <- Highcharts$new()
              topHardestSubjPlot$series(data = head(topHardestSubj$Hours, 3), type = "column") 
              topHardestSubjPlot$title(text ="Top Most Difficult Subjects")
              topHardestSubjPlot$subtitle(text = "Based on Average Number of Hours Spent on Assignments")
              topHardestSubjPlot$chart(height = 300, width = 375)
              topHardestSubjPlot$plotOptions(column = list(color = "#666699"), groupPadding = 0, borderWidth = 2)
              topHardestSubjPlot$yAxis(title = list(text = "Hours Spent"))  
              topHardestSubjPlot$xAxis(type = "category", categories = head(topHardestSubj$Subject, 3), 
                                    labels = list(step = 1, style = list(fontSize= '12px')))
              topHardestSubjPlot$tooltip( formatter = "#! function() {return '<b>' + this.x + '</b>' +  ': ' + 
                                       this.y;} !#")
              topHardestSubjPlot$legend(enabled = FALSE)
              topHardestSubjPlot$addParams(dom = 'chart6')
              return(topHardestSubjPlot)
              })

              #reactive (-R) data set of butterbeer sales and students' workload, used in chart 2
              salesAndHoursR <- reactive({
                #aggregate the hours students spent on studying, summing them across months and then averaging across years
                monthlyHours <- ddply(hpdata, "monthYear", summarise, TotalHours = sum(hoursworked))
                monthlyHours$MonthOnly <- substr(monthlyHours$monthYear, 1,3)
                monthlyHours <- ddply(monthlyHours, "MonthOnly", summarise, 
                                      AveTotalHours = round(mean(TotalHours,na.rm = TRUE), digit = 2))
                monthlySales <- ddply(butterbeer, "MonthOnly", summarise, 
                                      AveBeerSales = round(mean(Sales,na.rm = TRUE), digit = 2))
                monthlySales$MonthOnly <- substr(monthlySales$MonthOnly, 1,3)
                #merge the two data sets together
                salesAndHours <- merge(monthlyHours, monthlySales, all.y = TRUE)
                #sort the dataframe by month, using order method on a fake date, created based on the abbreviated month
                salesAndHours$fakeDate <- as.Date(paste0("01/", salesAndHours$MonthOnly,"/1991"), format = "%d/%b/%Y")
                salesAndHours <- arrange(salesAndHours,fakeDate)
                salesAndHours
              })
              
              #chart showing the relationship between butterbeer sales in Hogsmead and students' workload
              #this chart will be displayed differently in shinyapps.io as opposed to your local Shiny Server or local RStudio
              #the reason being platform differences and how JavaScript code is interpreted in the back. 
              output$chart2 <- renderChart({
                #plot the relationship
                salesAndHoursPlot <- Highcharts$new()
                salesAndHoursPlot$xAxis(type = "category", categories = salesAndHoursR()$MonthOnly,
                                        labels = list(style = list(fontSize= '12px')), 
                                        tickInterval = 1)
                salesAndHoursPlot$series(name = "Total Hours", data = salesAndHoursR()$AveTotalHours, type = "column") 
                salesAndHoursPlot$series(name = "Butterbeer Sales", data = salesAndHoursR()$AveBeerSales, type = "spline", yAxis = 1) 
                salesAndHoursPlot$title(text ="Average Total Number of Hours Students Spent on Assignments vs. Butterbeer Sales (in Galleons)")
                salesAndHoursPlot$chart(height = 450, width = 760, marginRight= 65)
                salesAndHoursPlot$tooltip( formatter = "#! function() {
                                           return '<b>' + this.x + '</b>' +  '<br />' + 
                                           this.points[0].series.name + ': '  + this.points[0].y + '<br />' +
                                           this.points[1].series.name + ': '  + this.points[1].y;} !#", shared = TRUE)
                salesAndHoursPlot$plotOptions(series = list(stacking = "normal"))
                salesAndHoursPlot$yAxis(list(
                  list(title = list(text = 'Total Hours'), tickInterval = 10), 
                  list(title = list(text = 'Butterbeer Sales'), opposite=TRUE, min = 0, tickInterval = 250)))
                salesAndHoursPlot$legend(align = 'center', verticalAlign = 'top', y = 45, margin = 30, enabled = TRUE)
                salesAndHoursPlot$colors("#4572A7", "#89A54E")
                salesAndHoursPlot$addParams(dom = 'chart2')
                return(salesAndHoursPlot)
                })
              
              
}
)