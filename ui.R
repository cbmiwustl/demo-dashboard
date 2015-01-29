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
#the line below is run in local RStudio only to pull the latest jQuery libraries for rCharts. 
#if a conflict occurs and some charts don't display set this to FALSE both here and in ui.R
#do not run this line if you upload to Shinyapps.io, no plots will appear
#options(rcharts.cdn = TRUE)

#main Shiny Server function for UI

#this dashboard uses an adjusted version of bootstrap.css file, the theme is called "Cerulean"; 
#the original was obtained at this link: http://bootswatch.com/2/cerulean/

shinyUI(navbarPage("Demo Dashboard",theme = "bootstrap.css",
    tabPanel("Main",
             htmlOutput("chart5and6and2")
    ),               
    navbarMenu("Hogwarts",
       tabPanel("Hours (Professors & Subjects)",  
        fluidRow(
          column(width = 3,
            h4("Hours (Professors & Subjects)"),
            p("This page shows hours spent on assignments for specific Professors and Subjects"),
            br()),
          column(width = 3, 
            br(),
            dateInput("startDate", label = strong("Start Date"), value = as.Date("1991-09-01"), 
                           format = "M d, yyyy", min = "1991-09-01", max = "1998-05-28")),
          column(width = 3,
            br(),
            dateInput("endDate", label = strong("End Date (Inclusive)"), value = as.Date("1998-05-28"), 
                      format = "M d, yyyy", min = "1991-09-01", max = "1998-05-28")),
          column(width = 2,
            br(),
            helpText(paste0("Hogwarts Data - Last Updated: ", 
                                 format(as.Date(file.info("data/hpdata.csv")$mtime), format = "%m/%d/%Y"))))
          
        ),
        htmlOutput("chart1and13withCondition")
      ),
      tabPanel("Hours (Students)",
        sidebarLayout(
          sidebarPanel(
            h3("Hours (Students)"),
            p("This page shows hours students spent on assignments during the period
                     specified below."),
            dateInput("startDate2", label = strong("Start Date"), value = as.Date("1991-09-01"), 
                      format = "M d, yyyy", min = "1991-09-01", max = "1998-05-28"),
            dateInput("endDate2", label = strong("End Date (Inclusive)"), value = as.Date("1998-05-28"), 
                      format = "M d, yyyy", min = "1991-09-01", max = "1998-05-28"),
            br(),
            helpText(paste0("Hogwarts Data - Last Updated: ", 
                            format(as.Date(file.info("data/hpdata.csv")$mtime), format = "%m/%d/%Y")))
          ),
          mainPanel(
            htmlOutput("chart3withCondition"),
            htmlOutput("chart4withCondition")
          )
        )
      )
    ),
    navbarMenu("Ministry of Magic",
      tabPanel("Ministry of Magic Departments",
             sidebarLayout(
               sidebarPanel(
                 h3("Ministry of Magic Departments"),
                 p("This page shows the usage of Obliviate Spell (erases a person's memory of an event)
                          by officials of the Ministry of Magic during the period specified below."),
                 dateInput("ministryStartDate", label = strong("Start Date"), value = as.Date("1994-09-01"), 
                           format = "M d, yyyy", min = "1994-09-01", max = "1997-05-28"),
                 dateInput("ministryEndDate", label = strong("End Date (Inclusive)"), value = as.Date("1997-05-28"), 
                           format = "M d, yyyy", min = "1994-09-01", max = "1997-05-28"),
                 selectInput("MMgroupBy", label = strong("Group results by: "), 
                             choices = c("Department" = "Department", "Official" = "Member")),
                 br(),
                 helpText(paste0("Ministry of Magic Data - Last Updated: ", 
                                  format(as.Date(file.info("data/ministry.csv")$mtime), format = "%m/%d/%Y")))
               ),
               mainPanel(
                 htmlOutput("chart10withCondition")
               )
             )   
      ),
      tabPanel("Ministry of Magic Trends",
             sidebarLayout(
               sidebarPanel(
                 h3("Ministry of Magic Trends"),
                 p("This page shows specific spell usage by Ministry of Magic over time"),
                 selectInput("MinDepartment", label = strong("Select Department for Historic Graph"), 
                             choices = sort(unique(ministry$Department))),
                 helpText("To zoom in on an area, draw a horizontal line on the chart with your mouse, to reset - hit 'Reset Zoom'"),
                 h4("Explanation of Spells"),
                 strong("Bombarda Maxima"),p("- creates a large explosion 
                                                     capable of removing entire walls"),
                 strong("Deletrius"),p("- eradication spell"),
                 strong("Levicorpus"),p("- the victim is dangled upside-down by their 
                                               ankles, sometimes accompanied by a flash of light"),
                 strong("Obliviate"),p("-  used to hide a memory of a particular event"),
                 br(),
                 helpText(paste0("Ministry of Magic Data - Last Updated: ", 
                                 format(as.Date(file.info("data/ministry.csv")$mtime), format = "%m/%d/%Y")))
                 ),
               mainPanel(
                 div(class='wrapper',
                     tags$style(".highcharts{ height: 100%; width: 800px;}"), showOutput("chart12", "highcharts"))
               )
             )   
      )
    ),
    tabPanel("About",
          fluidRow(
               column(width = 6, offset = 3,
                 h3("Center for Biomedical Informatics at Washington University in St. Louis",
                    align = "center"),
                 p("The Center for Biomedical Informatics (CBMI) at Washington University in St. Louis is led by 
                    a group of experienced bioinformatics experts and integrates elements of Medical Informatics, 
                    Bioinformatics and Computational Sciences. It offers industry-standard, enterprise-class IT 
                    infrastructure and software tools to store, integrate, query, analyze, and visualize complex 
                    clinical and molecular data sets. CBMI provides essential data management/analysis and 
                    collaborative tools and comprehensive training resources to promote collaborative studies 
                    and facilitate the identification of diagnostic and prognostic biomarkers and the subsequent 
                    development of personalized therapies. There are three major applications developed and 
                    implemented by CBMI: ClinPortal, a clinical research data management system; OpenSpecimen, a 
                    biospecimen management and tracking system; and CIDER (Clinical Investigation and Exploration 
                    Repository), a medical record data mining tool.  In addition to these, CBMI has developed 
                    several other tools/solutions such as the ICTS Service Tracker, eNavigator, BioMS, 
                    NCTN Navigator, etc. CBMI has established an Agile 
                    development methodology which is well supported by processes and tools. For more information about CBMI,
                    visit the ", a("CBMI website.", href = "http://cbmi.wustl.edu/"), align = "justify"),
                 h3("Demo Dashboard", align = "center"),
                 p("This sample dashboard was developed as a simplified version of the internal 
                    dashboard that CBMI uses in its operations. The demo dashboard shows metrics related 
                    to Hogwarts School of WitchCraft and 
                    Wizardry and the Ministry of Magic in England. The data were inspired
                    by the Harry Potter series by J.K. Rowling, however, the dashboard doesn't relfect any details from the 
                    books. The dashboard was developed using R, Shiny and 
                    rCharts. It contains interactive charts and tables and takes
                    input from users. Please try it out for yourself and let us
                    know what you think at helpATbmi.wustl.edu.", align = "justify"),                 
                 br(),
                 p(a(strong("Source code"), href = "https://github.com/cbmiwustl/demo-dashboard"),
                        " for the dashboard is available on GitHub."), 
                 p("Main Reference: 
                   Rowling, J.K.. Harry Potter Series. New York: Scholastic, 1997-2007. Print. ", align = "justify"),
                 p("Much credit is also given to Harry Potter Wikia: 
                          http://harrypotter.wikia.com/wiki.", align = "justify"),
                 p(strong("Browser and Screen Recommendations: "), "this dashboard is fully compatible with 
                     Mozilla Firefox and Chrome browsers on a Windows machine, and Mozilla Firefox, Chrome, and 
                     Safari on a Mac machine. Optimal resolution is 1440x900 (but will display ok on others). 
                     Please expand the browser window for proper viewing of the dashboard"),
                 h4("Funding", align = "center"),
                 p("Funding for this project was provided by the Washington University Institute of Clinical and 
                   Translational Sciences grant UL1TR000448 from the National Center for Advancing Translational 
                   Sciences (NCATS) of the National Institutes of Health (NIH).  The content is solely the responsibility 
                   of the authors and does not necessarily represent the official view of the NIH. The project was 
                   also supported by funding from Alvin J. Siteman Cancer Center, Grant # P30CA091842. All authors 
                   from Washington University School of Medicine declare no competing interests related to this work."),
                 br(),br()

             ))
    )
  ))