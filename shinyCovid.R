library(shiny)
library(data.table)
library(dplyr)
library(stats)
library(ggpubr)
library(caret)
library(precrec)
library(readxl)
library(EnvStats)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(RCurl)
library(randomForest)
library(fastDummies)
library(ggplot2)
library(multiROC)
library(pROC)
library(GGally)
library(reshape2)
library(DT)
library(reactable)
library(gghalves)
library(markdown)
library(shinyjs)

set.seed(123)


DMPs_model <-
  readRDS(url(
    'https://github.com/lehieu2307/ShinyAppCovid/raw/main/DMPs_model.rds'
  ))
formula_standard <-
  readRDS(url(
    'https://github.com/lehieu2307/ShinyAppCovid/raw/main/COMER_equation.rds'
  ))
cutoff_standard <- formula_standard$cutoff
formula_extend <-
  readRDS(
    url(
      'https://github.com/lehieu2307/ShinyAppCovid/raw/main/COMER_age_adjustment_equation.rds'
    )
  )
cutoff_extend <- formula_extend$cutoff

prgoressBar <-
  function(value = 0,
           label = FALSE,
           color = "red",
           size = NULL,
           striped = FALSE,
           active = FALSE,
           vertical = FALSE) {
    stopifnot(is.numeric(value))
    if (value < 0 || value > 100)
      stop("'value' should be in the range from 0 to 100.", call. = FALSE)
    if (!(
      color %in% shinydashboard:::validColors ||
      color %in% shinydashboard:::validStatuses
    ))
      stop("'color' should be a valid status or color.", call. = FALSE)
    if (!is.null(size))
      size <- match.arg(size, c("sm", "xs", "xxs"))
    text_value <- paste0(value, "%")
    if (vertical)
      style <-
      htmltools::css(height = text_value, `min-height` = "2em")
    else
      style <-
      htmltools::css(width = text_value, `min-width` = "2em")
    tags$div(
      class = "progress",
      class = if (!is.null(size))
        paste0("progress-", size),
      class = if (vertical)
        "vertical",
      class = if (active)
        "active",
      tags$div(
        class = "progress-bar",
        class = paste0("progress-bar-", color),
        class = if (striped)
          "progress-bar-striped",
        style = style,
        role = "progressbar",
        `aria-valuenow` = value,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        tags$span(class = if (!label)
          "sr-only", text_value),
        tags$style(
          paste0(".progress-bar-", color, " {background-color: ", color, ";}")
        )
      ),
      tags$span(class = "progress-number", paste(format(
        value / 100, nsmall = 2
      ))),
    )
  }

####################################
# UI                               #
####################################
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  navbarPage(
    "COVID-19 Severity Prediction",
    tabPanel(
      "Guideline",
      titlePanel("User guideline"),
      div(
        includeMarkdown(
          "https://github.com/lehieu2307/ShinyAppCovid/raw/main/guildline.md"
        ),
        align = "justify"
      ),
      
    ),
    tabPanel(
      "Multiple Prediction",
      sidebarPanel(
        HTML("<h3>Input parameters</h3>"),
        strong(HTML("1. Click here to download example data:")),
        
        # uiOutput(outputId = "align_guide"),
        p(),
        downloadButton("downloadData", "Download example data"),
        
        br(),
        br(),
        # input file
        fileInput("file_data_multiple", "2. Choose TSV File:", accept = ".tsv"),
        fluidRow(column(
          4,
          actionButton("buttonPreMultiple", "Prediction", class = "btn btn-primary")
        ),),
      ),
      br(),
      br(),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          style = "width:100%",
          
          DT::dataTableOutput('userdata_table')
        )
      ),
      br(),
      uiOutput("without_age_text"),
      fluidRow(
        column(width = 6,
               plotlyOutput("graph")),
        column(
          width = 6,
          height = 400,
          DT::dataTableOutput("plot2")
        )
      ),
      uiOutput("with_age_text"),
      fluidRow(
        column(width = 6,
               plotlyOutput("graph2")),
        column(
          width = 6,
          height = 400,
          DT::dataTableOutput("plot3")
        )
      )
      
    ),
    tabPanel(
      "Single Prediction",
      # Input values
      sidebarPanel(
        HTML("<h3>Input parameters</h3>"),
        strong(HTML("1. Click here to download example data:")),
        p(),
        downloadButton("downloadData2", "Download example data"),
        
        br(),
        br(),
        # input file
        fileInput("file_Single", "2. Choose TSV File:", accept = ".tsv"),
        
        actionButton("buttonSinglePre", "Prediction", class = "btn btn-primary")
      ),
      column(
        12,
        align = "center",
        DT::dataTableOutput('row_modif'),
        
        fluidRow(
          column(2, align = "center", ),
          column(
            8,
            align = "center",
            uiOutput("instruct_text"),
            br(),
            fluidRow(
              column(2,
                     align = "right",
                     div(uiOutput("avatarUser"), align = "right"),
                     br(),
                     br(),),
              column(
                6,
                align = "left",
                br(),
                div(textOutput('content_name'), align = "left"),
                tags$head(
                  tags$style("#content_name{font-size: 32px;
                                                 font-style: Georgia;
                                                 }")
                ),
                br(),
                div(textOutput('content_age'), align = "left"),
                tags$head(
                  tags$style("#content_age{font-size: 32px;
                                                 font-style: Georgia;
                                                 }")
                ),
              )
            ),
            
            fluidRow(
              column(4, align = "center",
                     fluidRow(
                       column(
                         12,
                         align = "center",
                         align = "center",
                         
                         div(textOutput('content_cpg01'), align = "center"),
                         tags$head(
                           tags$style("#content_cpg01{font-size: 20px;
                                                              font-style: Georgia;
                                                              }")
                         ),
                         uiOutput("plot_cpg01"),
                         br(),
                         
                         div(textOutput('content_cpg02'), align = "center"),
                         tags$head(
                           tags$style("#content_cpg02{font-size: 20px;
                                                              font-style: Georgia;
                                                              }")
                         ),
                         uiOutput("plot_cpg02"),
                         br(),
                         
                         div(textOutput('content_cpg03'), align = "center"),
                         tags$head(
                           tags$style("#content_cpg03{font-size: 20px;
                                                              font-style: Georgia;
                                                              }")
                         ),
                         uiOutput("plot_cpg03"),
                         br(),
                         
                       ),
                     ),),
              
              column(2, align = "center",
                     uiOutput("image_chr"),),
              column(
                6,
                br(),
                
                div(textOutput('content_cpg04'), align = "center"),
                tags$head(
                  tags$style("#content_cpg04{font-size: 20px;
                                              font-style: Georgia;
                                              }")
                ),
                br(),
                div(textOutput('content_cpg05'), align = "center"),
                tags$head(
                  tags$style("#content_cpg05{font-size: 20px;
                                              font-style: Georgia;
                                              }")
                ),
                br(),
                
                div(textOutput('content_cpg06'), align = "center"),
                tags$head(
                  tags$style("#content_cpg06{font-size: 25px;
                                              font-style: Georgia;
                                              }")
                )
              )
            ),
            fluidRow(column(4,),
                     column(2,
                            uiOutput(
                              "uibuttonwithoutage"
                            )),
                     column(2,
                            uiOutput("uibuttonwithage"))),
            br(),
            plotlyOutput("dotplot_prediction"),
            br(),
          ),
        ),
      ),
    ),
    
    tabPanel(
      "About",
      titlePanel("About the study"),
      div(
        includeMarkdown(
          "https://github.com/lehieu2307/ShinyAppCovid/raw/main/about_covid.md"
        ),
        align = "justify"
      ),
    ),
  )
)


####################################
# Server                           #
####################################

server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  dotplot1 <- reactive({
    testdata <- testdata()
    req(input$file_data_multiple)
  })
  
  fileUploadedSingle <- reactiveVal(FALSE)
  observeEvent(input$file_Single, {
    fileUploadedSingle(TRUE)
  })
  
  
  fileUploadedMultiple <- reactiveVal(FALSE)
  observeEvent(input$file_data_multiple, {
    fileUploadedMultiple(TRUE)
  })
  
  input_file_Single <- reactive({
    file <- input$file_Single
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "tsv", "Please upload a tsv file"))
    test <- as.data.frame(fread(file$datapath))
    test
  })
  input_file <- reactive({
    file <- input$file_data_multiple
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "tsv", "Please upload a tsv file"))
    test <- as.data.frame(fread(file$datapath))
    test
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "COMERscore_shinyapp_example.tsv"
    },
    content = function(file) {
      url <-
        "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/COMERscore_shinyapp_example.tsv"
      download.file(url, destfile = file, method = "auto")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "COMERscore_shinyapp_example.tsv"
    },
    content = function(file) {
      url <-
        "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/COMERscore_shinyapp_example.tsv"
      download.file(url, destfile = file, method = "auto")
    }
  )
  
  observeEvent(input$buttonSinglePre, {
    if (!fileUploadedSingle()) {
      showNotification("Please upload TSV file before running predictions.", type = "warning")
    } else {
      test <- input_file_Single()
      fullDT <-
        as.data.frame(fread(
          paste0(
            "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/covid_DT_fullset.tsv"
          )
        ))
      fullDT[, c("COMER_standard", "COMER_extend")] <-
        round(fullDT[, c("COMER_standard", "COMER_extend")], 2)
      
      
      data <- test[, c("sample_ID", "sex", "age", DMPs_model)]
      data[, DMPs_model] <- round(data[, DMPs_model], 2)
      
      
      
      test_button_01 <- data
      test_button_01$COMER_score <-
        predict(formula_standard, test_button_01)
      test_button_01$COMER_score <-
        round(test_button_01$COMER_score, 2)
      
      test_button_01$Prediction <-
        ifelse(test_button_01$COMER_score > cutoff_standard,
               "Severe",
               "Non-severe")
      
      
      output$content_01 <-
        renderText(test_button_01[1, DMPs_model[1]])
      cpg01 <- test_button_01[1, DMPs_model[1]] * 100
      cpg02 <- test_button_01[1, DMPs_model[2]] * 100
      cpg03 <- test_button_01[1, DMPs_model[3]] * 100
      
      
      output$plot_cpg01 <- renderUI({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        cpg01 <- datarow[1, DMPs_model[1]] * 100
        prgoressBar(cpg01,
                    color = "primary",
                    striped = TRUE,
                    active = TRUE)
      })
      
      
      # output with names
      output$content_name <- renderText({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        paste("ID: ",
              data[input$row_modif_rows_selected, "sample_ID"])
      })
      
      # output with age
      output$content_age <- renderText({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        # data[input$row_modif_rows_selected,"age"]
        sprintf("Age: %s", data[input$row_modif_rows_selected, "age"])
        
      })
      
      
      
      output$content_cpg01 <- renderText({
        req(input$row_modif_rows_selected)
        # sprintf("Beta value of %s", DMPs_model[1])
        paste("Beta value",
              '\u03B21',
              "(",
              DMPs_model[1],
              ")")
      })
      output$content_cpg02 <- renderText({
        req(input$row_modif_rows_selected)
        # sprintf("Beta value of %s", DMPs_model[2])
        paste("Beta value",
              '\u03B22',
              "(",
              DMPs_model[2],
              ")")
      })
      output$content_cpg03 <- renderText({
        req(input$row_modif_rows_selected)
        # sprintf("Beta value of %s", DMPs_model[3])
        paste("Beta value",
              '\u03B23',
              "(",
              DMPs_model[3],
              ")")
      })
      
      output$content_cpg04 <- renderText({
        req(input$row_modif_rows_selected)
        if (id_button$number_button == 1) {
          paste(
            "COMER score = 6.9651  -  3.2713 ×",
            '\u03B21',
            "-  6.6951 ×",
            '\u03B22' ,
            "- 6.3909 x",
            '\u03B23'
          )
          
        } else if (id_button$number_button == 2) {
          paste(
            "COMER score = 6.589859 + 0.004134 x α - 3.235885 ×",
            '\u03B21',
            "-  6.378156 ×",
            '\u03B22' ,
            "- 6.233536 x",
            '\u03B23'
          )
        }
      })
      
      output$content_cpg05 <- renderText({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        if (id_button$number_button == 1) {
          paste(
            "COMER score = 6.9651  -  3.2713 ×",
            round(datarow[1, DMPs_model[1]], 3),
            "-  6.6951 ×",
            round(datarow[1, DMPs_model[2]], 3),
            "- 6.3909 x",
            round(datarow[1, DMPs_model[3]], 3)
          )
          
        } else if (id_button$number_button == 2) {
          paste(
            "COMER score = 6.589859 + 0.004134 x ",
            data[input$row_modif_rows_selected, "age"],
            "-  3.235885 ×",
            round(datarow[1, DMPs_model[1]], 3),
            "-  6.378156 ×",
            round(datarow[1, DMPs_model[2]], 3),
            "- 6.233536 x",
            round(datarow[1, DMPs_model[3]], 3)
          )
        }
      })
      
      
      output$content_cpg06 <- renderText({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected, ]
        if (id_button$number_button == 1) {
          result_score <-
            6.9651 - 3.2713 * datarow[1, DMPs_model[1]] -  6.6951 * datarow[1, DMPs_model[2]] - 6.3909 *
            datarow[1, DMPs_model[3]]
          result_score <-
            format(round(result_score, 2), nsmall = 2)
          
          paste("COMER score = ",
                result_score)
        } else if (id_button$number_button == 2) {
          result_score <-
            6.589859 + 0.004134 * data[input$row_modif_rows_selected, "age"]  - 3.235885 * datarow[1, DMPs_model[1]] -  6.378156 * datarow[1, DMPs_model[2]] - 6.233536 *
            datarow[1, DMPs_model[3]]
          result_score <-
            format(round(result_score, 2), nsmall = 2)
          paste("COMER score = ",
                result_score)
        }
        
      })
      
      
      output$avatarUser <- renderUI({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        index_sex <- data[input$row_modif_rows_selected, "sex"]
        
        if (index_sex == "M") {
          tags$img(src = "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/male.png",
                   height = 140,
                   align = "center")
        } else{
          tags$img(src = "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/female.png",
                   height = 140,
                   align = "center")
        }
      })
      
      output$image_chr <- renderUI({
        req(input$row_modif_rows_selected)
        tags$img(src = "https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/image.png",
                 height = 237,
                 align = "center")
      })
      
      output$row_modif <- renderDT({
        data[, DMPs_model[1]] <- format(data[, DMPs_model[1]], nsmall = 2)
        data[, DMPs_model[2]] <-
          format(data[, DMPs_model[2]], nsmall = 2)
        data[, DMPs_model[3]] <-
          format(data[, DMPs_model[3]], nsmall = 2)
        
        datatable(
          data,
          selection = "single",
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;', 'Scores in details and prediction') ,
          options = list(
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            )),
            scrollX = TRUE,
            scrollY = "250px",
            
            lengthMenu = list(c(5, 10, 30, 50, -1),
                              c('5', '10', '30', '50', 'All'))
          )
        )
      })
      
      output$plot_cpg01 <- renderUI({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        cpg01 <- datarow[1, DMPs_model[1]] * 100
        prgoressBar(cpg01,
                    color = "primary",
                    striped = TRUE,
                    active = TRUE)
      })
      
      output$plot_cpg02 <- renderUI({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        cpg02 <- datarow[1, DMPs_model[2]] * 100
        prgoressBar(cpg02,
                    color = "primary",
                    striped = TRUE,
                    active = TRUE)
      })
      
      output$plot_cpg03 <- renderUI({
        req(input$row_modif_rows_selected)
        datarow <- data[input$row_modif_rows_selected,]
        cpg03 <- datarow[1, DMPs_model[3]] * 100
        prgoressBar(cpg03,
                    color = "primary",
                    striped = TRUE,
                    active = TRUE)
      })
      
      output$uibuttonwithoutage <- renderUI({
        req(input$row_modif_rows_selected)
        if (input$row_modif_rows_selected) {
          actionButton(inputId = "buttonwithoutage",
                       label = "Plot without age",
                       class = "btn-primary")
        } else {
          NULL
        }
      })
      
      output$uibuttonwithage <- renderUI({
        req(input$row_modif_rows_selected)
        if (input$row_modif_rows_selected) {
          actionButton(inputId = "buttonwithage", label = "Plot with age")
        } else {
          NULL
        }
      })
      
      id_button <- reactiveValues(number_button = 1)
      observe({
        addClass("buttonwithoutage", "btn-primary")
        id_button$number_button = 1
      })
      
      observeEvent(input$buttonwithoutage, {
        req(input$row_modif_rows_selected)
        removeClass("buttonwithage", "btn-primary")
        addClass("buttonwithoutage", "btn-primary")
        id_button$number_button = 1
        # r$my_color <- "red"
        
      })
      
      observeEvent(input$buttonwithage, {
        req(input$row_modif_rows_selected)
        removeClass("buttonwithoutage", "btn-primary")
        addClass("buttonwithage", "btn-primary")
        id_button$number_button = 2
        # r$my_color <- "blue"
      })
      
      output$distPlot <- renderPlot({
        if (id_button$number_button == 1) {
          x <- faithful[, 2]
          bins <- seq(min(x), max(x))
          hist(x, breaks = bins, col = "red")
        } else if (id_button$number_button == 2) {
          x <- faithful[, 2]
          bins <- seq(min(x), max(x))
          hist(x, breaks = bins, col = "blue")
        }
      })
      
      output$dotplot_prediction <- renderPlotly({
        req(input$row_modif_rows_selected)
        userCustom <- data[input$row_modif_rows_selected,]
        userCustom$COMER_standard <-
          predict(formula_standard, userCustom)
        userCustom$predictive_status_standard <-
          ifelse(userCustom$COMER_standard > cutoff_standard,
                 "Severe",
                 "Non-severe")
        
        userCustom$COMER_extend <-
          predict(formula_extend, userCustom)
        userCustom$predictive_status_extend <-
          ifelse(userCustom$COMER_extend > cutoff_extend,
                 "Severe",
                 "Non-severe")
        
        if (id_button$number_button == 1) {
          g_standard <- ggplot(fullDT, aes(x = COMER_standard)) +
            theme_light() +
            stat_density(
              geom = "line",
              position = "identity",
              color = "darkred",
              lwd = 0.7
            ) +
            geom_dotplot(
              aes(color = predictive_status_standard,
                  fill = predictive_status_standard),
              alpha = 0.8,
              stroke = 0.5,
              binwidth = 0.04,
              dotsize = 1,
              method = 'histodot'
            ) +
            geom_segment(
              data = userCustom,
              aes(
                x = COMER_standard,
                xend = COMER_standard,
                y = 0,
                yend = 1
              ),
              color = "black",
              linewidth = 0.7,
              linetype = "twodash"
            ) +
            geom_point(
              data = userCustom,
              aes(
                x = round(COMER_standard, 2),
                y = 0,
                text = paste0("COMER score: ", sprintf("%.2f", COMER_standard))
              ),
              shape = 23,
              stroke = 1,
              color = "black",
              fill = "#008080",
              alpha = 1,
              size = 6
            ) +
            scale_fill_manual(values = c("#CC79A7", "#FC4E07")) +
            scale_color_manual(values = c("#CC79A7", "#FC4E07")) +
            labs(
              title = "COMER score distribution",
              fill = "",
              color = "",
              x = "COMER score",
              y = "Density"
            ) +
            theme(
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15),
              legend.position = "none"
            ) +
            annotate(
              "text",
              x = c(-2.5, round(userCustom$COMER_standard, 2), 4.6),
              y = c(0.8,-0.1, 0.8),
              size = c(6, 4, 6),
              label = c("Non-severe", "Current patient", "Severe"),
              color = c("#CC79A7", "#008080", "#FC4E07")
            )
          ggplotly(g_standard, tooltip = list("text"))
        } else if (id_button$number_button == 2) {
          g_age_adj <- ggplot(fullDT, aes(x = COMER_extend)) +
            theme_light() +
            stat_density(
              geom = "line",
              position = "identity",
              color = "darkred",
              lwd = 0.7
            ) +
            geom_dotplot(
              aes(color = predictive_status_standard,
                  fill = predictive_status_standard),
              alpha = 0.8,
              stroke = 0.5,
              binwidth = 0.04,
              dotsize = 1,
              method = 'histodot'
            ) +
            geom_segment(
              data = userCustom,
              aes(
                x = COMER_extend,
                xend = COMER_extend,
                y = 0,
                yend = 1
              ),
              color = "black",
              linewidth = 0.7,
              linetype = "twodash"
            ) +
            geom_point(
              data = userCustom,
              aes(
                x = round(COMER_extend, 2),
                y = 0,
                text = paste0(
                  "Age adjusted COMER score: ",
                  sprintf("%.2f", COMER_extend)
                )
              ),
              shape = 23,
              stroke = 1,
              color = "black",
              fill = "#008080",
              alpha = 1,
              size = 6
            ) +
            scale_fill_manual(values = c("#CC79A7", "#FC4E07")) +
            scale_color_manual(values = c("#CC79A7", "#FC4E07")) +
            labs(
              title = "Age adjusted COMER score distribution",
              size = 40,
              fill = "",
              color = "",
              x = "Age adjusted COMER score",
              y = "Density"
            ) +
            theme(
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15),
              legend.position = "none"
            ) +
            annotate(
              "text",
              x = c(-2.5, round(userCustom$COMER_standard, 2), 4.6),
              y = c(0.8,-0.1, 0.8),
              size = c(6, 4, 6),
              label = c("Non-severe", "Current patient", "Severe"),
              color = c("#CC79A7", "#008080", "#FC4E07")
            )
          ggplotly(g_age_adj, tooltip = list("text"))
        }
      })
      
      output$instruct_text <- renderUI({
        if (is.null(input$row_modif_rows_selected) ||
            length(input$row_modif_rows_selected) == 0) {
          HTML(
            "<h3 style='text-align: center; font-size: 16px; color: #50A625;'>*Please select a data row in the table to prediction*</h3>"
          )
        }
      })
      
    }
  })
  
  
  observeEvent(input$buttonPreMultiple, {
    if (!fileUploadedMultiple()) {
      showNotification("Please upload TSV file before running predictions.", type = "warning")
    } else {
      test <- input_file()
      testDT <- test[, c("sample_ID", "age", DMPs_model)]
      testDT[, DMPs_model] <- round(testDT[, DMPs_model], 2)
      
      test_button01 <- testDT
      test_button01$COMER_score <-
        round(predict(formula_standard, test_button01), 2)
      
      test_button01$Prediction <-
        ifelse(test_button01$COMER_score > cutoff_standard,
               "Severe",
               "Non-severe")
      ##### Personal Prediction
      output$userdata_table = renderDataTable({
        testDT[, DMPs_model[1]] <-
          format(testDT[, DMPs_model[1]], nsmall = 2)
        testDT[, DMPs_model[2]] <-
          format(testDT[, DMPs_model[2]], nsmall = 2)
        testDT[, DMPs_model[3]] <-
          format(testDT[, DMPs_model[3]], nsmall = 2)
        
        datatable(
          testDT,
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;', 'Your samples information') ,
          options = list(
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            )),
            scrollX = TRUE,
            scrollY = "250px",
            lengthMenu = list(c(5, 10, 30, 50, -1),
                              c('5', '10', '30', '50', 'All'))
          )
        )
      })
      
      output$graph <- renderPlotly({
        test_button01 %>%
          plot_ly() %>%
          add_trace(
            x = ~ Prediction,
            y = ~ round(COMER_score, 2),
            color = ~ Prediction,
            type = "box",
            boxpoints = "all",
            colors = c("#CC79A7", "#FC4E07")
          ) %>%
          layout(
            legend = list(
              orientation = "h",
              x = 0,
              xanchor = "center",
              y = 1,
              yanchor = "bottom"
            ),
            xaxis = list(
              title = list(
                text = '',
                font = list(size = 20),
                standoff = 25
              ),
              showgrid = TRUE
            ),
            yaxis = list(title = list(
              text = 'COMER SCORE',
              font = list(size = 15),
              standoff = 25
            ))
          )
      })
      
      output$plot2 <- renderDataTable({
        dataplot2 <- test_button01[,-(2:(ncol(test_button01) - 2))]
        datatable(
          dataplot2,
          extensions = 'Buttons',
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;', 'Scores in details and prediction') ,
          options = list(
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            )),
            scrollX = TRUE,
            scrollY = "250px",
            dom = 'Bltp',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            lengthMenu = list(c(5, 10, 30, 50, -1),
                              c('5', '10', '30', '50', 'All'))
          )
        )
      })
      
      
      dataPreAge <- test[, c("sample_ID", "age", DMPs_model)]
      dataPreAge[, DMPs_model] <- round(dataPreAge[, DMPs_model], 2)
      dataPreAge$COMER_score <-
        round(predict(formula_extend, dataPreAge), 2)
      
      dataPreAge$PredictionAge <-
        ifelse(dataPreAge$COMER_score > cutoff_extend,
               "Severe",
               "Non-severe")
      
      
      output$graph2 <- renderPlotly({
        dataPreAge %>%
          plot_ly() %>%
          add_trace(
            x = ~ PredictionAge,
            y = ~ COMER_score,
            color = ~ PredictionAge,
            type = "box",
            boxpoints = "all",
            colors = c("#CC79A7", "#FC4E07")
          ) %>%
          layout(
            legend = list(
              orientation = "h",
              x = 0,
              xanchor = "center",
              y = 1,
              yanchor = "bottom"
            ),
            xaxis = list(
              title = list(
                text = '',
                font = list(size = 20),
                standoff = 25
              ),
              showgrid = TRUE
            ),
            yaxis = list(
              title = list(
                text = 'Age adjusted COMER score',
                font = list(size = 15),
                standoff = 25
              )
            )
          )
      })
      
      output$plot3 <-
        renderDataTable({
          dataPreAgeCut <- dataPreAge[,-(2:(ncol(dataPreAge) - 2))]
          dataPreAgeCut$COMER_score <-
            round(dataPreAgeCut$COMER_score, 2)
          datatable(
            dataPreAgeCut,
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: center; color:black;  font-size:150% ;',
              'Scores in details and prediction (Age adjustment)'
            ) ,
            extensions = 'Buttons',
            options = list(
              columnDefs = list(list(
                className = "dt-left", targets = "_all"
              )),
              scrollX = TRUE,
              scrollY = "250px",
              dom = 'Bltp',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              lengthMenu = list(c(5, 10, 30, 50, -1),
                                c('5', '10', '30', '50', 'All'))
            )
          )
        })
      
      output$without_age_text <- renderUI({
        HTML(
          "<h3 style='text-align: center; font-size: 30px;'>Prediction without age</h3>"
        )
      })
      output$with_age_text <- renderUI({
        HTML("<h3 style='text-align: center; font-size: 30px;'>Prediction with age</h3>")
      })
    }
  })
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
