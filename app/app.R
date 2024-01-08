### Shiny App to Assist Survey Cleaning

library(shiny)
library(reactlog)
# While Shiny app is running, press Cmd+F3 to launch the reactlog application.
# https://rstudio.github.io/reactlog/articles/reactlog.html

# library(gridlayout)
library(bslib)
# library(plotly)
# library(DT)

library(tidyverse)

# tell shiny to log all reactivity
reactlog_enable()
# options(shiny.reactlog = TRUE)


# Data --------------------------------------------------------------------

raw <- readRDS("/Users/lennart/Library/CloudStorage/OneDrive-UTCloud/BA_Klein/gesis.rds")


# UI ----------------------------------------------------------------------

ui <- bslib::page_sidebar(
  title = "Survey Analysis Helper",
  ### Sidebar -----------------------------------------------------------------
  sidebar = bslib::sidebar(
    width = 300,
    selectizeInput(
      selected = "",
      # selected = "Vote", # DEBUG
      inputId = "selected_variable",
      label = "Select Variable",
      choices = c("", names(raw))
    ),
    uiOutput("varNameInput"),
    checkboxInput(
      inputId = "show_data",
      value = FALSE,
      label = "Show Data Outcomes"
    ),
    checkboxInput(
      inputId = "rev",
      value = FALSE,
      label = "Reverse Match Values"
    ),
    checkboxInput(
      inputId = "translate",
      value = FALSE,
      label = "Translate Factor Levels"
    ),
    textAreaInput(inputId = "var_label", label = "Variable Label", value = ""),
    uiOutput("selectNA"),
    tags$hr(),
    # textOutput(outputId = "deepl_quota")
  ),
  ### Main Panel ------------------------------------------------------------
  # tags$h3("Dataset"),
  # # tags$h4("Class"),
  # verbatimTextOutput(outputId = "datasetClass"),
  # tags$hr(),
  tags$h2("Variable Overview"),
  bslib::layout_columns(
    col_widths = c(6, 6, 6, 6), # 12 max
    fill = FALSE,
    card(
      fill = FALSE,
      tags$h4("Count"),
      tableOutput(outputId = "countTable")
    ),
    card(
      fill = FALSE,
      tags$h4("Structure"),
      verbatimTextOutput(outputId = "variable_structure"),
      verbatimTextOutput(outputId = "variable_class"),
    ),
    card(
      fill = FALSE,
      tags$h4("Levels"),
      verbatimTextOutput(outputId = "variable_levels"),
      verbatimTextOutput(outputId = "variable_levels_no")
    ),
    card(
      fill = FALSE,
      tags$h4("Missingness"),
      verbatimTextOutput(outputId = "variable_missing")
    )
  ),

  # tags$h4("Var Label"),
  # verbatimTextOutput(outputId = "variable_label")

  tags$hr(), # horizontal rule
  tags$h3("Coding Helpers"),
  card(
    fill = FALSE,
    tags$h5("Recoder"),
    codeModules::codeOutput(outputId = "helper_recoder"),
    tags$h5("Rematcher"),
    codeModules::codeOutput(outputId = "helper_rematcher"),
    tags$h5("Explicit NA"),
    codeModules::codeOutput(outputId = "helper_NA"),
    tags$h5("Relabel"),
    codeModules::codeOutput(outputId = "helper_relabel")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  output$deepl_quota <- renderText({
    usage <- deeplr::usage2(auth_key = Sys.getenv("DEEPL_API_KEY"))
    paste0("DeepL Quota:\n", usage$character_count, " / ", format(usage$character_limit, big.mark = ",", scientific = FALSE))
  })

  output$variable_structure <- renderPrint({
    raw[[input$selected_variable]] |> str()
  })

  output$variable_class <- renderPrint({
    raw[[input$selected_variable]] |> class()
  })

  output$variable_levels <- renderText({
    raw[[input$selected_variable]] |>
      levels() |>
      paste(collapse = "\n")
  })

  output$variable_levels_no <- renderText({
    n_levels <- raw[[input$selected_variable]] |>
      levels() |>
      length()
    str_glue("-> {n_levels} Levels")
  })

  output$variable_missing <- renderText({
    na_n <- raw[[input$selected_variable]] |>
      is.na() |>
      sum()
    na_percent <- na_n / nrow(raw)
    na_percent <- round(na_percent * 100, 2)

    str_glue("NA = {na_n} ({na_percent}%)")
  })

  output$variable_label <- renderPrint({
    raw[[input$selected_variable]] |> labelled::print_labels()
  })


  ## DEBUG
  # input <- list()
  # input$selected_variable <- "Arbe_Dig"

  output$helper_recoder <- codeModules::renderCode({
    vec <- raw[[input$selected_variable]]

    if (is.factor(vec)) {
      lev <- levels(vec)
    } else {
      lev <- unique(vec)
    }

    same <- TRUE # DEBUG
    if (same) {
      glue <- stringr::str_glue("\"{lev}\" = \"{lev}\"")
    } else {
      glue <- stringr::str_glue("\"\" = \"{lev}\"")
    }

    if (input$translate) {
      # Functions that call the free API end with a "2"
      lev_en <- deeplr::translate2(
        text = lev,
        source_lang = "DE",
        target_lang = "EN",
        auth_key = Sys.getenv("DEEPL_API_KEY")
      )
      glue <- stringr::str_glue("\"{lev_en}\" = \"{lev}\"")
    }

    recoder <- paste0(
      input$var_name, " = ", input$selected_variable, " |> fct_recode(\n  ",
      paste0(glue, collapse = ",\n  "), "\n)"
    )
    recoder
  })
  output$helper_rematcher <- codeModules::renderCode({
    vec <- raw[[input$selected_variable]]

    if (is.factor(vec)) {
      lev <- levels(vec)
    } else {
      lev <- unique(vec)
    }
    numbers <- TRUE
    if (numbers) {
      if (input$rev) {
        glue <- stringr::str_glue("\"{lev}\" ~ {rev(seq_along(lev))}")
      } else {
        glue <- stringr::str_glue("\"{lev}\" ~ {seq_along(lev)}")
      }
    } else {
      glue <- stringr::str_glue("\"{lev}\" ~ ")
    }
    # var_name <- input$var_name |>
    recoder <- paste0(
      input$var_name, " = ", input$selected_variable, " |> case_match(\n  ", paste0(glue, collapse = ",\n  "),
      "\n)"
    )

    recoder
  })

  output$helper_NA <- codeModules::renderCode({
    str_glue("|> fct_na_level_to_value(extra_levels = {utils::capture.output(dput(input$NA_labels))})")
  })


  output$helper_relabel <- codeModules::renderCode({
    if (input$var_label == "") {
      "No label added."
    } else {
      # input$var_label <- "Wenn am nächsten Sonntag Bundestagswahl wäre, welche Partei würden Sie dann wählen?"

      paste0("labelled::var_label(dat$", input$selected_variable, ') <- "', input$var_label, '"')
    }
  })


  # ----------------------------------------------------------------------

  var_name <- reactive({
    if (is.factor(raw[[input$selected_variable]])) {
      input$selected_variable |>
        janitor::make_clean_names() |>
        paste0("_f")
    } else if (is.numeric(raw[[input$selected_variable]])) {
      input$selected_variable |>
        janitor::make_clean_names() |>
        paste0("_num")
    } else {
      input$selected_variable |> janitor::make_clean_names()
    }
  })

  var_labels <- reactive({
    if (is.factor(raw[[input$selected_variable]])) {
      raw[[input$selected_variable]] |> levels()
    } else {
      "No Labels"
    }
  })

  output$varNameInput <- renderUI({
    textInput(
      inputId = "var_name",
      label = "New Variable Name",
      value = var_name()
    )
  })

  output$selectNA <- renderUI({
    selectizeInput(
      inputId = "NA_labels",
      label = "NA Labels",
      multiple = TRUE,
      choices = var_labels()
    )
  })


  # Dataset -----------------------------------------------------------------

  output$htmlDataset <- renderPrint({
    # tags$strong("Class:")
    # tags$code(raw |> class())
  })

  output$datasetClass <- renderPrint({
    raw |> class()
  })


  output$variableName <- renderText({
    HTML(paste("<b>Variable:</b>", input$selected_variable))
  })

  tableData <- reactive({
    if (input$selected_variable == "") {
      # Default dataset or a placeholder
      NULL
    } else {
      raw # data
    }
  })


  output$countTable <- renderTable({
    if (!is.null(tableData())) {
      count <- tableData() |> count(.data[[input$selected_variable]])

      # hide data column
      if (input$show_data) {
        return(count)
      } else {
        return(count |> select(-n))
      }
    }
  })
}


# App ---------------------------------------------------------------------

shinyApp(ui, server)

# runApp(appDir = "app/app.R")

# once app has closed, display reactlog from shiny
# shiny::reactlogShow()
