### Shiny App to Assist Survey Cleaning ###

library(shiny)
# library(reactlog)
# While Shiny app is running, press Cmd+F3 to launch the reactlog application.
# https://rstudio.github.io/reactlog/articles/reactlog.html

library(bslib)

# library(waiter)
# library(shinyjs)

library(dplyr)
library(stringr)

# tell shiny to log all reactivity
# reactlog_enable()
# options(shiny.reactlog = TRUE)


# Data --------------------------------------------------------------------

# raw <- readRDS("/Users/lennart/Library/CloudStorage/OneDrive-UTCloud/BA_Klein/gesis.rds")

# UI ----------------------------------------------------------------------

ui <- bslib::page_sidebar(
  waiter::useWaiter(),
  waiter::waiterShowOnLoad(),

  # shinyjs::useShinyjs(),
  title = "Survey Analysis Helper",
  ### Sidebar -----------------------------------------------------------------
  sidebar = bslib::sidebar(
    width = 300,
    # selectizeInput(
    #   inputId = "data",
    #   label = "Select Your Dataset",
    #   choices = c("", keep(ls(), ~ is.data.frame(get(.x)))),
    #   selected = ""
    # ),
    fileInput(
      inputId = "data",
      label = "Select Your Dataset",
      accept = c(".rds")
    ),
    conditionalPanel(
      condition = "input.data != ''",
      ### Hide until data choosen
      selectizeInput(
        inputId = "selected_var",
        label = "Select Variable",
        choices = "",
        selected = ""
      ),
      textInput(
        inputId = "var_name",
        label = "New Variable Name",
        value = ""
      ),
      checkboxInput(
        inputId = "show_data",
        label = "Show Data Outcomes",
        value = FALSE
      ),
      checkboxInput(
        inputId = "rev",
        label = "Reverse Match Values",
        value = FALSE
      ),
      checkboxInput(
        inputId = "translate",
        label = "Translate Factor Levels",
        value = FALSE
      ),
      textAreaInput(
        inputId = "var_label",
        label = "Variable Label",
        value = ""
      ),
      selectizeInput(
        inputId = "NA_labels",
        label = "NA Labels",
        multiple = TRUE,
        choices = ""
      ),
      tags$hr(),
      # textOutput(outputId = "deepl_quota")
    )
  ),
  ### Main Panel ------------------------------------------------------------
  # tags$h3("Dataset"),
  # # tags$h4("Class"),
  # verbatimTextOutput(outputId = "datasetClass"),
  # tags$hr(),
  conditionalPanel(
    condition = "input.data != ''",
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
    # verbatimTextOutput(outputId = "debugText")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  waiter::waiter_hide()

  # Sidebar Server ----------------------------------------------------------

  data <- reactive({
    req(input$data)
    readRDS(input$data$datapath)

    # get(input$data)
  })

  data_col <- reactive({
    req(data())
    req(input$selected_var)

    data()[[input$selected_var]]
  })


  ## Selected Variable
  observeEvent(input$data, {
    updateSelectizeInput(
      inputId = "selected_var",
      choices = c("", names(data()))
    )
  })

  var_name <- eventReactive(input$selected_var, {
    clean_name <- input$selected_var |>
      janitor::make_clean_names()

    if (is.factor(data_col)) {
      str_glue("{clean_name}_f")
    } else if (is.numeric(data()[[input$selected_var]])) {
      str_glue("{clean_name}_num")
    } else {
      clean_name
    }
  })

  observeEvent(input$selected_var, {
    updateTextInput(
      inputId = "var_name",
      value = var_name()
    )
  })

  ####
  var_labels <- reactive({
    if (is.factor(data()[[input$selected_var]])) {
      data()[[input$selected_var]] |> levels()
    } else {
      "No Labels"
    }
  })

  observeEvent(input$selected_var, {
    updateSelectizeInput(
      inputId = "NA_labels",
      choices = var_labels()
    )
  })



  output$deepl_quota <- renderText({
    usage <- deeplr::usage2(auth_key = Sys.getenv("DEEPL_API_KEY"))
    paste0("DeepL Quota:\n", usage$character_count, " / ", format(usage$character_limit, big.mark = ",", scientific = FALSE))
  })

  output$variable_structure <- renderPrint({
    data_col() |> str()
  })

  output$variable_class <- renderPrint({
    data_col() |> class()
  })

  output$variable_levels <- renderText({
    data_col() |>
      levels() |>
      paste(collapse = "\n")
  })

  output$variable_levels_no <- renderText({
    n_levels <- data_col() |>
      levels() |>
      length()
    str_glue("-> {n_levels} Levels")
  })

  output$variable_missing <- renderText({
    na_n <- data_col() |>
      is.na() |>
      sum()
    na_percent <- na_n / nrow(data())
    na_percent <- round(na_percent * 100, 2)

    str_glue("NA = {na_n} ({na_percent}%)")
  })

  # output$variable_label <- renderPrint({
  #   data_col() |> labelled::print_labels()
  # })

  output$countTable <- renderTable({
    if (input$selected_var != "") {
      count <- data() |> count(.data[[input$selected_var]])

      # hide data column
      if (input$show_data) {
        return(count)
      } else {
        return(count |> select(-n))
      }
    }
  })

  # Code Snippets -------------------------------------------------------------

  output$helper_recoder <- codeModules::renderCode({
    vec <- data_col()

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
      input$var_name, " = ", input$selected_var, " |> fct_recode(\n  ",
      paste0(glue, collapse = ",\n  "), "\n)"
    )
    recoder
  })

  output$helper_rematcher <- codeModules::renderCode({
    vec <- data_col()

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
    recoder <- paste0(
      input$var_name, " = ", input$selected_var, " |> case_match(\n  ", paste0(glue, collapse = ",\n  "),
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
      paste0(
        "labelled::var_label(dat$", input$selected_var, ') <- "', input$var_label, '"'
      )
    }
  })

  # Dataset Info -----------------------------------------------------------------

  # output$htmlDataset <- renderPrint({
  #   # tags$strong("Class:")
  #   # tags$code(data() |> class())
  # })

  # output$datasetClass <- renderPrint({
  #   data() |> class()
  # })


  # -------------------------------------------------------------------------
  output$debugText <- renderPrint({
  })
}


# App ---------------------------------------------------------------------

shinyApp(ui, server)


# once app has closed, display reactlog from shiny
# shiny::reactlogShow()
