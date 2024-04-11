box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/age_logic,
  app/logic/functions,
  app/logic/quantitative/n_employee_logic
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title1")),#"Altersgruppen der betroffenen Personen"
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle",shiny::textOutput(ns("title2")) ),#"Am meisten betroffene Altersgruppe :"
                    p(class = "description", shiny::textOutput(ns("text"))), #names(table(import_data$data$age))[which.max(table(import_data$data$age))]
                    # Graph goes here
                    uiOutput(ns("plot_personage"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_age <- age_logic$data_age(filter)
    data_age1 <- age_logic$data_age1(filter)
    data_emp <- n_employee_logic$data_emp(filter)
    marker <- list(color="#F8D1A4")
    color <- c("#F8D1A4", "#F8D1A4","#F8D1A4", "#F8D1A4",
               "#F8D1A4",  "#F8D1A4", "#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4",
               "#F8D1A4")
    button_state <- reactiveVal(FALSE)

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      }
    })

    toggle <- reactiveValues(piechart = TRUE)
    if (filter=="Eine Organisation/Institution")  {
      output$plot_personage <- renderUI({
        if (toggle$piechart) {
          plotlyOutput(ns("piechart1"))
        } else {
          plotlyOutput(ns("barplot1"))
        }
      })

      output$barplot1 <- renderPlotly({
        functions$generate_barplot(data_emp,"Number of employees", marker)
      })

      output$piechart1 <- renderPlotly({
        functions$generate_piechart(data_emp,"Number of employees", color)
      })
    } else {
      output$plot_personage <- renderUI({
        if (toggle$piechart) {
          plotlyOutput(ns("piechart"))
        } else {
          plotlyOutput(ns("barplot"))
        }
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_age,"Age",marker)
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_age,"Age",color)
      })
    }

    if (filter=="Eine Organisation/Institution") {
      output$title1 <- shiny::renderText({
        paste("Anzahl der Mitarbeiter")
      })
    } else {
      output$title1 <- shiny::renderText({
        paste("Altersgruppen der betroffenen Personen")
      })
    }

    if (filter=="Eine Organisation/Institution") {
      output$title2 <- shiny::renderText({
        paste(" ")
      })
    } else {
      output$title2 <- shiny::renderText({
        paste("Am meisten betroffene Altersgruppe :")
      })
    }

    if (filter=="Eine Organisation/Institution") {
      output$text <- shiny::renderText({
        paste("")
      })
    } else {
      output$text <- shiny::renderText({
        names(table(data_age1$age))[which.max(table(data_age1$age))]
      })
    }

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })


  })
}

