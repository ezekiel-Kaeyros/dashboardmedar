
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
  app/logic/quantitative/date_occ_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Zeitlicher Abstand zwischen Vorkommnis und Meldung",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle", "Modal-zeitlicher Abstand :"),
                    p(class = "description",shiny::textOutput(ns("text"))),
                    # Graph goes here
                    uiOutput(ns("plot_date"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_months <- date_occ_logic$data_temp(filter)
    data_months1 <- date_occ_logic$data_temp1(filter)
    marker <- list(color = c("#F9AFC5"))
    color <- c("#F9AFC5", "#F9AFC5","#F9AFC5", "#F9AFC5",
               "#F9AFC5",  "#F9AFC5", "#F9AFC5","#F9AFC5","#F9AFC5","#F9AFC5F9AFC581BEF7",
               "#F9AFC5")
    #ns <- NS(id)
    #output$plot_personaf <- render
    button_state <- reactiveVal(FALSE)

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })

    toggle <- reactiveValues(barplot = TRUE)
    output$plot_date <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(data_months,"Time",marker)

    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_months,"Time",color)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })

    output$text <- shiny::renderText({
      #data_months1$temporal_class[which.max(data_months1$total)]
      names(table(data_months1$Var1))[which.max(table(data_months1$Var1))]
    })


  })
}
