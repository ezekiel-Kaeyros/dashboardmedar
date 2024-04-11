box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText,plotOutput,
        renderPlot],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`], dplyr,graphics[axis, text],RColorBrewer
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/previous_measures_logic,
  app/logic/functions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Bereits ergriffene Maßnahmen",#"",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle","Die meisten bisherigen Maßnahmen :" ), #a refaire
                    p(class = "description", shiny::textOutput(ns("text"))),
                    uiOutput(ns("previous_measures"))#,width="500px", height = 485) #,width="500px"
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_measure <- previous_measures_logic$data_choice_measures(filter)
    data_measure1 <- previous_measures_logic$data_choice_measures1(filter)
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
    output$previous_measures <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(data_measure,"Previous measure",marker)
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_measure,"Previous measure",color)
    })

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })

    output$text <- shiny::renderText({
      names(table(unlist(data_measure1$haveYouReportedYes)))[which.max(table(unlist(data_measure1$haveYouReportedYes)))]
    })


  })
}
