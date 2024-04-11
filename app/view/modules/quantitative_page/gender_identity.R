
box::use(
  shiny[moduleServer, div,NS, h1,h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/gender_logic,
  app/logic/functions,
  app/logic/quantitative/organization_type_logic
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title1")), #"Geschlechtsidentität",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle",shiny::textOutput(ns("title2")) ,#"Verschiedene Geschlechter :",
                    p(class = "description", shiny::textOutput(ns("subtitle")))), #length(unique(import_data$data$gender))-2
                    # Graph goes here
                    uiOutput(ns("plot_persongen"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_gen <- gender_logic$data_gen(filter)
    data_type <- organization_type_logic$data_type(filter)
    marker <- list(color="#F8D1A4")
    color <- c("#F8D1A4", "#F8D1A4","#F8D1A4", "#F8D1A4",
               "#F8D1A4",  "#F8D1A4", "#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4",
               "#F8D1A4")
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
    if (filter=="Eine Organisation/Institution")  {
      output$plot_persongen <- renderUI({
        if (toggle$barplot) {
          plotlyOutput(ns("barplot1"))
        } else {
          plotlyOutput(ns("piechart1"))
        }
      })

      output$barplot1 <- renderPlotly({
        functions$generate_barplot(data_type,"Type", marker)
      })

      output$piechart1 <- renderPlotly({
        functions$generate_piechart(data_type,"Type", color)
      })
    } else {
      output$plot_persongen <- renderUI({
        if (toggle$barplot) {
          plotlyOutput(ns("barplot"))
        } else {
          plotlyOutput(ns("piechart"))
        }
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_gen,"Gender",marker)
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_gen,"Gender",color)
      })
    }

    if (filter=="Eine Organisation/Institution") {
      output$title1 <- shiny::renderText({
        paste("Typ der Organisation")
      })
    } else {
      output$title1 <- shiny::renderText({
        paste("Geschlechtsidentität")
      })
    }

    if (filter=="Eine Organisation/Institution") {
      output$title2 <- shiny::renderText({
        paste("Verschiedene Arten von Organisationen :")
      })
    } else {
      output$title2 <- shiny::renderText({
        paste("Verschiedene Geschlechter :")
      })
    }

    if (filter!="Eine Organisation/Institution"){
      output$subtitle <- shiny::renderText({
        dim(data_gen)[1]
        #nrow(data_gender)
      })
    } else {
      output$subtitle <- shiny::renderText({
        length(unique(unlist(import_data$data$organizationType)))
      })
    }
    # output$plot_persongen <- renderUI({
    #   if (toggle$barplot) {
    #     plotlyOutput(ns("barplot"))
    #   } else {
    #     plotlyOutput(ns("piechart"))
    #   }
    # })

    # output$barplot <- renderPlotly({
    #   functions$generate_barplot(data_gen,"Gender",marker)
    # })
    #
    # output$piechart <- renderPlotly({
    #   functions$generate_piechart(data_gen,"Gender",color)
    # })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
