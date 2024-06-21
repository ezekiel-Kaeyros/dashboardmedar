box::use(
  shiny[div, NS, tags, tagList, img, h1, a,downloadButton],
  shiny.fluent[fluentPage, ThemeProvider]
)

box::use(
  app/view/components/sidebar,
  app/view/components/header
)


theme <- list(
  palette = list(
    themePrimary = "#5547AC",
    themeLighterAlt = "#f6f6fc",
    themeLighter = "#dedbf2",
    themeLight = "#c3bde6",
    themeTertiary = "#8e84cd",
    themeSecondary = "#6457b5",
    themeDarkAlt = "#4b3f9a",
    themeDark = "#3f3582",
    themeDarker = "#2f2760",
    neutralLighterAlt = "#faf9f8",
    neutralLighter = "#f3f2f1",
    neutralLight = "	#edebe9",
    neutralQuaternaryAlt = "#2c2b2a",
    neutralQuaternary = "#e1dfdd",
    neutralTertiaryAlt = "#c8c6c4",
    neutralTertiary = "#a19f9d",
    neutralSecondary = "#605e5c",
    neutralPrimaryAlt = "#3b3a39",
    neutralPrimary = "#323130",
    neutralDark = "#201f1e",
    black = "#000000",
    white = "#ffffff"
  )
)

#' @export
main_layout <- function(main_ui) {
  ThemeProvider(
    theme = theme,
    div(class = "container",
    #div(class = "sidebar", sidebar$sidebar_ui),
      div(class = "content",
          div(class = "header", header$header_ui),
          div(class = "main", main_ui)),
      div(class = "footer")
    )
  )
}


#' @export
home_layout <- function(home_data_overview,home_recent) {
  fluentPage(
    div(class = "home",
        tagList(
          div(class = "home__header",
              h1("Good Morning,"),
              h1(class = "title--colored", "Name"),
          ),
          div(class = "home__overview",
              h1(class = "overview__title", "Overview"),
              home_data_overview,
          ),
          div(class = "recent_page",
              home_recent
          )
        )
    )
  )
}


# Qualitative data page
# tl: top left card; tr_card: top right card; bl: bottom left card; br: bottom right card

#' @export
quantitative_page_layout <- function(head,tl_card, bl_card, middle_card,mid_card, tr_card, br_card, mid_b_card) {#mid_card
  div(class = "quantitative_page",
      head,
      # div(class = "head_section",
      #   h1(class = "quantitative_page__title", ""),
      #   div( style = "float: right;  gap: 0.5rem;",
      #     # shiny.fluent::Link(href="#!/quantitative_bivariate", "Go to Bivariate",
      #     #                    style = "background-color: #fff; text-decoration:none; padding: 1em 1.5em;
      #     #                   text-align: center; border-color: #000; border-radius: 12px;
      #     #                   border: 1px solid black;
      #     #                  color: #000; font-weight: bold;"),
      #     shiny.fluent::DefaultButton.shinyInput("export_quantitative", "Export data",
      #                                            iconProps = list(iconName = "Download"))
      #
      #   ),
      # ),
    div(class = "quantitative_page__content",
      div(class = "left",
        tl_card,
        bl_card
      ),
      div(class = "middle",
        middle_card,
        tags$br(),
        tags$br(),
        mid_card,
      ),
      div(class = "right",
        tr_card,
        br_card
      )
    ),
    div( class = "middle_bottom",
         mid_b_card
    )
  )
}

#' @export
quantitative_bivariate_layout <- function(tl_card, bl_card,tr_card, br_card, token) {#mid_card
  div(class = "quantitative_bivariate",
      div(class = "head_section",
          h1(class = "quantitative_page__title", ""),
          div( style = "float: right; display: flex; gap: 0.5rem;",
               shiny.fluent::Link(href = paste("#!/quantitative?token=", token, sep = ""),
                                  "Quantitativ",
                                  style = "background-color: #fff; text-decoration:none; padding: 1.5em 1.5em;
                                  border-color: #000; border-radius: 12px; border: 1px solid black;
                           color: #000; font-weight: bold; display: flex;"),
               shiny.fluent::DefaultButton.shinyInput("export_bivariate", "Daten exportieren",
                                                      iconProps = list(iconName = "Download"))
          ),
      ),
      div(class = "quantitative_bivariate__content",
          div(class = "quantitative_bivariate_left",
              tl_card,
              bl_card
          ),
          div(class = "quantitative_bivariate_right",
              tr_card,
              br_card
          ),
      )

  )
}

#' @export
qualitative_layout <- function(card, token) {#mid_card
  div(class = "qualitative_page",
      div(
        style = "float: right; display: flex; gap: 0.5rem;",
        shiny.fluent::Link(href=paste("#!/wordcloud?token=", token, sep = ""),
                           "Siehe Wordcloud",
                           style = "background-color: #fff; text-decoration:none; padding: 1em 1.5em;
                            text-align: center; border-color: #000; border-radius: 12px;
                            border: 1px solid black;
                           color: #000; font-weight: bold;"),
        shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                               iconProps = list(iconName = "Refresh"),
                                               style = "background-color: #fff; text-decoration:none; padding: 1em 1.5em;
                                                border-color: #000; border-radius: 12px; height:50px; top: -15px;
                                                border: 1px solid black; color: #000; font-weight: bold;"
        )
      ),
      tags$br(),
      tags$br(),
      h1(class = "qualitative_page__title",  ""),
      div(class = "qualitative_page__content",
          div(class = "qualitative_page_middle",
              card
          )
      )

  )
}

#' @export
wordcloud_layout <- function(card, token) {#mid_card
  div(class = "qualitative_page",
      div(
        style = "float: right; margin-top:15px; margin-right: 10px;",
        shiny.fluent::Link(href=paste("#!/qualitative?token=", token, sep = ""),
                           "Siehe Qualitativ",
                           style = "background-color: #2B8049; text-decoration:none; padding: 1em 1.5em;
                           justify-content: center; align-items: center; width: 250px; height: 56px;
                            text-align: center; border-color: #000; border-radius: 12px;
                           color: #fff; font-weight: bold;")
      ),
      tags$br(),
      h1(class = "qualitative_page__title",  ""),
      div(class = "qualitative_page__content",
          div(class = "qualitative_page_middle",
              card
          )
      )

  )
}
