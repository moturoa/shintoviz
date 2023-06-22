
library(shiny)
library(softui)


table_opleverjaar_prijsklasse_ehv = function(data){

  # TODO dit is een aparte prijsklasse kolom die ook aangemaakt moet worden in read_woningproductie (bv.)
  levs <- c("Sociale huur overig", "Sociale huur corporaties (zelfstandig)", "Huur(middenhuur)",  "Huur(duur)",
            "Koop(sociaal)", "Koop(middelduur)", "Koop(duur)")

  data <- filter(data, !is.na(prijsklasse), !prijsklasse %in% c("Onbekend","")) %>%
    mutate(prijsklasse = factor(prijsklasse3, levels = levs),
           prijsklasse = recode(prijsklasse, "Sociale huur corporaties (zelfstandig)" = "Sociale huur corp.\n(zelfstandig)")
    )

  data %>%
    select(opleverjaar, aantalwoningen, prijsklasse) %>%
    group_by(opleverjaar, prijsklasse) %>%
    summarize(n_woningen = sum(aantalwoningen), .groups = "drop") %>%
    group_by(prijsklasse) %>%
    arrange(opleverjaar) %>%
    mutate(n_woningen_cumu = cumsum(n_woningen)) %>%
    ungroup %>%
    filter(!is.na(prijsklasse), prijsklasse != "")

}



devtools::load_all()

pdata <- readRDS("c:/repos/wbm3.0/tmp.rds")

cfg <- yaml::read_yaml("test/testconfig.yml")$config

ui <- softui::simple_page(

  softui::box(width = 4,

    tags$div(id = "placeholder"),

    tags$hr(),
    numericInput("num1", "Base size", value = 14)

  )

)

server <- function(input, output, session) {

  shintoviz::insert_plot_widgets(reactive(pdata), cfg, id = "placeholder",
                                 ui_container = "tabset_panel",
                                 global_settings = reactive(list(base_size = input$num1)))


}

shinyApp(ui, server)
