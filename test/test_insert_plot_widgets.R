
library(shiny)
library(softui)

library(tidyr)
#
# table_opleverjaar_prijsklasse_ehv = function(data, max_opleverjaar = NULL){
#
#   # TODO dit is een aparte prijsklasse kolom die ook aangemaakt moet worden in read_woningproductie (bv.)
#   levs <- c("Sociale huur overig", "Sociale huur corporaties (zelfstandig)", "Huur(middenhuur)",  "Huur(duur)",
#             "Koop(sociaal)", "Koop(middelduur)", "Koop(duur)")
#
#   data <- filter(data, !is.na(prijsklasse), !prijsklasse %in% c("Onbekend","")) %>%
#     mutate(prijsklasse = factor(prijsklasse3, levels = levs),
#            prijsklasse = recode(prijsklasse, "Sociale huur corporaties (zelfstandig)" = "Sociale huur corp.\n(zelfstandig)")
#     )
#
#   if(!is.null(max_opleverjaar)){
#
#     data <- filter(data, opleverjaar <= !!max_opleverjaar)
#   }
#
#   data %>%
#     select(opleverjaar, aantalwoningen, prijsklasse) %>%
#     group_by(opleverjaar, prijsklasse) %>%
#     summarize(n_woningen = sum(aantalwoningen), .groups = "drop") %>%
#     group_by(prijsklasse) %>%
#     arrange(opleverjaar) %>%
#     mutate(n_woningen_cumu = cumsum(n_woningen)) %>%
#     ungroup %>%
#     filter(!is.na(prijsklasse), prijsklasse != "")
#
# }
#
#
# table_aantalwoningen_prijsklasse = function(data){
#
#     data <- data %>%
#       mutate(prijsklasse3 = gsub("betaalbaar|goedkoop", "sociaal", prijsklasse),
#              prijsklasse3 = case_when(
#                prijsklasse3 == "Huur(sociaal)" & !onzelfstandig & grepl("Corporatie", kenmerken) ~ "Sociale huur corporaties (zelfstandig)",
#                prijsklasse3 == "Huur(sociaal)" ~ "Sociale huur overig",
#                TRUE ~ as.character(prijsklasse3)
#              ))
#
#     levs <- c("Sociale huur overig", "Sociale huur corporaties (zelfstandig)", "Huur(middenhuur)",  "Huur(duur)",
#               "Koop(sociaal)", "Koop(middelduur)", "Koop(duur)")
#
#     data <- filter(data, !prijsklasse %in% c("Onbekend","")) %>%
#       mutate(prijsklasse3 = factor(prijsklasse3, levels = levs),
#              prijsklasse3 = recode(prijsklasse3, "Sociale huur corporaties (zelfstandig)" = "Sociale huur corp.\n(zelfstandig)"))
#
#
#   data %>%
#     filter(!is.na(prijsklasse3)) %>%
#     group_by(prijsklasse3) %>%
#     summarize(aantalwoningen = sum(aantalwoningen, na.rm = TRUE),
#               .groups = "drop")
# }
#
# aantalwoningen_sociale_huur <- function(data){
#
#   data <- mutate(data,
#                  sociale_huur = prijsklasse %in% c("Huur(betaalbaar)","Huur(goedkoop)"),
#                  is_corporatie = grepl("Corporatie",kenmerken),
#                  zelfstandig = !onzelfstandig,
#                  soc_class = case_when(
#                    sociale_huur & is_corporatie & zelfstandig ~ "Corporaties zelfstandig",
#                    sociale_huur & is_corporatie & onzelfstandig ~ "Corporaties onzelfstandig",
#                    sociale_huur & !is_corporatie & zelfstandig ~ "Marktpartijen zelfstandig",
#                    sociale_huur & !is_corporatie & onzelfstandig ~ "Marktpartijen onzelfstandig",
#                    TRUE ~ "Anders"
#                  ),
#                  soc_class = factor(soc_class, levels = c("Corporaties zelfstandig","Corporaties onzelfstandig",
#                                                           "Marktpartijen zelfstandig","Marktpartijen onzelfstandig",
#                                                           "Anders"))
#   ) %>%
#     filter(soc_class != "Anders")
#
#   data <- data %>%
#     #filter(opleverjaar %in% !!.db$next_5jr) %>%
#     group_by(soc_class, opleverjaar) %>%
#     summarize(aantalwoningen = sum(aantalwoningen), .groups = "drop")
#
#   data
# }
#
# format_aantalwoningen_sociale_huur <- function(data){
#
#   setNames(data, c("Klasse","Opleverjaar","Aantal woningen"))
#
# }

table_capval_prijsklasse_opleverjaar <- function(data){

  data %>%
    select(opleverjaar, socialehuur, middeldurehuur, vrijsectorhuur, onbekendhuur,
           aantalwoningen, socialekoop, middeldurekoop, hoogsegmentkoop, onbekendkoop) %>%
    mutate(
      socialehuur = replace_na(socialehuur,0),
      middeldurehuur = replace_na(middeldurehuur,0),
      vrijsectorhuur = replace_na(vrijsectorhuur, 0),
      socialekoop = replace_na(socialekoop, 0),
      middeldurekoop = replace_na(middeldurekoop, 0),
      hoogsegmentkoop = replace_na(hoogsegmentkoop, 0),
      aantalonbekend = aantalwoningen - socialehuur - middeldurehuur -vrijsectorhuur - socialekoop - middeldurekoop - hoogsegmentkoop) %>%

    select(-aantalwoningen, -onbekendkoop, -onbekendhuur) %>%
    pivot_longer(cols = c(socialehuur,middeldurehuur,vrijsectorhuur, socialekoop,middeldurekoop,hoogsegmentkoop,aantalonbekend),
                 names_to = "prijsklasse",
                 values_to = "aantalwoningen") %>%
    mutate(prijsklasse = recode(prijsklasse,
                                socialehuur = "Sociale huur",
                                middeldurehuur = "Middeldure huur",
                                vrijsectorhuur = "Vrije sector huur",
                                socialekoop = "Sociale koop",
                                middeldurekoop = "Middeldure koop",
                                hoogsegmentkoop = "Hoogsegment koop",
                                aantalonbekend = "Onbekend"
    )) %>%
    filter(!is.na(aantalwoningen)) %>%
    mutate(prijsklasse = factor(prijsklasse, levels = c("Sociale huur","Middeldure huur","Vrije sector huur",
                                                        "Sociale koop","Middeldure koop","Hoogsegment koop",
                                                        "Onbekend")))


}

devtools::load_all()

pdata <- readRDS("c:/repos/wbm3.0/wp.rds") %>%
  filter(opleverjaar > 2023)

#cfg <- yaml::read_yaml("test/testconfig.yml")$config
cfg <- yaml::read_yaml("test/testconfig2.yml")

ui <- softui::simple_page(

  softui::box(width = 4,

    numericInput("num1", "Base size", value = 14)

  ),
  tags$div(id = "placeholder", style = "width: 600px;")


)

server <- function(input, output, session) {

  shintoviz::insert_plot_widgets(reactive(pdata), cfg, id = "placeholder",
                                 width = 4,
                                 plotOutput_only  =TRUE,
                                 global_settings = reactive(list(base_size = input$num1)))


}

shinyApp(ui, server)



