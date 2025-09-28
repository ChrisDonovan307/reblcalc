box::use(
  shiny
)

# Module Template
template_ui <- function(id) {
  ns <- shiny$NS(id)
}

template_server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
