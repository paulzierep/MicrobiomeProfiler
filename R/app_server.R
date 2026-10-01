#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    identifiers <- galaxy_identifier_input()
    input_type <- galaxy_input_type()
    subtype <- galaxy_input_subtype()
    mod_GENEenrichment_server(
        "GENEenrichment_ui_1",
        initial_ids = if (identical(input_type, "gene")) identifiers else NULL,
        initial_type = subtype
    )
    mod_MDenrichment_server(
        "MDenrichment_ui_1",
        initial_ids = if (identical(input_type, "taxid")) identifiers else NULL
    )
    mod_Metaboenrichment_server(
        "Metaboenrichment_ui_1",
        initial_ids = if (identical(input_type, "metabolite")) identifiers else NULL,
        initial_type = subtype
    )

}
