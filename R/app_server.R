#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    # Galaxy integration. galaxy_ie.R is inert outside a Galaxy interactive tool:
    # it renders no buttons unless HISTORY_ID and API_KEY are set.
    galaxy_ie_app("microbiomeprofiler", "MICROBIOMEPROFILER_OUTPUT_DIR", "MICROBIOMEPROFILER_INPUT")

    identifiers <- galaxy_identifier_input()
    input_type <- galaxy_input_type()
    subtype <- galaxy_input_subtype()

    # One registry for the whole session, shared with the modules so that the
    # "Send to Galaxy" button next to each download can regenerate that exact
    # file. Registered under the namespaced output id, because each module has
    # its own downdotPolt / downbarPolt.
    downloads <- galaxy_ie_send_server(input, output, session, label = "MicrobiomeProfiler results")

    mod_GENEenrichment_server(
        "GENEenrichment_ui_1",
        initial_ids = if (identical(input_type, "gene")) identifiers else NULL,
        initial_type = subtype,
        downloads = downloads
    )
    mod_MDenrichment_server(
        "MDenrichment_ui_1",
        initial_ids = if (identical(input_type, "taxid")) identifiers else NULL,
        downloads = downloads
    )
    mod_Metaboenrichment_server(
        "Metaboenrichment_ui_1",
        initial_ids = if (identical(input_type, "metabolite")) identifiers else NULL,
        initial_type = subtype,
        downloads = downloads
    )

}