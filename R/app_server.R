#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    # Galaxy integration. galaxy_ie.R is inert outside a Galaxy interactive tool:
    # it renders no buttons and no picker unless HISTORY_ID and API_KEY are set.
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

    # Import a dataset or collection element from the current history.
    #
    # galaxy_ie_import() repoints MICROBIOMEPROFILER_INPUT at the downloaded copy.
    # The identifier list is read once at start-up above, so the imported
    # identifiers are pushed into the text area of the module the tool was
    # configured for; otherwise the import would be invisible until a restart.
    imported <- galaxy_ie_picker_server(input, output, session)
    observeEvent(imported(), {
        ids <- galaxy_identifier_input()
        if (is.null(ids)) return()
        target <- switch(
            input_type,
            gene = "GENEenrichment_ui_1-genelist",
            taxid = "MDenrichment_ui_1-genelist",
            metabolite = "Metaboenrichment_ui_1-genelist",
            NULL
        )
        if (is.null(target)) return()
        session$sendInputMessage(target, list(value = ids, datapath = NA))
    }, ignoreInit = TRUE)
}