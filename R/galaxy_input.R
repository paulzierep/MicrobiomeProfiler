# Galaxy identifier-list input helpers.

galaxy_identifier_input <- function(path = Sys.getenv("MICROBIOMEPROFILER_INPUT", unset = "")) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  values <- trimws(unlist(strsplit(readLines(path, warn = FALSE), "[\\t,;]")))
  values <- values[nzchar(values) & !grepl("^#", values)]
  if (!length(values)) return(NULL)
  paste(values, collapse = "\n")
}

galaxy_input_type <- function() {
  Sys.getenv("MICROBIOMEPROFILER_INPUT_TYPE", unset = "")
}

galaxy_input_subtype <- function() {
  Sys.getenv("MICROBIOMEPROFILER_INPUT_SUBTYPE", unset = "")
}
