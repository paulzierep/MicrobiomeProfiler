# Galaxy-aware download helpers.

galaxy_output_dir <- function() {
  Sys.getenv("MICROBIOMEPROFILER_OUTPUT_DIR", unset = "")
}

galaxy_log <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), " ", paste0(..., collapse = ""))
  message(line)
  output_dir <- galaxy_output_dir()
  if (nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat(line, "\n", file = file.path(output_dir, "galaxy_upload.log"), append = TRUE)
  }
}

galaxy_put_async <- function(path) {
  put <- Sys.which("put")
  if (!nzchar(put) || !nzchar(Sys.getenv("HISTORY_ID")) || !nzchar(Sys.getenv("API_KEY"))) {
    galaxy_log("direct upload skipped path=", path, " put_available=", nzchar(put),
               " history_present=", nzchar(Sys.getenv("HISTORY_ID")),
               " api_key_present=", nzchar(Sys.getenv("API_KEY")))
    return(FALSE)
  }

  command <- paste(shQuote(put), "-p", shQuote(path), ">>/dev/null 2>&1")
  system2("/bin/sh", c("-c", shQuote(command)), wait = FALSE)
  galaxy_log("direct upload started path=", path, " put=", put)
  TRUE
}

galaxy_download_handler <- function(filename, content, contentType = NULL) {
  shiny::downloadHandler(
    filename = filename,
    content = function(file) {
      content(file)
      output_dir <- galaxy_output_dir()
      if (!nzchar(output_dir)) return(invisible(NULL))

      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      name <- basename(if (is.function(filename)) filename() else filename)
      output_path <- file.path(output_dir, name)
      file.copy(file, output_path, overwrite = TRUE)
      galaxy_log("saved output name=", name, " path=", output_path,
                 " bytes=", file.info(output_path)$size)
      galaxy_put_async(output_path)
    },
    contentType = contentType
  )
}
