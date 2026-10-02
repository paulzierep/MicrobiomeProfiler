# Shared Galaxy Interactive Tools glue for the microbiome tools.
#
# This file is the single implementation of three things, so a fix lands in one
# place instead of three app forks:
#
#   1. "Send to Galaxy" buttons next to every download button
#   2. an "Import from Galaxy history" picker that lists the datasets and
#      collection elements of the current history
#   3. uploading a file back into the current history, non-blocking
#
# Each app sources this from its own checkout (MetaDAVis from
# scripts/galaxy_downloads.R, the others from a top level galaxy_ie.R) and then
# declares which env var holds its output directory and which input slot an
# imported file should land in. See galaxy_ie_app() below.
#
# Nothing here changes what a download produces: the upload path re-invokes the
# content function that downloadHandler() was given, so the file that is sent is
# byte-for-byte the file a click on the download button would have written.
#
# galaxy_ie_helpers (https://github.com/bgruening/galaxy_ie_helpers) does the
# talking to Galaxy. It knows how to reach the instance from inside a tool
# container and which history the session belongs to. The tool XML exports:
#
#   HISTORY_ID       the history this interactive session belongs to
#   GALAXY_URL       the instance URL ($DOCKER_HOST is substituted in as well)
#   GALAXY_WEB_PORT  fallback port when that URL does not answer
#   API_KEY          a key that may write into that history
#
# Everything degrades to a no-op when those are absent, so a plain `docker run`
# or `shiny::runApp()` behaves exactly as it did before.

# --- the app that is using this file ---------------------------------------
#
# `prefix` is the app name used for log lines and notification text, and for the
# CSS/JS ids, so two of these apps can never collide in one browser page.
# `output_env` is the env var holding the Galaxy job's output directory.
# `import_env` is the env var the app reads its main input from; an imported
# file is written there so the app picks it up through its normal input path.
#
# The history picker is opt-in per app through `picker_ui`/`picker_server`,
# which default to NULL, so an app that does not want it simply gets nothing.
# Declare which env var holds the output directory and which input slot an
# imported file should land in. Assigns the configuration as a side effect, so
# the usual `galaxy_ie_app(...)` at the top of the app's helper file is enough
# and there is nothing to remember to assign.
galaxy_ie_app <- function(prefix, output_env, import_env = "") {
  galaxy_ie$app <- list(
    prefix = prefix,
    output_env = output_env,
    import_env = import_env
  )
  invisible(galaxy_ie$app)
}

# The default configuration. An app that sources this file without calling
# galaxy_ie_app() gets a working "Send to Galaxy" and no picker.
galaxy_ie <- new.env(parent = emptyenv())
galaxy_ie$app <- galaxy_ie_app(prefix = "app", output_env = "GALAXY_OUTPUT_DIR")

# --- the upload command ------------------------------------------------------
#
# galaxy_ie_helpers is installed into its own virtualenv in the image. Use its
# supported console command; the Python package has no __main__ module.
galaxy_ie_put <- function() {
  candidates <- c(
    Sys.getenv("GALAXY_IE_PUT", unset = ""),
    "/opt/galaxy_ie_helpers/bin/put",
    "put"
  )
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    path <- if (grepl("/", candidate, fixed = TRUE)) candidate else Sys.which(candidate)
    if (nzchar(path) && file.access(path, mode = 1L) == 0L) {
      return(path)
    }
  }
  NA_character_
}

# The command that downloads datasets into /import/. Its --id is required, so it
# cannot be used to list a history; that is what galaxy_ie_history_cmd() is for.
galaxy_ie_get <- function() {
  candidates <- c(
    Sys.getenv("GALAXY_IE_GET", unset = ""),
    "/opt/galaxy_ie_helpers/bin/get",
    "get",
    "/opt/galaxy_ie_helpers/bin/gx-get",
    "gx-get"
  )
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    path <- if (grepl("/", candidate, fixed = TRUE)) candidate else Sys.which(candidate)
    if (nzchar(path) && file.access(path, mode = 1L) == 0L) {
      return(path)
    }
  }
  NA_character_
}

# The command that lists the visible history as JSON. This is a separate console
# script from `get`, which cannot do it: get's --id argument is required.
galaxy_ie_history_cmd <- function() {
  candidates <- c(
    Sys.getenv("GALAXY_IE_GET_USER_HISTORY", unset = ""),
    "/opt/galaxy_ie_helpers/bin/get_user_history",
    "get_user_history",
    "/opt/galaxy_ie_helpers/bin/gx-get-user-history",
    "gx-get-user-history"
  )
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    path <- if (grepl("/", candidate, fixed = TRUE)) candidate else Sys.which(candidate)
    if (nzchar(path) && file.access(path, mode = 1L) == 0L) {
      return(path)
    }
  }
  NA_character_
}

galaxy_ie_history <- function() Sys.getenv("HISTORY_ID", unset = "")
galaxy_ie_key <- function() Sys.getenv("API_KEY", unset = "")
galaxy_ie_output_dir <- function() Sys.getenv(galaxy_ie$app$output_env, unset = "")

galaxy_ie_output_path <- function(name) {
  output_dir <- galaxy_ie_output_dir()
  if (!nzchar(output_dir)) {
    return(NA_character_)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(output_dir, basename(name))
}

# Never logs the API key itself, only whether one is present.
galaxy_ie_log <- function(...) {
  line <- paste0(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), " ",
    galaxy_ie$app$prefix, " ", paste0(..., collapse = "")
  )
  message(line)
  output_dir <- galaxy_ie_output_dir()
  if (nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat(line, "\n", file = file.path(output_dir, "galaxy_upload.log"), append = TRUE)
  }
}

# TRUE when this session can upload back to Galaxy. Checked in the UI as well, so
# a plain `docker run` offers no upload buttons that cannot work.
galaxy_ie_ready <- function() {
  nzchar(galaxy_ie_history()) &&
    nzchar(galaxy_ie_key()) &&
    !is.na(galaxy_ie_put())
}

# TRUE when this session can read from Galaxy. Separate from galaxy_ie_ready()
# because the two directions are independent: the history picker only needs
# `get`, and must not disappear just because `put` is missing.
galaxy_ie_can_read <- function() {
  nzchar(galaxy_ie_history()) &&
    nzchar(galaxy_ie_key()) &&
    !is.na(galaxy_ie_get()) &&
    !is.na(galaxy_ie_history_cmd())
}

galaxy_ie_not_ready_message <- function() {
  paste0(
    "This session is not running as a Galaxy interactive tool ",
    "(no history id or API key), so files cannot be sent to or read from Galaxy."
  )
}

# --- uploading ---------------------------------------------------------------

# Write `path` into the current history under `name`. Returns a list with ok and
# message so the caller can tell the user what happened.
galaxy_ie_send <- function(path, name = basename(path), filetype = "auto") {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = paste0("there is no file to send at ", path)))
  }
  put <- galaxy_ie_put()
  if (is.na(put)) {
    return(list(
      ok = FALSE,
      message = "galaxy_ie_helpers is not installed, so nothing can be sent back to Galaxy"
    ))
  }
  if (!nzchar(galaxy_ie_history()) || !nzchar(galaxy_ie_key())) {
    return(list(ok = FALSE, message = galaxy_ie_not_ready_message()))
  }

  galaxy_ie_log(
    "upload start name=", name,
    " path=", normalizePath(path, mustWork = FALSE),
    " bytes=", file.info(path)$size,
    " filetype=", filetype,
    " history=", galaxy_ie_history(),
    " url=", Sys.getenv("GALAXY_URL", unset = "<unset>"),
    " put=", put,
    " api_key_present=", nzchar(galaxy_ie_key())
  )

  output <- tryCatch(
    system2(
      put,
      c(
        "-p", shQuote(path),
        "-t", shQuote(filetype),
        "--history-id", shQuote(galaxy_ie_history())
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 1L, error = conditionMessage(e))
  )

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  error_detail <- attr(output, "error")
  output_text <- paste(as.character(output), collapse = " | ")
  galaxy_ie_log(
    "upload finish name=", name,
    " exit_status=", status,
    if (!is.null(error_detail)) paste0(" error=", error_detail) else "",
    if (nzchar(output_text)) paste0(" output=", output_text) else " output=<empty>"
  )
  if (status != 0L) {
    detail <- paste(utils::tail(as.character(output), 20L), collapse = "\n")
    return(list(
      ok = FALSE,
      message = paste0(
        "the upload failed with exit code ", status,
        if (nzchar(detail)) paste0(":\n", detail)
      )
    ))
  }
  list(ok = TRUE, message = paste0(name, " was added to the Galaxy history"))
}

# The blocking helper above waits for the API call, which would freeze the Shiny
# session on a slow Galaxy. Run it in a background Rscript instead. The helper
# file is re-sourced in the child, so it needs to be locatable from the working
# directory of the app.
# Uploads in a background process, so the browser is not held up by the API call.
#
# The child deliberately does NOT source this file. A Shiny app installed as an R
# package (golem, as MicrobiomeProfiler is) has no source tree on disk at
# runtime, so a child that re-sourced the helper would fail exactly where the
# upload matters most. Everything the child needs is passed in as arguments, and
# the environment (HISTORY_ID, API_KEY, ...) is inherited from the parent.
galaxy_ie_send_async <- function(path, name = basename(path), filetype = "auto") {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!nzchar(name)) {
    return(invisible(FALSE))
  }
  put <- galaxy_ie_put()
  if (is.na(put)) {
    galaxy_ie_log("async upload not possible, put not found; uploading synchronously name=", name)
    return(invisible(galaxy_ie_send(path, name, filetype)$ok))
  }

  # Exactly the same call galaxy_ie_send() makes: `put` derives the dataset name
  # from the file's basename, so the path has to be the saved copy in the output
  # directory (which is named after the download) rather than a temp file.
  expression <- sprintf(
    "invisible(system2(%s, c(\"-p\", %s, \"-t\", %s, \"--history-id\", %s), stdout = FALSE, stderr = FALSE))",
    encodeString(put, quote = "\""),
    encodeString(normalizePath(path, mustWork = FALSE), quote = "\""),
    encodeString(filetype, quote = "\""),
    encodeString(galaxy_ie_history(), quote = "\"")
  )
  system2(
    rscript,
    c("--vanilla", "-e", shQuote(expression)),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE
  )
  galaxy_ie_log("async upload started name=", name, " path=", path)
  invisible(TRUE)
}

# --- the download registry ---------------------------------------------------
#
# A download button cannot be reused for an upload: its content function is only
# ever called while a download is in flight. So every handler is registered on
# the way through, which is what lets the generic "Send to Galaxy" handler below
# produce the file again without a second implementation of any table or plot.
galaxy_ie_registry <- function() {
  new.env(parent = emptyenv())
}

galaxy_ie_register <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  assign(
    id,
    list(filename = filename, content = content, contentType = contentType),
    envir = registry
  )
}

# downloadHandler() itself, plus the registration. Defined here rather than at
# the call sites so the call sites keep reading exactly as they did: the
# arguments and the return value are downloadHandler()'s.
galaxy_ie_download_with <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  galaxy_ie_register(registry, id, filename, content, contentType)
  shiny::downloadHandler(filename = filename, content = content, contentType = contentType)
}

# The file name a download button would use, as a plain string. Galaxy names the
# dataset after it, so it has to survive a path and keep its extension.
galaxy_ie_download_name <- function(handler, id) {
  name <- tryCatch(
    if (is.function(handler$filename)) handler$filename() else as.character(handler$filename),
    error = function(e) NULL
  )
  if (is.null(name) || length(name) != 1L || !nzchar(name)) {
    name <- paste0(id, ".txt")
  }
  name <- basename(as.character(name))
  if (!grepl("\\.[A-Za-z0-9]+$", name)) {
    name <- paste0(name, ".txt")
  }
  name
}

# --- the "Send to Galaxy" UI ------------------------------------------------
#
# One small piece of javascript, added to the page once, instead of a second
# button next to every download button: every download link shiny renders is an
# <a class="shiny-download-link" id="<output id>">, so the companion buttons can
# be injected wherever they appear - including the ones inside panels that are
# only rendered once an analysis has been run.
#
# The button asks the server for the file by output id
# (input$<prefix>_galaxy_send), which regenerates it with the registered content
# function and uploads it.
galaxy_ie_send_ui <- function() {
  if (!galaxy_ie_ready()) {
    # NULL outside Galaxy, so a plain `docker run` does not offer a button that
    # cannot work.
    return(NULL)
  }
  shiny::tags$script(shiny::HTML(galaxy_ie_send_js()))
}

galaxy_ie_send_js <- function() {
  prefix <- galaxy_ie$app$prefix
  # The prefix is interpolated into the JS strings below, so keep it to
  # identifier characters only.
  prefix <- gsub("[^A-Za-z0-9_]", "_", prefix)
  sprintf("
(function () {
  var inputName = '%1$s_galaxy_send';
  var pending = {};

  function idOf(anchor) {
    if (anchor.id) return anchor.id;
    var href = anchor.getAttribute('href') || '';
    var match = /[?&]w2=([^&]+)/.exec(href);
    return match ? decodeURIComponent(match[1]) : null;
  }

  function buttonFor(anchor, id) {
    var button = document.createElement('button');
    button.id = '%1$s_gx_send_' + id;
    button.type = 'button';
    button.className = 'btn btn-default btn-sm %1$s-gx-send';
    button.style.marginLeft = '6px';
    button.style.whiteSpace = 'nowrap';
    button.textContent = 'Send to Galaxy';
    button.title = 'Add this file to the Galaxy history of this session';
    button.addEventListener('click', function (event) {
      event.preventDefault();
      if (pending[id]) return;
      pending[id] = true;
      button.disabled = true;
      button.textContent = 'Sending...';
      Shiny.setInputValue(inputName, id, { priority: 'event' });
    });
    return button;
  }

  function release(id) {
    delete pending[id];
    var button = document.getElementById('%1$s_gx_send_' + id);
    if (!button) return;
    button.disabled = false;
    button.textContent = 'Send to Galaxy';
  }

  function inject() {
    var links = document.querySelectorAll('a.shiny-download-link');
    for (var i = 0; i < links.length; i++) {
      var anchor = links[i];
      var id = idOf(anchor);
      if (!id) continue;
      var key = '%1$s_gx_send_' + id;
      if (document.getElementById(key)) continue;
      anchor.parentNode.insertBefore(buttonFor(anchor, id), anchor.nextSibling);
    }
  }

  Shiny.addCustomMessageHandler('%1$s_galaxy_send_done', function (id) {
    release(id);
  });

  $(document).on('shiny:connected', function () { window.setTimeout(inject, 250); });
  $(document).on('shiny:value', function () { window.setTimeout(inject, 0); });
  $(document).on('shiny:bound', function () { window.setTimeout(inject, 0); });

  // Download buttons are rendered and removed as panels are shown, completed
  // and reset, so new ones have to be picked up while the session runs.
  var observer = new MutationObserver(function () { window.setTimeout(inject, 0); });
  observer.observe(document.documentElement, { childList: true, subtree: true });
})();
", prefix)
}

# --- the server side of "Send to Galaxy" ------------------------------------
#
# Wires up the registry and the observer. Returns the registry so the app can
# wrap its own downloadHandler()s with galaxy_ie_download_with(registry, id, ...).
#
# `label` is what the collection is called in the user-facing messages.
galaxy_ie_send_server <- function(input, output, session, label = "results") {
  registry <- galaxy_ie_registry()
  observeEvent(input[[paste0(galaxy_ie$app$prefix, "_galaxy_send")]], {
    id <- input[[paste0(galaxy_ie$app$prefix, "_galaxy_send")]]
    # Always let the browser re-enable its button, whatever happens below.
    on.exit(
      session$sendCustomMessage(paste0(galaxy_ie$app$prefix, "_galaxy_send_done"), id),
      add = TRUE
    )

    handler <- get0(id, envir = registry, inherits = FALSE)
    if (is.null(handler)) {
      showNotification(
        paste0("There is nothing to send for ", id, "."),
        type = "warning"
      )
      return()
    }

    name <- galaxy_ie_download_name(handler, id)
    # Write directly into the Galaxy job's discovered output directory. This
    # keeps the result available when the interactive job ends even if the API
    # upload below cannot reach Galaxy.
    path <- galaxy_ie_output_path(name)
    if (is.na(path)) {
      path <- file.path(tempdir(), paste0("galaxy_ie_", session$token, "_", name))
    }
    produced <- tryCatch(
      {
        handler$content(path)
        TRUE
      },
      error = function(e) {
        showNotification(
          paste0("The file could not be produced: ", conditionMessage(e)),
          type = "error",
          duration = 15
        )
        FALSE
      }
    )
    if (!produced || !file.exists(path)) {
      return()
    }

    persisted <- identical(
      dirname(path),
      normalizePath(galaxy_ie_output_dir(), mustWork = FALSE)
    )
    if (persisted) {
      upload_started <- tryCatch(
        {
          galaxy_ie_send_async(path, name)
          TRUE
        },
        error = function(e) {
          galaxy_ie_log("could not start direct upload name=", name, " error=", conditionMessage(e))
          FALSE
        }
      )
      galaxy_ie_log(
        "saved output name=", name,
        " path=", normalizePath(path, mustWork = FALSE),
        " bytes=", file.info(path)$size,
        " direct_upload=", if (upload_started) "started asynchronously" else "not started"
      )
      result <- list(
        ok = TRUE,
        message = paste0(
          "Saved ", name, ". Direct history upload was started in the background; ",
          "the file will also appear in ", label, " when this interactive job ends."
        )
      )
    } else {
      # Not running as a Galaxy interactive tool: there is no job directory to
      # discover, so send it and let the API be the only destination.
      result <- galaxy_ie_send(path, name)
    }

    showNotification(
      result$message,
      type = if (result$ok) "message" else "error",
      duration = if (result$ok) 8 else 15
    )
  })

  registry
}

# --- the history picker ------------------------------------------------------
#
# Lists the visible datasets and collection elements of the current history and
# downloads the selected ones into the app's main input slot, so the app picks
# them up through the same path it uses for the dataset the tool was given.
#
# Returns a list of choices for selectInput(). NULL when the session cannot
# reach Galaxy, so the app can skip rendering the picker entirely.
galaxy_ie_history_entries <- function() {
  if (!galaxy_ie_can_read()) {
    return(NULL)
  }
  history_cmd <- galaxy_ie_history_cmd()
  if (is.na(history_cmd)) {
    return(NULL)
  }

  output <- tryCatch(
    system2(
      history_cmd,
      c("--history-id", shQuote(galaxy_ie_history())),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) structure(character(), status = 1L)
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    galaxy_ie_log("history listing failed exit_status=", status)
    return(NULL)
  }
  text <- paste(as.character(output), collapse = "\n")
  if (!nzchar(trimws(text))) {
    return(NULL)
  }
  history <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (is.null(history) || is.null(history$contents)) {
    return(NULL)
  }

  contents <- history$contents
  # A history can be empty, and jsonlite drops the column entirely for a list of
  # zero rows, so treat anything without a usable hid column as "nothing yet".
  if (!is.data.frame(contents) || nrow(contents) == 0L || is.null(contents[["hid"]])) {
    return(NULL)
  }

  entries <- character(0)
  names <- character(0)
  for (i in seq_len(nrow(contents))) {
    hid <- contents$hid[i]
    if (is.null(hid) || is.na(hid)) next
    label <- as.character(contents$name[i])

    # A collection element carries the identifier it has inside the collection,
    # which is usually what identifies it to the user, so show it next to the
    # history id rather than making them look the dataset up separately.
    if ("element_identifier" %in% names(contents)) {
      element <- contents$element_identifier[i]
      if (!is.null(element) && !is.na(element) && nzchar(as.character(element))) {
        label <- paste0(label, " / ", as.character(element))
      }
    }

    entry <- paste0("#", hid, ": ", label, " (", as.character(contents$extension[i]), ")")

    # Mark the collection itself, because its history id is not something that can
    # be downloaded as a single file - the elements below it can.
    if ("history_content_type" %in% names(contents)) {
      kind <- as.character(contents$history_content_type[i])
      if (!is.na(kind) && kind == "dataset_collection") {
        entry <- paste0(entry, " [collection]")
      }
    }

    entries <- c(entries, entry)
    names <- c(names, as.character(hid))
  }
  if (!length(entries)) {
    return(NULL)
  }
  stats::setNames(entries, names)
}

# Downloads the entries chosen in the picker into the app's main input slot.
# Returns a message for the UI.
galaxy_ie_import <- function(selection) {
  import_env <- galaxy_ie$app$import_env
  if (is.null(import_env) || !nzchar(import_env)) {
    return(list(ok = FALSE, message = "This app does not support importing from history."))
  }
  get <- galaxy_ie_get()
  if (is.na(get)) {
    return(list(ok = FALSE, message = "galaxy_ie_helpers is not installed, so nothing can be read from Galaxy."))
  }
  if (!length(selection)) {
    return(list(ok = FALSE, message = "Select at least one dataset to import."))
  }

  # Where the staged input lives, if the tool was given one. Not required: the
  # import works without a staged dataset too, it just has nowhere obvious to put
  # the downloaded file, so it uses the job directory.
  staged <- Sys.getenv(import_env, unset = "")
  import_dir <- if (nzchar(staged)) {
    dirname(staged)
  } else {
    file.path(getwd(), "galaxy_ie_import")
  }
  dir.create(import_dir, recursive = TRUE, showWarnings = FALSE)

  hids <- names(selection)
  # `get` downloads into /import/<hid>, one file per hid, and flattens a
  # collection into its elements, so importing several hids at once works and
  # collection elements come back as individual files.
  output <- tryCatch(
    system2(
      get,
      c("-i", shQuote(hids), "-t", "hid", "--history-id", shQuote(galaxy_ie_history())),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 1L)
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    galaxy_ie_log("import failed hids=", paste(hids, collapse = ","), " exit_status=", status)
    return(list(
      ok = FALSE,
      message = paste0(
        "The import failed with exit code ", status, ":\n",
        paste(utils::tail(as.character(output), 20L), collapse = "\n")
      )
    ))
  }

  downloaded <- trimws(as.character(output))
  downloaded <- downloaded[nzchar(downloaded) & file.exists(downloaded)]
  if (!length(downloaded)) {
    return(list(ok = FALSE, message = "The datasets could not be downloaded from the history."))
  }
  if (length(downloaded) > 1L) {
    return(list(
      ok = FALSE,
      message = paste0(
        "That selection downloaded ", length(downloaded),
        " files, but this app takes a single input file. Select one, or import a ",
        "collection one element at a time."
      )
    ))
  }

  copied_path <- file.path(import_dir, basename(downloaded[1]))
  copied <- file.copy(downloaded[1], copied_path, overwrite = TRUE)
  if (!copied) {
    return(list(ok = FALSE, message = paste0("Could not write to ", copied_path, ".")))
  }

  # Every app reads its input path from this variable lazily, at the point where
  # it needs the data, so pointing it at the imported file is all it takes to
  # make the app switch over. The dataset the tool was originally given is left
  # alone, so this can be undone.
  #
  # The name has to be computed, and Sys.setenv() takes the names in the call as
  # the variable names, so a plain Sys.setenv(import_env = path) would set an
  # env var called "import_env" and leave the app reading the staged file.
  do.call(Sys.setenv, stats::setNames(list(copied_path), import_env))

  galaxy_ie_log(
    "import ok from=", downloaded[1],
    " to=", copied_path,
    " bytes=", file.info(copied_path)$size,
    " env=", import_env
  )
  list(
    ok = TRUE,
    message = paste0(
      "Imported ", basename(downloaded[1]), " (",
      file.info(copied_path)$size, " bytes) into the current input."
    )
  )
}

# The picker UI, to be placed in the app's sidebar. NULL outside Galaxy.
galaxy_ie_picker_ui <- function(id = "galaxy_ie_import", label = "Import from Galaxy history") {
  if (!galaxy_ie_can_read()) {
    return(NULL)
  }
  shiny::tagList(
    shiny::tags$hr(),
    shiny::tags$strong(label),
    shiny::tags$p(
      style = "color: #555; font-size: 12px; margin: 4px 0;",
      "Load a dataset or collection element from the history of this session ",
      "into the input above."
    ),
    shiny::uiOutput(paste0(id, "_choices")),

  )
}

# The picker server. Call from the app's server function; it wires the choice
# list, the button and the notification, and returns a reactive value holding the
# hids that were imported, so the app can refresh whatever depends on its input
# slot (its own file input, for instance).
galaxy_ie_picker_server <- function(input, output, session, id = "galaxy_ie_import") {
  selected <- shiny::reactiveVal(character(0))
  if (!galaxy_ie_can_read()) {
    return(selected)
  }

  output[[paste0(id, "_choices")]] <- shiny::renderUI({
    entries <- galaxy_ie_history_entries()
    if (is.null(entries) || !length(entries)) {
      shiny::helpText("This history has no datasets yet.")
      return(NULL)
    }
    shiny::selectizeInput(
      session, paste0(id, "_selected"),
      label = NULL,
      choices = entries,
      multiple = TRUE,
      options = list(placeholder = "Select dataset(s) or collection elements")
    )
  })

  shiny::observeEvent(input[[paste0(id, "_selected")]], {
    entries <- galaxy_ie_history_entries()
    chosen <- input[[paste0(id, "_selected")]]
    if (is.null(entries) || is.null(chosen) || !length(chosen)) return()
    selection <- entries[intersect(chosen, names(entries))]

    result <- galaxy_ie_import(selection)
    if (result$ok) {
      selected(names(selection))
    } else {
      selected(character(0))
    }
    shiny::showNotification(
      result$message,
      type = if (result$ok) "message" else "error",
      duration = if (result$ok) 8 else 15
    )
  }, ignoreInit = TRUE)

  selected
}