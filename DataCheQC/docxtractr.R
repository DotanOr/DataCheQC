library(xml2, quietly = TRUE)
# Collection of functions from lib. docxtractr adjusted for R 3.5.2
is_doc <- function(path) { tolower(tools::file_ext(path)) == "doc" }

is_url <- function(path) { grepl("^(http|ftp)s?://", path) }

ensure_docx <- function(docx) {
  if (!inherits(docx, "docx")) stop("Must pass in a 'docx' object", call.=FALSE)
  if (!(all(purrr::map_lgl(c("docx", "ns", "tbls", "path"), exists, where=docx))))
    stop("'docx' object missing necessary components", call.=FALSE)
}

docx_tbl_count <- function(docx) {
  ensure_docx(docx)
  length(docx$tbls)
}

file_copy <- function(from, to) {
  fc <- file.copy(from, to)
  if (!fc) stop(sprintf("file copy failure for file %s", from), call.=FALSE)
}

docx_extract_tbl <- function(docx, tbl_number=1, header=TRUE, preserve=FALSE, trim=TRUE) {
  
  ensure_docx(docx)
  if ((tbl_number < 1) | (tbl_number > docx_tbl_count(docx))) {
    stop("'tbl_number' is invalid.", call.=FALSE)
  }
  
  if (preserve) trim <- FALSE
  
  ns <- docx$ns
  tbl <- docx$tbls[[tbl_number]]
  
  cells <- xml2::xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
  rows <- xml2::xml_find_all(tbl, "./w:tr", ns=ns)
  
  purrr::map_df(rows, ~{
    res <- xml2::xml_find_all(.x, "./w:tc", ns=ns)
    if (preserve) {
      purrr::map(res, ~{
        paras <- xml2::xml_text(xml2::xml_find_all(.x, "./w:p", ns=ns))
        paste0(paras, collapse="\n")
      }) -> vals
    } else {
      vals <- xml2::xml_text(res, trim=trim)
    }
    names(vals) <- sprintf("V%d", 1:length(vals))
    as.list(vals)
    # data.frame(as.list(vals), stringsAsFactors=FALSE)
  }) -> dat
  
  if (header) {
    hopeful_names <- make.names(dat[1,])
    colnames(dat) <- hopeful_names
    dat <- dat[-1,]
  } else {
    hdr <- has_header(tbl, rows, ns)
    if (!is.na(hdr)) {
      message("NOTE: header=FALSE but table has a marked header row in the Word document")
    }
  }
  
  rownames(dat) <- NULL
  
  class(dat) <- c("tbl_df", "tbl", "data.frame")
  dat
  
}

has_header <- function(tbl, rows, ns) {
  
  # microsoft has a tag for some table structure info. examine it to
  # see if the creator of the header made the first row special which
  # will likely mean it's a header candidate
  look <- try(xml2::xml_find_first(tbl, "./w:tblPr/w:tblLook", ns), silent=TRUE)
  if (inherits(look, "try-error")) {
    return(NA)
  } else {
    look_attr <- xml2::xml_attrs(look)
    if ("firstRow" %in% names(look_attr)) {
      if (look_attr["firstRow"] == "0") {
        return(NA)
      } else {
        return(paste0(xml2::xml_text(xml_find_all(rows[[1]], "./w:tc", ns)), collapse=", "))
      }
    } else {
      return(NA)
    }
  }
  
}

read_docx <-
  function (path, track_changes = NULL) 
  {
    stopifnot(is.character(path))
    if (!is.null(track_changes)) {
      track_changes <- match.arg(track_changes, c("accept", 
                                                  "reject"))
    }
    tmpd <- tempdir()
    tmpf <- tempfile(tmpdir = tmpd, fileext = ".zip")
    is_input_doc <- is_doc(path)
    if (is_input_doc) {
      lo_assert()
      tmpf_doc <- tempfile(tmpdir = tmpd, fileext = ".doc")
      tmpf_docx <- gsub("\\.doc$", ".docx", tmpf_doc)
    }
    else {
      tmpf_doc <- NULL
      tmpf_docx <- NULL
    }
    on.exit({
      unlink(tmpf)
      unlink(tmpf_doc)
      unlink(tmpf_docx)
      unlink(sprintf("%s/docdata", tmpd), recursive = TRUE)
    })
    if (is_url(path)) {
      if (is_input_doc) {
        res <- httr::GET(path, write_disk(tmpf_doc))
        httr::stop_for_status(res)
        convert_doc_to_docx(tmpd, tmpf_doc)
        file_copy(tmpf_docx, tmpf)
      }
      else {
        res <- httr::GET(path, write_disk(tmpf))
        httr::stop_for_status(res)
      }
    }
    else {
      path <- path.expand(path)
      if (!file.exists(path)) 
        stop(sprintf("Cannot find '%s'", path), call. = FALSE)
      if (is_input_doc) {
        file_copy(path, tmpf_doc)
        convert_doc_to_docx(tmpd, tmpf_doc)
        file_copy(tmpf_docx, tmpf)
      }
      else {
        file_copy(path, tmpf)
      }
    }
    if (!is.null(track_changes)) {
      pandoc_bin <- Sys.which("pandoc")
      if (pandoc_bin == "") {
        pandoc_bin <- Sys.getenv("RSTUDIO_PANDOC")
        if (pandoc_bin == "") {
          warning("Track changes option was used but no pandoc binary was found. ", 
                  "Please ensure that the directory containing pandoc is available ", 
                  "on the system PATH and restart the R session before trying again. ", 
                  "Reading in document *without* tracking any changes.")
        }
      }
      if (pandoc_bin != "") {
        system2(command = pandoc_bin, args = c("-f", 
                                               "docx", "-t", "docx", "-o", 
                                               tmpf, sprintf("--track-changes=%s", track_changes), 
                                               tmpf))
      }
    }
    unzip(tmpf, exdir = sprintf("%s/docdata", tmpd))
    doc <- xml2::read_xml(sprintf("%s/docdata/word/document.xml", 
                                  tmpd))
    ns <- xml2::xml_ns(doc)
    tbls <- xml2::xml_find_all(doc, ".//w:tbl", ns = ns)
    if (file.exists(sprintf("%s/docdata/word/comments.xml", 
                            tmpd))) {
      docmnt <- read_xml(sprintf("%s/docdata/word/comments.xml", 
                                 tmpd))
      cmnts <- xml2::xml_find_all(docmnt, ".//w:comment", 
                                  ns = ns)
    }
    else {
      cmnts <- xml2::xml_find_all(doc, ".//w:comment", 
                                  ns = ns)
    }
    docx <- list(docx = doc, ns = ns, tbls = tbls, cmnts = cmnts, 
                 path = path)
    class(docx) <- "docx"
    docx
  }

docx_extract_all_tbls <- function(docx, guess_header=TRUE, preserve=FALSE, trim=TRUE) {
  
  ensure_docx(docx)
  if (docx_tbl_count(docx) < 1) return(list())
  
  ns <- docx$ns
  
  purrr::map(1:docx_tbl_count(docx), function(i) {
    hdr <- FALSE
    if (guess_header) {
      tbl <- docx$tbls[[i]]
      rows <- xml2::xml_find_all(tbl, "./w:tr", ns=ns)
      hdr <- !is.na(has_header(tbl, rows, ns))
    }
    docx_extract_tbl(docx, i, hdr, preserve, trim)
  })
  
}