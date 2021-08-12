#' Extract metadata from a DOCX file
#' 
#' @param path character vector, paths to DOCX files
#' 
#' 
docxmeta <- function(path) {
  requireNamespace("xml2")
  requireNamespace("purrr")
  requireNamespace("tidyr")
  isthere <- file.exists(path)
  if(any(!isthere))
    stop("non-existing files:", paste(path[!isthere], collapse=", "))
  # list of xpath expr for every file within DOCX
  
  do1 <- function(fname) {
    r <- purrr::imap(
      docxmeta_xpaths,
      function(xpv, fn)
      {
        doc <- xml2::read_xml(unzip(fname, fn))
        map_chr(xpv, ~ xml2::xml_text(xml2::xml_find_first(doc, .x, ns=xml2::xml_ns(doc))))
      }
    )
    c(
      list(
        file_name = basename(fname)
      ),
      r
    )
  }
  
  lapply(
    lapply(path, do1, xpaths=xpaths),
    lapply, tibble::enframe
  )
}

# List of XPATHs per file within a DOCX
docxmeta_xpaths <- list(
  'docProps/core.xml' = c(
    title = "/*/dc:title",
    subject = "/*/dc:subject",
    creator = "/*/dc:creator",
    keywords = "/*/cp:keywords",
    description = "/*/dc:description",
    lastmodifiedby = "/*/cp:lastModifiedBy",
    revision = "/*/cp:revision",
    date_created = "/*/dcterms:created",
    date_modified = "/*/dcterms:modified"
  ),
  'docProps/app.xml' = c(
    characters = "/*/d1:Characters",
    words = "/*/d1:Words"
  )
)


if(FALSE) {
  # path to docx tree
  pth <- "/mnt/c/Users/mbojanowski/OneDrive - Akademia Leona Kozminskiego/Dokumenty/alk-ekonometria-prace"
  lf <- list.files(path=pth, pattern="\\.docx$", full.names=TRUE, ignore.case = TRUE, recursive = TRUE)
  fpth <- lf[1]
  
  doc <- xml2::read_xml(unzip(fpth, "docProps/app.xml"))
  xml2::xml_find_first(doc, ".//d1:Words", ns=xml2::xml_ns(doc))
  
  r <- docxmeta(fpth)
  r <- docxmeta(lf[1:10])
}
