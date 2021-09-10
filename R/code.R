#' Print R package source code in Markdown format
#'
#' @param path root directory of your package source code
#' @param ignore ignore files, by default specified in .Rbuildignore
#' @param pattern source code file, use regexp here
#' @param file word document containing source code
#'
#' @return
#' @export
#'
#' @examples code()
code <- function (path = ".",
                  ignore = ".Rbuildignore",
                  pattern = "\\.(R|md|Rd|DESCRIPTION|NAMESPACE)$",
                  file = "sourcecode.docx"){
  files = list.files(path = path, recursive = TRUE, full.names = TRUE, pattern = pattern)
  ignore = readLines(ignore)
  ignore = gsub("#.*","",ignore)
  pat = paste0(ignore, collapse = "|")
  files = files[grep(pat, files, invert = TRUE)]
  tmpfile = tempfile(fileext = ".rmd")
  content = lapply(files, function(x){
    paste0("\n\n", "## ", x, "\n\n", print_source(x),"\n\n")
  })
  content = do.call("paste0", content)
  xfun::write_utf8(content, con = tmpfile)
  file =  paste0(fs::path_real(path), .Platform$file.sep, file)
  rmarkdown::render(tmpfile, output_file = file, output_format = "word_document")
}

print_source <- function(file, lang = xfun::file_ext(file)) {
  x = xfun::read_utf8(xfun::magic_path(file))
  lang = tolower(lang)
  if (nchar(lang) > 1) {
    lang = sub('^r', '', lang)
    if (lang == 'nw') lang = 'tex'
  }
  knitr::asis_output(paste(c(sprintf("````%s", lang), x, "````"), collapse = '\n'))
}
