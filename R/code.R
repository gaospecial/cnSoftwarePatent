#' Print R package source code in Markdown format
#'
#' @param path root directory of your package source code
#' @param ignore ignore files, use regexp here
#' @param pattern source code file, use regexp here
#'
#' @return
#' @export
#'
#' @examples code()
code <- function (path = ".", ignore = ".Rbuildignore", pattern = "(R|md|Rd|Rmd|DESCRIPTION|NAMESPACE)$"){
  files = list.files(path = path, recursive = TRUE, full.names = TRUE, pattern = pattern)
  ignore = readLines(ignore)
  ignore = gsub("#.*","",ignore)
  pat = paste0(ignore, collapse = "|")
  files = files[grep(pat, files, invert = TRUE)]
  success = lapply(files, function(x){
    paste("##", x) %>% cat()
    cat("\n\n")
    import_example(x) %>% cat()
    cat("\n\n")
  })
  cat("\n\nAll source codes are exported successfully. \n")
}

import_example <- function(file, lang = xfun::file_ext(file)) {
  x = xfun::read_utf8(xfun::magic_path(file))
  lang = tolower(lang)
  if (nchar(lang) > 1) {
    lang = sub('^r', '', lang)
    if (lang == 'nw') lang = 'tex'
  }
  knitr::asis_output(paste(c(sprintf("````%s", lang), x, "````"), collapse = '\n'))
}
