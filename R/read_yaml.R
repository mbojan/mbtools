#' Parses YAML file with a simple R handler
#' 
#' @param string YAML string to read
#' @param input YAML file to read
#' @param ... other arguments
#' 
#' @export
#' 
#' @examples
#' y <- "num: 1
#' key0:
#'   key1: value1
#' key2:
#'   key22: value22
#' rcode_num: !r 1:5
#' rcode_ch: !r paste(LETTERS[1:10], collapse=', ')
#' date: !r Sys.Date()
#' "
#' read_yaml(y)



read_yaml <- function(string, ...) {
  yaml::yaml.load(
    string=string,
    handlers = yaml_handlers,
    ...
  )
}

#' @rdname read_yaml
#' @export
read_yaml_file <- function(input, ...) {
  yaml::yaml.load_file(
    input=input,
    handlers = yaml_handlers,
    ...
  )
  
}



yaml_handlers <- list(
  r = function(x) eval(parse(text=x))
)


#' @rdname read_yaml
