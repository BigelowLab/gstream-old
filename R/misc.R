#' Read configuration
#' 
#' @export
#' @param filename char, the config file (with path)
#' @return configuration list
read_configuration = function(filename = "~/.gstream"){
  yaml::read_yaml(filename)
}
