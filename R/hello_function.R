#' A Hello Function
#'
#' This function allows you to say hello.
#' @param text_file_path file path to the text doc
#' @keywords hello
#' @export
#' @examples
#' hello_function()
hello_function <- function(filename) {
	droplist <- system.file("extdata", "droplist.txt", package = "senet")
	print(paste("hello", filename))
	print(paste("droplist: ", droplist))
}
