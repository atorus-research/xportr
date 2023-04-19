# function that can be used with mockery::stub to mock cli messages
cli_mocked_fun <- function(text, ...) {
  xportr_logger(text, "message")
}