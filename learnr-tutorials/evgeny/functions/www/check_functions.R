# Checker functions for Function.rmd learnr tutorial

check_ex_fun_1 <- function(USER_CODE) {
  cat("I am checking the user code now\n")
  cat("Here is literally what was passed to the function: \n")
  print(USER_CODE)
  #cat("This is the code I am checking: ",deparse(USER_CODE$code[[1]]),"\n")
  
  code <- for_checkr(USER_CODE)
  
  result <- line_where(code,
                       passif(TRUE,"You passed!"))
  result
}

# checker <- function(label, user_code, check_code, envir_result, evaluate_result, ...) {
#   list(message = "Great job!", correct = TRUE, location = "append")
# }

code_to_check <- for_checkr(quote(sumx <- function(x) sum(x)))
check_ex_fun_1(code_to_check)


fx <- function(x) quo(x)
fx(2)
fx("potatoes")
