# Checker functions for Function.rmd learnr tutorial


# One correct submission
c1 <- quote({
  sum_vec <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x,na.rm=na.rm)
    s <- sd(x,na.rm=na.rm)
    mis <- sum(is.na(x))
    Result <- c(round(m,d),round(s,d),mis)
    names(Result) <- c("Mean","SD","Miss")
    return(Result)
  }
  sum_vec(x=c(4,6,1,NA,1,5,NA),d=2,na.rm=TRUE)
})

# Some examples of potential student submissions
s1 <- quote({
    other_function <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x,na.rm=na.rm)
    s <- sd(x,na.rm=na.rm)
    mis <- sum(is.na(x))
    Result <- c(round(m,d),round(s,d),mis)
    names(Result) <- c("Mean","SD","Miss")
    return(Result)
    }
}) # Names the function wrong

s2 <- quote({
  sum_vec <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x,na.rm=na.rm)
    s <- sd(x,na.rm=na.rm)
    Result <- c(round(m,d),round(s,d))
    names(Result) <- c("Mean","SD")
    return(Result)
  }
  sum_vec(x=c(4,6,1,NA,1,5,NA),d=2,na.rm=TRUE)
}) # Forgets either mean, standard deviation, or number of missing values

s3 <- quote({
  sum_vec <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x,na.rm=na.rm)
    s <- sd(x,na.rm=na.rm)
    mis <- sum(is.na(x))
    Result <- c(m,s,mis)
    names(Result) <- c("Mean","SD","Miss")
    return(Result)
  }
  sum_vec(x=c(4,6,1,NA,1,5,NA),d=2,na.rm=TRUE)
}) # Doesn't round the answer

s4 <- quote({
  sum_vec <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x,na.rm=na.rm)
    s <- sd(x,na.rm=na.rm)
    mis <- sum(is.na(x))
    Result <- c(round(m,d),round(s,d),mis)
    names(Result) <- c("Mean","SD","Miss")
    return(Result)
  }
}) # Doesn't call the function afterwards

s5 <- quote({
  sum_vec <- function(x, d=0, na.rm=FALSE) {
    m <- mean(x)
    s <- sd(x)
    mis <- sum(is.na(x))
    Result <- c(round(m,d),round(s,d),mis)
    names(Result) <- c("Mean","SD","Miss")
    return(Result)
  }
  sum_vec(x=c(4,6,1,NA,1,5,NA),d=2,na.rm=TRUE)
}) # Doesn't remove nas from mean and standard deviation


check_ex_fun_1 <- function(USER_CODE) {
  # cat("I am checking the user code now\n")
  # cat("Here is literally what was passed to the function: \n")
  # print(USER_CODE)
  # cat("This is the code I am checking: ",deparse(USER_CODE$code[[1]]),"\n")
  
  code <- for_checkr(USER_CODE)
  
  # Messages corresponding to the mistakes above
  mc1 <- "You got it, good job!" # Correct
  m1 <- "Remember to name the function sum_vec. You named the function {{Z}}"
  m2 <- "Remember to have your function calculate the mean, standard deviation, and number of missing values. "
  m3 <- "Remember to round the answer to d decimal places"
  m4 <- "Remember to call the function with the specified arguments after you create it"
  m5 <- "Remember to have your function handle missing values in mean() and sd() according to the na.rm argument"
  
  # Check problem 1
  
  check1 <- line_where(code,
                       failif(Z != "sum_vec",m1),
                       passif(TRUE,"{{Z}}"))
  if (failed(check1)) {
    return(check1)
  }
  
  # Check problem 2
  # checkr's line_calling function can't handle scoping- can't find call to mean() within user-written function
  # Manually grep out mean, sd, is.na
  ln <- deparse(code$code[[1]]) # Will fail if function is not the first line of code
  has_mean <- any(stringr::str_detect(ln,"mean"))
  has_sd <- any(stringr::str_detect(ln,"sd"))
  has_isna <- any(stringr::str_detect(ln,"is.na"))
  
  check2_mean <- line_where(code,failif(!has_mean,stringr::str_c(m2,"I did not see you use the mean() function anywhere.")))
  check2_sd <- line_where(code,failif(!has_sd,stringr::str_c(m2,"I did not see you use the sd() function anywhere.")))
  check2_isna <- line_where(code,failif(!has_isna,stringr::str_c(m2,"I did not see you use the is.na() function anywhere.")))
    
  if (failed(check2_mean)) {
    return(check2_mean)
  }
  else if (failed(check2_sd)) {
    return(check2_sd)
  }
  else if (failed(check2_isna)) {
    return(check2_isna)
  }
  
  # Check problem 3
  # Just check output for rounding to two decimal places
  check3 <- line_where(code,
                       #failif(F == "function","First line"),
                       failif(if (F == "sum_vec") {
                         cat(V,"\n")
                            any(round(V,2) != V) 
                          }
                          else {
                            FALSE
                          },
                         "Remember to round your answer to two decimal places"),
                       passif(F == "sum_vec","Good job!"))
  
  if (failed(check3)) {
    return(check3)
  }
  
  # Check problem 4
  # sum_vec <- function(x) x
  # tryCatch({
  #   check4 <- line_calling(code,sum_vec,message = "Remember to call your function after creating it")
  # 
  #   if(failed(check4)) {
  #     return(check4)
  #   }
  # })
  # 
  
  # Check problem 5
  
  check5 <- line_where(code,failif(if (F == "sum_vec") {
                                      any(is.na(V))
                                    }
                                    else {
                                      FALSE
                                    },"Remember to remove NAs from the answer if needed"),
                       passif(F == "sum_vec","Good job!"))

  if(failed(check5)) {
    return(check5)
  }

  check5
}

check_ex_fun_1(c1)
check_ex_fun_1(s1)
check_ex_fun_1(s2)
check_ex_fun_1(s3)
check_ex_fun_1(s4)
check_ex_fun_1(s5)

