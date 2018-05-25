# Create a database for storing user activity from the Function.rmd learnr tutorial

library(tidyverse)
library(DBI)
library(RSQLite)


# Create a database
dbpath <- "/Users/alexstringer/phd/s18/leaf-ra/leaf2018/learnr-tutorials/evgeny/functions/"
function_db <- src_sqlite(stringr::str_c(dbpath,"functions.sqlite"),create=TRUE)


# Create a table for holding question information
# The easiest way to do this is to create a data_frame with the required schema and
# copy_to it into the created database

question_dataframe <- data_frame(
  user_id = "test_user",
  tutorial_id = "test_tutorial",
  tutorial_version = 1.0,
  timestamp = Sys.time(),
  question_label = "test_label",
  answer = "test_answer",
  correct = 0
)
copy_to(function_db,question_dataframe,"multiplechoice",temporary = FALSE,overwrite = TRUE)

# Check that it worked
test_that_it_worked <- RSQLite::dbSendQuery(function_db$con,
                                   "SELECT * FROM multiplechoice") %>%
  dbFetch()
test_that_it_worked

# Write another test row
dbSendQuery(function_db$con,
            "INSERT INTO multiplechoice VALUES ('test_user','test_tutorial',2.0,1527183448,'test_label','42',1)")


# Check that it worked
test_that_it_worked <- RSQLite::dbSendQuery(function_db$con,
                                            "SELECT * FROM multiplechoice") %>%
  dbFetch()
test_that_it_worked


# Create a table to store student code evaluations
code_dataframe <- data_frame(
  user_id = "test_user",
  tutorial_id = "test_tutorial",
  tutorial_version = 1.0,
  timestamp = Sys.time(),
  exercise_label = "test_label",
  code = "test_code",
  output = "test_output",
  error_message = "test_error",
  checked = 0,
  feedback_correct = 0,
  feedback_message = "test_feedback_message",
  feedback_type = "test_feedback_type",
  feedback_location = "test_feedback_location"
)
copy_to(function_db,code_dataframe,"codeevaluations",temporary = FALSE,overwrite = TRUE)

# Check that it worked
test_that_it_worked <- RSQLite::dbSendQuery(function_db$con,
                                            "SELECT * FROM codeevaluations") %>%
  dbFetch()
test_that_it_worked
