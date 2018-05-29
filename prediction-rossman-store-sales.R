#' ---
#' title: "Introductory Predictive Modelling" 
#' subtitle: "Rossman Store Sales Kaggle Competition"
#' author: "Alex Stringer"
#' date: '`r Sys.Date()`'
#' output: 
#'   html_document:
#'     toc: true
#' editor_options: 
#'   chunk_output_type: console
#' ---
  
#' This notebook is to serve as an introduction to predictive modelling in R, using
#' the Rossman Store Sales Kaggle competition dataset, available from [kaggle](https://www.kaggle.com/c/rossmann-store-sales/data)
#' or stored on [github](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets/rossman)
#' 
#' Topics covered:
#' 
#'   - Understanding the problem reading data documentation
#'   - Reading data into R and checking that the results match what is expected,
#'   based on the documentation
#'   - Merging multiple datasets in preparation for analysis
#'   - Exploratory data analysis and feature preprocessing
#'   - Prediction using linear regression and LASSO
#'   - Evaluating predictions on the training and validation sets
#'   - Prediction using linear mixed effects models
#'   - Prediction using multi-level Bayesian heirarchical modelling
#'   - Formatting and submitting test-set predictions to Kaggle programatically
  
# Load the tidyverse packages
suppressMessages({
  suppressWarnings({
    library(tidyverse)
  })
})


#' # The Problem
#' 
#' ## Description
#' 
#' Before we even start reading the data into R, we need to understand the problem as documented. Before beginning
#' any predictive modelling exercise, you should be writing a detailed description of the problem and all available
#' data sources. This will help keep the project organized, and facilitate collaboration within your team. 
#' 
#' This is a Kaggle competition; the documentation is available [here](https://www.kaggle.com/c/rossmann-store-sales)
#' 
#' There are a variety of factors affecting store sales. Rossman is a company operating drug stores in Europe, whose store managers
#' are requird to predict their stores' sales up to 6 weeks in advance. The goal of this competition is to use
#' provided data on the stores to develop a predictive model for store sales.
#' 
#' ## Available Data
#' 
#' The provided data contain information at two levels:
#' 
#'   - The **store** level, file **store.csv** with one row per store. Contains information such as the "store type", 
#'   distance to the nearest competitor, and so on, that would be expected to affect a store's sales overall
#'   - The **store x day** level, file **train.csv** with one row per store per day. Contains information on each store
#'   that is specific to the day, such as whether it was a holiday, whether any promo was happening, etc. Also includes
#'   the target variable, **sales**, and a related variable **customers** representing the number of customers in the store
#'   on that day.
#'   
#' The goal of our analysis is going to be to predict day-level sales, so we know we are going to need to use both
#' the store and the store x day information. These will need to be merged after reading in.
#' 
#' # Reading in the Data
#' 
#' ## Figuring out what to expect
#' 
#' Before reading in the data, do your best to understand what to expect: how many rows and columns should be in the file?
#' What are the data types and reasonable value ranges for each column? This can be figured out from reading the documentation
#' if available.
#' 
#' Regardless of whether documentation is available, if you are reading in text files, you should first view them in
#' a terminal. This allows you to verify whether the file has a header, what type of separator it uses (e.g. if it is
#' a .csv file, is it really comma-separated?), whether the file has any encoding challenges, and more. To view the file
#' using BASH (default terminal on UNIX systems, so Linux and Mac), navigate to the folder containing the file and type
#' 
#' `head train.csv`
#' 
#' This will print out the first five rows of the file. We verify here that our file does have a header, and has 9 columns.
#' 
#' To count the rows, use the `wc -l` BASH command:
#' 
#' `wc -l train.csv`
#' 
#' This should output `1017210 train.csv`, indicating that there are 1,017,210 rows in the `train.csv` file. You can run
#' `wc -l *` to list the line counts for all files in the current folder. We see that `test.csv` has 41,089 rows, and
#' `store.csv` has 1,116. So there are 1,116 stores- we can verify whether the training and test datasets together contain
#' this number of stores once we read in the data.
#' 
#' To count the columns in a text file if you know the separator, in this case `,`, just count the number of separators
#' in the first line (and add one!)
#' 
#' `head -n1 train.csv | tr -cd , | wc -c`
#' 
#' This command has 3 parts:
#' 
#'   - The `head -n1 train.csv` command returns the first row (`-n1`) of the dataset `train.csv`
#'   - The `|` command (the "pipe") takes the return value of `head -n1 train.csv` and passes it to the next command.
#'   This works exactly like the `%>%` operator in `R` (in fact, this is what `%>%` was modelled after, and is why
#'   it is referred to as the "pipe")
#'   - `tr -cd ,` removes all characters from the line that are not commas
#'   - `wc -c` counts the characters in the result. Since the result is only the commas in the line, this counts the
#'   number of commas in the line. `wc` stands for "word count"; the `-c` flag counts characters, and the `-l` flag you saw
#'   above counts lines (which is the same as `tr -cd '\n' | wc -c`)
#'   
#' ## Reading in the files
#' 
#' Text files can be read in with the `read_delim` function in the `readr` package. There is a short form `read_csv`
#' specifically for reading comma-separated data, however it is just a wrapper around the more general (and therefore useful)
#' `read_delim`.
#'
#+ cache=TRUE
# Set the path where the data is stored
datapath <- "https://github.com/awstringer1/leaf2018/raw/gh-pages/datasets/rossman/"
# Read in the three files
train_read <- readr::read_delim(file = str_c(datapath,"train.csv"),
                                delim = ",",
                                col_names = TRUE)

#' This generated errors. What happened?
#' 
#' Reading the documentation for `read_delim`, we see that the function tries to intelligently guess the datatypes of the
#' columns using the first few rows of data. But, if there is data farther down that does not match the guessed type,
#' it doesn't re-guess, rather it throws an error. 
#' 
#' There are two ways to fix this, depending on the complexity of the dataset:
#'   - Set the `guess_max` parameter of `read_delim` to a number greater than the default of `1000`. This
#'   will use more rows for guessing the column types, reducing the chances of a mistake. This can slow down the reading
#'   of the file, and is not recommended for large files
#'   - Just look at the data and manually set the column datatypes
#'   
#' Except in situations with very easy to debug type-guessing errors, the second approach is strongly recommended as
#' it is is robust, and guaranteed to be correct, up to *human* error.
#' 
#' Looking at the data on the command line (thankfully there are only 9 columns!) we see the following variables:
#' 
#' `head -n1 train.csv | tr ',' '\n'`
#' 
#'   - "Store"
#'   - "DayOfWeek"
#'   - "Date"
#'   - "Sales"
#'   - "Customers"
#'   - "Open"
#'   - "Promo"
#'   -  "StateHoliday"
#'   - "SchoolHoliday"
#'   
#' From this, we can manually set the data types of the columns to be something reasonable. Included are the high-level 
#' data types and the `readr` single-letter codes; see the documentation for `read_delim`:
#' 
#'   - Character, 'c'
#'   - Character, 'c'
#'   - Date, 'D'
#'   - Numeric, 'd' (the 'd' is for "double" as in "double-precision floating-point number")
#'   - Numeric, 'd'
#'   - Character, 'c'
#'   - Character, 'c'
#'   - Character, 'c'
#'   - Character, 'c'
#'   
#' Why did we set so many variables that only take numeric values, like DayOfWeek and Promo, to character datatypes?
#' For modelling purposes, these variables aren't really numeric; DayOfWeek could just as well have been stored as
#' "Monday", "Tuesday",... the fact that whoever extracted these data chose to code the variables a certain way has no
#' bearing on their true underlying "type" in the context of building a predictive model. 
#' 
#' Trying again with the manual column types:
#' 
#+ cache=TRUE
train_read <- readr::read_delim(file = str_c(datapath,"train.csv"),
                                delim = ",",
                                col_names = TRUE,
                                col_types = c("ccDddcccc"))

glimpse(train_read)

#' `glimpse`ing the data tells us what we need to know: the data were read in with the correct types, and there are
#' the correct number of rows and columns.
#' 
#' Repeating these steps on `test.csv` and `store.csv`:
#' 
#+ cache=TRUE

test_read <- readr::read_delim(file = str_c(datapath,"test.csv"),
                               delim = ",",
                               col_names = TRUE,
                               col_types = c("cccDcccc"))

glimpse(test_read)

store_read <- readr::read_delim(file = str_c(datapath,"store.csv"),
                                delim = ",",
                                col_names = TRUE,
                                col_types = c("cccdcccccc"))

glimpse(store_read)

#' We were able to read in the data successfully.
#' 
#' # Merging Datasets
#' 
#'
