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
    library(lubridate)
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
#' Now that the data are read in, we need to merge the store-specific characteristics with the store x day data in the
#' training set. This need arises because our training data contains information for each store for many days, and
#' our prediction task is to predict day-level sales by store.
#' 
#' The practical concept here is that of a **key**: the most granular non-redundant level possible of unique identification
#' of a row in your dataset. A key can be one or more columns, and is used for uniquely identifying rows of a dataframe.
#' Dataframes sharing keys can be merged together.
#' 
#' For example, a key for the `store_read` table is the `Store` variable, because the variable is unique. We can check this:
#' 
store_read %>%
  nrow()

store_read %>%
  pull(Store) %>%
  unique() %>%
  length()

#' Suppose we added a column to `store_read` called `rowid`, just equal to the row number. Then `Store x rowid` would be
#' unique, but it would be redundant: both `Store` and `rowid` uniquely identify rows in that dataframe, so are not both
#' needed.
#' 
#' Now look at the `Store` variable in the `train-read` data:
#' 
train_read %>%
  nrow()

train_read %>%
  pull(Store) %>%
  unique() %>%
  length()

#' There are many more rows than stores. Why?
#' 
train_read %>%
  filter(Store == "1") %>%
  arrange(Date)

#' Each store has many rows in the dataframe, representing daily sales.
#' 
#' Check: how many unique days are there in the dataframe, and do all stores have data for all of them?
#' 
train_read %>%
  select(Date) %>%
  distinct()

train_read %>%
  group_by(Store) %>%
  summarize(numdays = n()) %>%
  group_by(numdays) %>%
  summarize(numtimes = n()) %>%
  arrange(desc(numtimes))

#' Interesting; most of the stores do have all 942 days of data. 180 of the stores are missing about 6 months worth
#' 
#' Check: is the `Store x Date` combination unique?
#' 
train_read %>%
  select(Store,Date) %>%
  distinct() %>%
  nrow()

train_read %>%
  nrow()

#' Yep.
#' 
#' We can merge `store_read` with `train_read` now. This is a **many-to-one** merge: each row in `store_read` will be *replicated*
#' for each occurance of the same `Store` value in `train_read`. That is, each store gets its data repeated for each day
#' that it occurs in `train_read`.
#' 
#' Before doing a merge, you need to know how many rows you expect to see in the result. The number of rows in the result
#' should be equal to the number of rows in `train_read`, since all stores are present in both dataframes.
#' 
#' Let's proceed using an *inner join*: rows will only be kept in the result table if the corresponding key values
#' are present in *both* merging tables. We can use the `dplyr` function `inner_join` to accomplish this as follows:
#' 
train_store <- train_read %>%
  inner_join(store_read,by = "Store")

glimpse(train_store)

#' Check that the join was successful: confirm the number of rows is as expected (using `glimpse` above), that all
#' variables are present, and that the key in the new table has the required uniqueness:
#' 
train_store %>%
  select(Store,Date) %>%
  distinct()

#' It appears that the join was successful! Let's do the same for the test set:
#' 
test_read %>%
  nrow()

test_store <- test_read %>%
  inner_join(store_read,by = "Store")

glimpse(test_store)

#' # Introduction to Feature Engineering and Preprocessing
#' 
#' Now that our data is read in and merged, we can start modelling.
#' 
#' ...or should we? We have our target variable defined already in the data (usually that
#' alone is a very time-consuming process, thanks Kaggle!), and we have a bunch of 
#' variables. Should we just pick a fancy model like a neural network, plug all the
#' variables into the precoded software, and call it a day?
#' 
#' It is almost never the case in a applied setting that someone will hand you a
#' dataset that is complete and ready to model. Much of the predictive modelling process
#' involves constructing useful modelling variables from the data you have in your possession.
#' 
#' ## Feature Engineering
#' 
#' The term **feature engineering** is typically used to describe the action of 
#' creating columns in a dataframe to be used in a predictive model, in two related situations.
#' When you have *structured* data as we have here, the data already resides in a nice matrix-like
#' format, and feature engineering involves creating new useful variables from what is supplied in
#' the data. Contrast this to when you have *unstructured* data, and feature engineering refers to the
#' act of putting the data into a matrix-like format suitable for modelling software- and deciding what
#' to put in the columns of this matrix. 
#' 
#' In our situation, we are dealing with structured data. Let's take a look at the variables in our
#' dataset, and think about how we could improve upon them. It is worth noting here that this step
#' is where the analyst gets to be creative, and depending on the task at hand can have a huge impact
#' on the predictive performance of the models considered. Any decisions made here are purely subjective,
#' and while following this tutorial, you can and should add or remove steps here to get a feel for the
#' effect this has on your models.
#' 
glimpse(train_store)

#' There are some potential new features that stand out:
#' 
#'   - `Date` has already been engineered a bit, producing the `DayOfWeek` variable. We might think
#'   sales would be different on different days of the week, so this makes sense. We might also think
#'   sales would be different in different months, e.g. a holiday effect. So, let's make a feature called
#'   `Month`, the month of the `Date`
#'   - We have the month and the year of the closest competition opening, but are these the most relevant
#'   summaries of this effect? If we think that having competition affects sales, it might matter more how
#'   long that competition has been around than exactly when it opened. Since we have the `Date`, we can
#'   use the `CompetitionOpenSinceMonth` and `CompetitionOpenSinceYear` to determine the length of time
#'   that the nearest competitor has been opened.
#'   - Similarly, we can do this to see how long `Promo2` has been in effect, rather than simply knowing when
#'   it was started.
#'   
#'   Let's implement these transformations. We will do this using a `dplyr` pipeline, for efficiency and readability.
#'   Since we have to do the same transformations to both the train and the test sets, it is best practice to create
#'   a function that does the transformation:
#'   
#+ cache=TRUE
feature_engineer <- function(ds) {
  # Function to create a date from a year and week
  yw <- function(y,w) {
    dt <- ymd(str_c(y,"0101"))
    week(dt) <- as.numeric(w)
    dt
  }
  
  ds %>%
    mutate(Month = as.character(lubridate::month(Date)),
           comp_open_month_formatted = if_else(str_length(CompetitionOpenSinceMonth) == 1,str_c("0",CompetitionOpenSinceMonth),CompetitionOpenSinceMonth),
           comp_open_date = lubridate::ymd(str_c(CompetitionOpenSinceYear,comp_open_month_formatted,"01")),
           comp_open_since_days = as.numeric(Date - comp_open_date),
           promo2_since_date = yw(Promo2SinceYear,Promo2SinceWeek),
           promo2_since_days = as.numeric(Date - promo2_since_date)
    ) %>%
    select(-comp_open_month_formatted,comp_open_date,promo2_since_date)
}

# Do it to the training data
train_feateng <- feature_engineer(train_store)
glimpse(train_feateng)

# ...and the test data
test_feateng <- feature_engineer(test_store)
glimpse(test_feateng)

#' The above transformations had to do almost exclusively with dates. Working with dates is a useful skill, so
#' let's unpack the above a bit. In order to create the `comp_open_since_days` variable, we
#' 
#'   - Reformatted the `CompetitionOpenSinceMonth` variable. This is an annoying thing that happens when you convert
#'   numeric month indicators to character: October - December, months 10 - 12, have 2 characters while the rest of
#'   the months have 1. This throws of date-parsing functions like `lubridate::ymd`, which take a character string
#'   of the form "yyyymmdd", "yyyy-mm-dd", etc, and expect the month to be 2 characters, so "01" instead of "1"
#'   - Computed the actual date that the competitor opened from their year and month of opening by concatenating these
#'   together and passing the result to `lubridate::ymd` in the form `20140428` (for example). This creates a `lubridate`
#'   date object, which has a standard internal format useful for calculations. Note that when we read in the data to begin
#'   with, `readr` already parsed `Date` into this same format
#'   - With `Date` and `comp_open_date` in internally-compatible formats, simply taking their difference returns
#'   the days difference between them, giving the `comp_open_since_days` feature.
#'   
#' The approach for computing `promo2_since_days` is similar, however because the data had the week of the year in
#' which promo2 began, instead of the month, we built a simple custom function for constructing a `lubridate` date 
#' from the week and year.
#' 
#' The point of this exercise is this: feature engineering is tedious and difficult. It is absolutely necessary
#' to have these skills in predictive modelling; in more complex problems with bigger datasets, this task gets
#' much more difficult and time-consuming, but doesn't become less necessary.
#' 
#' ## Preprocessing
#' 
#' While feature engineering typically refers to the construction of new columns in the modelling dataframe,
#' feature **preprocessing** typically refers to the actual modification of data values in order to make the
#' data more model-friendly. This can involve creating indicator variables for events, imputing missing values
#' (if appropriate), deleting variables that are highly missing or have low standard deviation or are completely
#' uncorrelated with the target or are too correlated with other variables or... this is another tedious, difficult
#' step in which the analyst has freedom to be creative, and also the power to drastically change the predictive
#' performance of models built on the resulting data.
#' 
#' Let's look again at our variables:
#' 
glimpse(train_feateng)
