#' ---
#' title: "Predictive Modelling - Data Processing" 
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
#' or stored on [github](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets/rossman). Predictive modelling is
#' tedious and difficult, and this notebook is long. It should take you a few hours to go through in detail, including
#' running all the code yourself. The reader is encouraged to modify the code where appropriate to see the effects of
#' changing steps, and to try to get a better predicting model.
#' 
#' The result of this notebook is a cleaned, model-ready dataset which will be used in subsequent tutorials on
#' fitting predictive models to data. The purpose of this notebook is not to talk about specific models or methods; 
#' rather it is to illustrate the long, tedious process of data engineering and preprocessing that is required before
#' predictive modelling methodologies can be applied to real-world data.
#' 
#' Topics covered:
#' 
#'   - Understanding the problem, reading data documentation
#'   - Reading data into R and checking that the results match what is expected,
#'   based on the documentation
#'   - Merging multiple datasets in preparation for analysis
#'   - Exploratory data analysis and feature preprocessing
#'   - Prediction using linear regression
#'   - Evaluating predictions on the training and validation sets
  
# Load the tidyverse packages
suppressMessages({
  suppressWarnings({
    library(tidyverse) # Includes all the necessary stuff: dplyr, ggplot, tidyr, and so on
    library(lubridate) # For dealing with dates
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
#' This is a Kaggle competition; the documentation is available [here](https://www.kaggle.com/c/rossmann-store-sales).
#' Go and read it in detail, and make notes.
#' 
#' There are a variety of factors affecting store sales. Rossman is a company operating drug stores in Europe, whose store managers
#' are required to predict their stores' daily sales up to 6 weeks in advance. The goal of this competition is to use
#' provided data on the stores to develop a predictive model for store sales on a daily basis.
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
#' using BASH (default terminal on UNIX systems, so Linux and Mac; if you're on windows, you can download 
#' [git bash](https://git-scm.com/download/win)), navigate to the folder containing the file and type
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
#' `store.csv` has 1,116. So there are 1,115 stores (if you printed the file out, you would notice the first row
#' is a header row containing column names)- we can verify whether the training and test datasets together contain
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
#' 
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

#' # Feature Engineering and Preprocessing
#' 
#' Now that our data is read in and merged, we can start modelling.
#' 
#' ...or can we? We have our target variable defined already in the data (usually that
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
#' creating columns in a dataframe to be used in a predictive model, in one of two related situations.
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

#' We will do the following preprocessing. You are encouraged to add/remove preprocessing steps as you see fit;
#' the possibilities are endless and have a nontrivial effect on the results, so you definitely should play
#' around here.
#' 
#'   - Remove any variables with very low standard deviation, or a very high proportion of missing values.
#'   Variables with low standard deviation won't inform the model (they can't have a relationship with $y$ if
#'   they themselves don't change), and variables that are mostly missing can't be used unless you want to get
#'   really creative (e.g. check if missingness is related to the response; we won't bother here).
#'   - Impute missing values. This will involve checking each variable and trying to determine why values are
#'   missing, and choosing an imputation method that hopefully won't have too big of an effect on the results
#'   - Remove variables that can't directly be used in the modelling, e.g. dates
#'   - Filter out days on which the stores are closed- in the test set, we'll just manually set these days'
#'   predicted sales to zero
#'   - Observe the correlation between continuous features, and potentially remove or combine highly correlated features
#'   - Comb through the categorical features and modify any that are extremely sparse, and delete any that are completely
#'   unrelated to the target
#'   - Investigate each continuous feature and potentially discretize ("bin") them, if their relationship with the
#'   response is highly non-linear
#'   - Standardize all continuous features and the target to have zero mean and unit variance. Note: it's not specifically
#'   important that all the variables have exactly zero mean and unit variance; it is important that they all be on the
#'   same scale. You might hear elsewhere of, for example, scaling the features to be within the range $(0,1)$, or some
#'   other scaling procedure that people have strong opinions about. There isn't a whole lot of difference, as long as
#'   the transoformed features are on roughly the same scale. Other note: we aren't actually going to do this here; 
#'   we'll save it for the tutorials in which we actually fit models.
#'   
#' As you can see, there are many things we could do; the above list is long, as is only the beginning. In cases where
#' you have more variables, you may also have to screen through them and initially filter out any that are completely
#' unrelated to the target, to get the sheer volume down.
#' 
#' It also needs to be said that this manner of preprocessing as described here is suited to **structured** data. If you
#' have unstructured data, like text or images, and you have engineered all your features yourself, the preprocessing steps
#' might be very different, and will probably be related to the type of data you have, like for example scaling or
#' recolouring images. It also depends on the type of model you are fitting- for example if you're fitting a neural
#' network to the MNIST handwritten digits data, many of the features have exactly zero variance (the corners of every
#' image are white!) but not screening these out doesn't bother the neural network.
#' 
#' ### Initial Screening by Standard Deviation and Missing Value Proportion
#' 
#' First we screen out by standard deviation and missing value proportion
#' 
stddev_threshold <- function(x) sd(x,na.rm=TRUE) < .01
onelevel_threshold <- function(x) length(unique(x)) == 1
propmissing_threshold <- function(x) mean(is.na(x)) > .5

vars_to_remove_stddev <- train_feateng %>%
  select_if(is.numeric) %>% # Only look at numeric variables
  select_if(stddev_threshold) %>%
  colnames()

# The equivalent notion for categorical variables is having only a single level
vars_to_remove_onelevel <- train_feateng %>%
  select_if(is.character) %>%
  select_if(onelevel_threshold) %>%
  colnames()

vars_to_remove_missing <- train_feateng %>%
  select_if(propmissing_threshold) %>%
  colnames()

vars_to_remove_stddev
vars_to_remove_onelevel
vars_to_remove_missing

#' These data are already pretty clean, so this step did nothing. This won't be the case for other datasets you
#' encounter, and it's good to have checked anyways.
#' 
#' ### Missing Values
#' 
#' Next let's talk about missing values. We won't get into a lengthy discussion about the inferential
#' aspects of missing value imputation. Let's check which variables have missing values to begin with:

has_missing <- function(x) any(is.na(x))

train_feateng %>%
  select_if(has_missing) %>%
  glimpse()

#' Because the data is, again, pretty clean, it turns out that the only variables that are missing are missing
#' for an interpretable reason: either the store doesn't have any competitors 
#' (`CompetitionDistance`,`CompetitionOpenSinceMonth`,`CompetitionOpenSinceYear` and derivatives) or the store doesn't
#' run `promo2`. We will deal with this in two steps:
#' 
#'   - Impute these variables to 0, or "0", as appropriate. Remember to make sure 0 isn't an actual data value before
#'   doing this.
#'   - Later, in the discretization step, we'll look at whether the lack of a competitor or promo2 is related to the
#'   response, and we can discretize or add a new feature to capture this.
#' 
# Set variables to impute
vars_to_impute <- train_feateng %>%
  select_if(has_missing) %>%
  colnames()

# Check that these variables don't actually have rows that are zero
has_zero <- function(x) {
  x <- x[!is.na(x)]
  if (is.numeric(x)) {
    any(x == 0)
  }
  else if (is.Date(x)) {
    FALSE
  }
  else {
    any(x == "0")
  }
}

train_feateng %>%
  select(one_of(vars_to_impute)) %>%
  select_if(has_zero)

# Okay, the two since_days variables have zeroes. This would probably be for the actual day
# that those stores' competitors opened- let's check:
train_feateng %>%
  select(Store,Date,comp_open_date,comp_open_since_days) %>%
  filter(comp_open_since_days == 0)

train_feateng %>%
  select(Store,Date,promo2_since_date,promo2_since_days) %>%
  filter(promo2_since_days == 0)


# Yep, looks like it. Go ahead and impute the data with zeroes

impute_with_zeroes <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- 0
  }
  else if (is.Date(x)) {
    # Don't do anything, we're deleting dates in the next step
  }
  else {
    x[is.na(x)] <- "0"
  }
  x
}

train_imputed <- train_feateng %>%
  mutate_at(vars_to_impute,impute_with_zeroes)

# Check it worked
train_imputed %>%
  select_if(has_missing)

# Just the two dates that we're going to get rid of anyways

#' ### Remove Variables
#' 
#' Here we perform the simple step of getting rid of variables we can't use directly in the model.
#' For us this means the dates. We kept them up to this point to use in checks (like in the imputation
#' step), but now we don't need them anymore. Also remove `Customers`, as it is a proxy for `Sales` and
#' isn't available in the test set (because it isn't known at the time of prediction).
#' 
vars_to_remove_cantuse <- c(
  "Customers",
  "CompetitionOpenSinceMonth",
  "CompetitionOpenSinceYear",
  "Promo2SinceWeek",
  "Promo2SinceYear",
  "comp_open_date",
  "promo2_since_date"
)

train_varsremoved <- train_imputed %>%
  select(-one_of(vars_to_remove_cantuse))


#' We're getting closer to having usable data: with the exception of `Store`, `Date` and `Sales`, all of the
#' variables in our dataframe *could* be used in a statistical model. We have to keep `Store` as it
#' is a grouping variable, and `Date` will be needed for the train-validation split.
#' 
#' ### Filter out closed days
#' 
#' Filter out rows representing sales for days that the store was closed:

# How many days are the stores closed?
train_varsremoved %>%
  group_by(Open) %>%
  summarize(numdays = n())

# Actually a lot. This might sound silly, but double check that sales are actually
# recorded as zero on days when stores are closed:
train_varsremoved %>%
  filter(Open == "0") %>%
  filter(abs(Sales) > 0)

# Nope, all good
# Are all stores closed roughly the same number of days?
train_varsremoved %>%
  filter(Open == "0") %>%
  group_by(Store) %>%
  summarize(numdays = n()) %>%
  group_by(numdays) %>%
  summarize(numtimes = n()) %>%
  arrange(desc(numtimes))

# So 263 stores are closed for 163 days in the dataset,
# 208 stores are closed for 158 days, and so on
# We could dig in to that more if we wanted
# Remove them:

train_closedremoved <- train_varsremoved %>%
  filter(Open == "1") %>%
  select(-Open) # Don't need this variable anymore as it will always equal 1

glimpse(train_closedremoved)

# Actually removed quite a few rows!

#' ### Correlation Between Features
#' 
#' Including redundant variables in a model can cause instability, as the variables
#' "fight" for which one explains the patterns in the data. In a regression context this
#' is called multicollinearity, and can result in the inversion of the design matrix being
#' numerically unstable. Such instability is observed more generally in optimization algorithms
#' involving matrices with nearly linearly dependent columns, and the resulting model fits 
#' are unstable. So, best to investigate this possibility beforehand.
#' 
#' When we have a medium-large number of variables, it is reasonable to compute all pairwise correlations
#' and display them in a sorted list. When we have a very large number of correlations, computing all
#' pairwise correlations could be infeasble, as with $p$ variables there are $p(p-1)/2$ correlations to compute,
#' which is quadratic in $p$.
#' 
#' Luckily here we have a very small number of continuous variables:
train_closedremoved %>%
  select_if(is.numeric)

#' In fact, only 3 of them. When the number of continuous variables is small, it is feasible to
#' make a heat map of the correlations. The `corrplot` package does a nice job of this, and
#' includes matrix ordering algorithms to cluster correlated variables together for easy viewing.
#' 
#' We'll also include the response variable in here, to get a rough idea of whether any are correlated with it
#' 
# Look at the actual correlation matrix
cormat <- cor(select_if(train_closedremoved,is.numeric))
round(cormat,2)

# Plot it
# 
corrplot::corrplot(cormat,
                   method = "shade",
                   type = "lower",
                   order = "AOE"
)

#' We don't see much relation between any of these variables- including the response. This isn't necessarily
#' a bad thing; correlation only measures *linear* dependence, the degree to which one variable can be expressed
#' as a linear function of the other. It doesn't mean we won't be able to model `Sales` using these variables. 
#' 
#' ### Investigate Categorical Variables
#' 
#' Now let's take a look at the distribution of each of our categorical variables, to ensure that each is suitable 
#' for modelling. We're looking for anything irregular such as sparse or mislabelled levels. We'll also take a look
#' at the joint distribution of each categorical variable and the target, to get an idea for whether any can be
#' expected to be predictive on their own.
#' 
#+ catvars, cache=TRUE
# Which are the categorical variables?
train_closedremoved %>%
  select_if(is.character)

# Put them in a vector, minus Store
categorical_variables <- train_closedremoved %>%
  select_if(is.character) %>%
  select(-Store) %>%
  colnames()

# Check out the distribution of each
cat_dist <- function(var) {
  plt <- train_closedremoved %>%
    ggplot(aes_string(x = var)) +
    theme_classic() +
    geom_bar(stat = "count",aes_string(fill = var),colour = "black") +
    labs(title = str_c("Barplot of ",var),
         subtitle = "Rossman Store Sales",
         x = var,
         y = "Count") +
    scale_y_continuous(labels = scales::comma_format()) +
    guides(fill = FALSE)
  
  return(plt)
}

# Get a list of plots of each categorical variable and pass them to cowplot::plot_grid
# This is a useful trick
categorical_variables %>%
  map(cat_dist) %>%
  cowplot::plot_grid(plotlist = .)

#' We see some sparse categories, specifically `DayOfWeek` = 7 (this is Sunday, and remember
#' we removed all days where the store is closed from the data), all the nonzero `StateHoliday`
#' levels, `StoreType` = b, and `Assortment` = b. It is not *necessarily* the case that including
#' a sparse level of a categorical variable will cause instability in the model fit, but it is 
#' certainly possible. We won't remove these levels yet, but if we find our models are having
#' convergence or stability issues, we know what to look for.
#' 
#' To get an idea of what might go wrong, look closer at the sparseness of `Assortment` = b and `StoreType` = b:
#' 

store_read %>%
  group_by(Assortment,StoreType) %>%
    summarize(count = n())

#' This is a problem. If we included both these variables unmodified, we would have one combination of
#' categories that only had a single store in it. We'll leave it for now, so we can see how and when it
#' creeps up in the modelling process.
#' 
#' What about the relationship with the target? For visualizing the relationship between a categorical
#' and a continuous variable, we can do boxplots of the continuous variable in each level of the categorical variable:
#' 
#+ catcont1, cache = TRUE, fig.height=20
#
cat_cont_dist <- function(var,df = train_closedremoved) {
  plt <- df %>%
    ggplot(aes_string(x = var,y = "Sales")) +
    theme_classic() +
    geom_boxplot(aes_string(fill = var),colour = "black") +
    labs(title = str_c("Boxplots of Sales within ",var),
         subtitle = "Rossman Store Sales",
         x = var,
         y = "Count") +
    scale_y_continuous(labels = scales::dollar_format()) +
    guides(fill = FALSE)
  
  return(plt)
}

categorical_variables %>%
  map(cat_cont_dist) %>%
  cowplot::plot_grid(plotlist = .,ncol=2)


#' Nothing surprising jumps out at us; sales are higher in December, and on days
#' where the store is running a promo. Remember that the boxplots for categories
#' which were identified as spare above might be misleading, as they are not based
#' off very many datapoints.
#' 
#' ### Investigate Continuous Variables
#' 
#' Let's now take a look at the continuous variables, both histograms of their
#' univariate distributions and scatterplots of their relationship with `Sales`:
#' 
continuous_variables <- train_closedremoved %>%
  select_if(is.numeric) %>%
  dplyr::select(-Sales) %>%
  colnames()

#+ cache=TRUE, fig.height = 20
#
cont_dist <- function(var) {
  plt <- train_closedremoved %>%
    ggplot(aes_string(x = var)) +
    theme_light() +
    geom_histogram(bins = 100,colour = "black",fill = "purple") +
    labs(title = str_c("Histogram of ",var),
         subtitle = "Rossman Store Sales",
         x = var,
         y = "Count") +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format())
  plt
}

continuous_variables %>%
  map(cont_dist) %>%
  cowplot::plot_grid(plotlist = .,ncol=1)

#' We note some things:
#' 
#'   - `CompetitionDistance` is highly skewed
#'   - `comp_open_since_days` has some bad outliers (there are stores that have been
#'   open for over $40,000$ days, or $109$ years, which isn't impossible but doesn't occur
#'   all that frequently), and is heavily spiked at zero. Recall zero means there is no competitor
#'   on record for that store
#'   - `promo2_since_days` is severely spiked at $0$, and has negative values. Either there were data
#'   recording errors (promos starting after the range of available data), or we made a mistake in the
#'   computation of this variable. In a more detailed analysis (like the kind maybe you are doing!) this
#'   should be investigated
#'   
#+ cache=TRUE, fig.height=20
#
cont_cont_dist <- function(var) {
  plt <- train_closedremoved %>%
    sample_n(50000) %>%
    ggplot(aes_string(x = var,y = "Sales")) +
    theme_light() +
    geom_point(pch=21,colour = "black",fill = "orange") +
    labs(title = str_c("Scatterplot of Sales vs ",var),
         subtitle = "Rossman Store Sales",
         x = var,
         y = "Sales") +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::dollar_format())
  return(plt)
}


continuous_variables %>%
  map(cont_cont_dist) %>%
  cowplot::plot_grid(plotlist = .,ncol=1)


#' We don't see any clear relationships. A couple anomalies arise; there is probably
#' only one store with each of those outlying values of `comp_open_since_days` (remember
#' each store shows up many times in this dataset). 
#' 
#' It is not likely that a linear relationship between sales and any of these variables
#' will be very strong. We could use a model that tries to fit complex non-linear
#' relationships and just hope it works, and try to interpret the results- or we
#' could tackle this in the preprocessing stage. You are encouraged to try the former
#' approach and see if you can improve on the results; here we will investigate 
#' discretizing these variables.
#' 
#' Discretizing means splitting the variables up into discrete ranges, for example
#' replacing `comp_open_since_days` with an indicator for `comp_open_since_days` between
#' 0 and 180, 180 and 365, 365 and 1000... and so on. This serves two purposes:
#' removes the effects of outliers, and allows simple modelling of nonlinear relationships.
#' It has the disadvantage of removing any *smooth* relationships between the response and
#' the features, so in practice it is good to try with and without this step.
#' 
#' One simple way to pick the *knots* (cut-points for the variables) is using a single-variable
#' decision tree, as implemented in the `rpart` package. This is tedious on the part of
#' the analyst, as you have to comb through all continuous variables manually and make sure
#' the knots you find make sense. Nonetheless, it's a useful technique to employ when
#' you have data that is poorly behaved (as we have here) and you're under pressure to
#' produce a clean, interpretable model.
#' 
#+ chooseknots, cache=TRUE
#
# Choose knots by recursive partitioning using rpart
# Implicitly set the number of knots by choosing the complexity parameter cp-
# read the rpart documentation
# 
get_knots <- function(var,complexity=0.01) {
  form <- as.formula(str_c("Sales ~ ",var))
  cntrl <- rpart::rpart.control(cp = complexity)
  tree <- rpart::rpart(form,train_closedremoved,control = cntrl)
  
  out <- tree$splits %>%
    as_data_frame()
  
  if (nrow(out) == 0) {
    return(c(0))
  }
  out %>%
    pull(index) %>%
    unique() %>%
    sort() %>%
    c(-Inf,.,Inf) # Add end points so cut() function doesn't return NAs
}

# Set a list of the variable names and the complexities you want to use
# You basically have to set these manually, unless you can think of a better way!
# Try to set them so you get a reasonable number of splits something between maybe
# 3 and 5, as a guideline. You don't want bins that are too big, because you're
# smoothing over any relationship with the response, but you don't want bins that
# are too small as the relationship will be too noisy and not replicate in the
# validation set
variables_and_complexities <- list(
  c("CompetitionDistance",0.003),
  c("comp_open_since_days",0.0005),
  c("promo2_since_days",0.001)
)

variable_knots <- variables_and_complexities %>%
  map(~get_knots(.x[1],complexity = .x[2])) %>%
  setNames(continuous_variables)

variable_knots

#' These chosen knots are telling us something about the relationship between
#' each feature and the response. The split points (0,25,255) for `CompetitionDistance`
#' indicate that having no competitor results in different sales than having a competitor
#' right next door (35 - 255, you read the documentation so you know these values are in metres),
#' but if the competitor is not right next door/on the same block, there isn't that much difference
#' in 500m vs 5km. You can draw similar conclusions for the other variables. This is just what
#' these data are saying for these values of the complexity parameter using this particular
#' type of decision tree; these aren't supposed to be formal statistical inferences.
#' 
#' We can compute our new discretized variables and plot their relationships with
#' the response using side-by-side boxplots as we did before:
#' 
#+ discretize, cache=TRUE
#
discretize_variables <- function() {
  # Pulls the variable_knots and train_closedremoved from the calling environment
  # and returns a new training set with the features discretized
  
  df <- train_closedremoved
  for (var in continuous_variables) {
    df <- df %>%
      mutate_at(var,function(x) {
          cut(x,breaks = variable_knots[[var]])
        }
      )
  }
  df
}

train_discretized <- discretize_variables()

glimpse(train_discretized)

# Plot them
continuous_variables %>%
  map(~cat_cont_dist(.x,df = train_discretized)) %>%
  cowplot::plot_grid(plotlist = .,ncol=2)



#' # Prediction
#' 
#' With an engineered, preprocessed dataset in hand, we can turn to prediction. In this 
#' document, we'll focus on the mechanical aspects of this: performing an appropriate
#' train-validation split, passing data to a model and returning predictions on the
#' training set, validating those predictions on the validation set, and (optionally)
#' computing predictions on the test set and submitting to kaggle via their API.
#' 
#' ## Train-Validation-Test Splitting
#' 
#' Evaluation of the results of a predictive model must be done on different data than
#' was used to fit the model. This is mainly to avoid developing a model that fits 
#' irregularities/noise in the training data that don't appear in subsequent datasets
#' on which the model is to be used- the problem of *overfitting*.
#' 
#' Standard practice is to split the data randomly by rows into a **training set**, used to
#' fit the model, and a **test set** used to evaluate final model performance. The training set
#' is often further split during the model building phase yielding a **validation set**, which
#' is used during intermediate model building (e.g. to choose hyperparameter values). This
#' training/validation split can be done many times if needed.
#' 
#' It is important not to just blindly apply standard practice in every new problem, but
#' to think about what an appropriate strategy is for the task at hand. This is a kaggle
#' competition dataset, and a test set is already provided, so we don't need to do that
#' split ourselves. We do need a validation set, though, as we don't want to submit to
#' kaggle every time we want to evaluate the performance of some intermediate model
#' that we build. Should we just split the training data, say 70/30, into training
#' and validation sets?
#' 
#' How to do the split depends on the structure of your data. Randomly splitting
#' based on rows is appropriate for the "classical" case, where rows represent
#' statistically independent observations from some population. That's not the case
#' here though- observations are grouped by store, and trended across time. The
#' train-validation split needs to address the actual prediction task you're 
#' trying to solve, which in this case is predicting sales for the **same stores**
#' as in the training set, but at **future time points**.
#' 
#' How far in the future do we have to predict? Let's look at the test set:
#' 
test_read %>%
  pull(Date) %>%
  summary()

#' We can also confirm that the stores in the test set are all in the training set:
#' 
teststores <- test_read %>% pull(Store) %>% unique()
trainstores <- train_read %>% pull(Store) %>% unique()
all(teststores %in% trainstores)

#' We have to predict daily sales for 6 weeks. This was also stated in the documentation.
#' So for our validation split, rather than randomly selecting stores (which doesn't
#' make sense to do since we aren't tasked with predicting sales for new stores) and
#' randomly selecting dates (we don't need to predict sales on random dates; we need
#' to predict daily sales for 6 weeks of consecutive days immediately following the training period),
#' we will simply make our validation set the last six weeks of training data.
#' 
#' Check the range of dates in the training data:
#' 
#+ check_date1
train_discretized %>%
  pull(Date) %>%
  summary()

#' The training data goes until July 31st 2015, so let's make the validation set
#' contain June 15th - July 31st 2015:
#' 
#+ trainvalidsplit
training_set <- train_discretized %>%
  filter(Date < ymd("20150615")) %>%
  dplyr::select(-Date)
validation_set <- train_discretized %>%
  filter(Date >= ymd("20150615")) %>%
  dplyr::select(-Date)

glimpse(training_set)
glimpse(validation_set)


#' ## Obtaining Predictions
#' 
#' Now we turn to the mechanics of passing data to a model and returning predictions.
#' We need to train a simple model on the training set, and score it on the validation
#' set. We'll use a simple linear regression on `Sales` as our model. This is 
#' obviously not an appropriate model for such grouped data; other tutorials will
#' describe more appropriate modelling techniques. Here we just want something that
#' we can pass the validation set to. The result needs to be a two-column dataframe,
#' with columns `observed` and `predicted` containing the observed and predicted
#' values from the model on the validation data.
#' 
#' Our simple model is defined as follows:
#' 
#+ simplemodel, cache=TRUE
simple_model <- lm(Sales ~ . - Store,data = training_set)
summary(simple_model)

#' We score this model on new data using the `predict` function:
#' 
#+ trainpred1
train_predictions <- predict(simple_model)
train_predictions[1:10]

#' This gives us predicted sales for each store and day in the training set.
#' 
#' This isn't quite what we want- we want a nicely structured dataframe containing
#' both the observed and predicted values. We can just construct this manually:
#' 
#+ trainpred2
train_predictions <- data_frame(
  observed = training_set %>% pull(Sales),
  predicted = predict(simple_model)
)
train_predictions

#' For the validation set, pass the newdata = validation_set argument to `predict`:
#' 
#+ validpred1
validation_predictions <- data_frame(
  observed = validation_set %>% pull(Sales),
  predicted = predict(simple_model,newdata = validation_set)
)
validation_predictions

#' We get a warning about "predictions from a rank-deficient fit" being misleading;
#' this happens because our design matrix was not of full rank in the simple model,
#' and a coefficient was not defined. 
#' 
#' Kaggle uses the Root Mean Squared Percentage Error (RMSPE) as the metric in
#' this competition. With our data structured in this way, we can implement this
#' in a function:
#' 
#+ rmpse1
get_rmspe <- function(preds) {
  preds %>%
    summarize(rmspe = sqrt( mean( ((observed - predicted)/observed)^2 ))) %>%
    pull(rmspe)
}

get_rmspe(train_predictions)
get_rmspe(validation_predictions)

#' Not great- we're off by 43% on average. We'll see how to do a bit better in
#' subsequent tutorials. Also note that there are still 54 rows with `observed` = 0
#' in the training set, which is causing a divide-by-zero error in the calculation;
#' we'll be manually setting the days where the store is closed to have `Sales` = 0
#' in the test set.
#' 
#' ## Scoring the Test Data
#' 
#' The final mechanical hurdle in our modelling pipeline is scoring the test data.
#' This should be easy after all the worok we just put in.
#' 
#' The only problem is preprocessing. In the feature engineering step, our actions
#' were a lot more prescribed, and we easily put them in a function and applied it
#' to both the training and test data. In the preprocessing step, though, we did
#' a lot of manual data analysis and decision making. Now, we need to record what we
#' did, and apply exactly the same steps to the test set. Best practice dictates that you do this as you go while preprocessing, so we're
#' being a bit sloppy here.
#' 
#' Our preprocessing consisted of:
#' 
#'   - Removing features with low standard deviation, high proportions of missing values,
#'   or only one level. No features were actually removed due to these rules.
#'   - Imputed missing variables as 0 (recall our discussion about why we did this)
#'   - Removed variables we didn't need (we don't need to do that on the test data)
#'   - Filtered out days where the store is closed. On the test set, we're going to
#'   set the sales for these days to zero, rather than remove them
#'   - Investigated correlation between numeric features, and the distribution of
#'   categorical features. No changes made
#'   - Discretized all three of our continuous features . We will have to do this on
#'   the test set, using the same cutpoints
#'
#' Luckily in this case, we only have to replicate two steps: imputation and discretization. It is
#' important to remember though that these data were really clean; in cases where they aren't, 
#' the task of applying the same preprocessing steps to both the training and test sets can
#' get more difficult, and you should probably write a function like we did in the feature
#' engineering step.
#' 
#+ testpreprocess
#
# Imputation
vars_to_impute

test_imputed <- test_feateng %>%
  mutate_at(vars_to_impute,impute_with_zeroes)

glimpse(test_imputed)

# Discretization
# Re-do the discretize function so it takes the dataframe as input (why didn't 
# I do that before? Who knows!)
discretize_variables <- function(df) {
  # Pulls the variable_knots and train_closedremoved from the calling environment
  # and returns a new training set with the features discretized
  
  for (var in continuous_variables) {
    df <- df %>%
      mutate_at(var,function(x) {
        cut(x,breaks = variable_knots[[var]])
      }
      )
  }
  df
}

test_discretized <- discretize_variables(test_imputed)
glimpse(test_discretized)

#' We can now compute the predictions in the same way as we did with the validation
#' data, and also manually set . We'll then format the results in the manner required by Kaggle:
#' 
#+ testpredict1

test_predict <- test_discretized %>%
  bind_cols(data_frame(Sales = predict(simple_model,newdata = test_discretized))) %>%
  mutate(Sales = if_else(Open == 0,0,Sales)) %>%
  mutate(Sales = if_else(is.na(Sales),0,Sales)) %>% # There are 11 NA predictions remove them
  dplyr::select(Id,Sales)
  
glimpse(test_predict)

#' This can be written to a .csv file using ``readr::write_csv`, in much the same way
#' as we used `readr::read_delim` to read in the original data:
#' 
#+ writetest1

readr::write_csv(test_predict,'/Users/alexstringer/phd/s18/leaf-ra/leaf2018/rossman-simple-model-predictions.csv')

#' You can then submit to kaggle either through their drag-and-drop interface, or, if you feel
#' like authenticating with their API, using the shell command
#' 
#' `kaggle competitions submit -c rossmann-store-sales -f rossman-simple-model-predictions.csv -m "Message"`
#' 
#' Submitting this got me a score of 0.38970, which is not great. Next, we'll see about
#' developing a better predicting model for these data.
#' 
#' As a final step, save the training, validation and test sets to disk as .RData files, so
#' we can read them in directly in the subsequent tutorial where we fit more appropriate models:
#' 
save(training_set,file = '/Users/alexstringer/phd/s18/leaf-ra/leaf2018/datasets/rossman-training-set.RData')
save(validation_set,file = '/Users/alexstringer/phd/s18/leaf-ra/leaf2018/datasets/rossman-validation-set.RData')
save(test_discretized,file = '/Users/alexstringer/phd/s18/leaf-ra/leaf2018/datasets/rossman-test-set.RData')

