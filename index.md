# Description
This site hosts materials created by Alex Stringer in support of the LEAF project.

Github repository: [https://github.com/awstringer1/leaf2018](https://github.com/awstringer1/leaf2018)

# Materials

## Instructor Tutorials
Tutorials for instructors wishing to integrate computation into their courses.

Worked example from Horton (2013): probability problem with empirical and analytical solution, and discussion

  - [Introduction: description of documents and learning objectives](http://awstringer1.github.io/leaf2018/horton2013-introduction.html)
  - [Description of the example studied and analytical solution](http://awstringer1.github.io/leaf2018/horton2013-example.html)
  - [Empirical investigation and solution](http://awstringer1.github.io/leaf2018/horton2013-empirical.html)
  - [Comparison of anlaytical and empirical results](http://awstringer1.github.io/leaf2018/horton2013-compare.html)
  - [Discussion of potential student challenges in implementing the simulations](http://awstringer1.github.io/leaf2018/horton2013-challenges.html)
  - [Recommendations on implementing similar examples in statistics courses](http://awstringer1.github.io/leaf2018/horton2013-implementation.html)
  - [Example assessments](http://awstringer1.github.io/leaf2018/horton2013-.html)

## Examples of Student-Facing Materials
Examples of materials that can be worked on with students as part of a course. Each item has an example of a course in which it could be used, with **bold** indicating a course in which it *has* been used. Also included are example Intended Learning Outcomes that the material might relate to. These are aligned with the Statistics Undergraduate Program Learning Outcomes.

### Student Tutorials
Materials that are designed to walk students through a statistical concept and the associated `R` skills for implementing it. This is distriguished from **Lecture Supplements** below by the detail in which the `R` code is covered; the **Student Tutorials** are designed to teach `R`, while the **Lecture Supplements** are mostly examples of using `R`.

[Resampling Methods](http://awstringer1.github.io/leaf2018/resampling-methods.html)

  - `R` concepts covered: simulation, loops, `ggplot`
  - Statistical concepts covered: parametric and non-parametric resampling methods
  - Necessary background: sampling distributions fo estimators, basic probability including expectation and variance
  - Description: a walkthrough of resampling methods, beginning with an artificial example of investigating the sampling distribution of the sample maximum for a) a uniform distribution and b) an unknown distribution, and concluding with an application of resampling for estimating the distribution of daily subway ridership from a real TTC dataset containing only a single day of station-level ridership counts
  - Intended Learning Outcomes:
    - Create intermediate level plots in `ggplot` to compare empirical and theoretical distributions (**Methods**)
    - Simulate to verify theoretical calculations, and understand the concept of a sampling distribution/repeated sampling (**Computational Thinking**)
    - Source, read in, and analyze a real dataset from Open Data Toronto; ask appropriate questions, and think critically about how data is reported online and in the media (**Real-World Problems**)

### Lecture Supplements
Materials that can be used during lecture as a supplement to traditional slides and blackboard writing.

[Normal Approximation to Binomial](http://awstringer1.github.io/leaf2018/lecture-1-normal-approx-binomial.html)
    
  - Example courses: **STA261**, STA255, STA220
  - Description: simulate from the binomial distribution and plot the results; compare normal density curve; compute approximate binomial probabilities using the normal distribution
  - Intended Learning Outcomes:
    - Create simulations in `R` to investigate theoretical results (**Computational Thinking**)
    - Plot univariate data using `ggplot` (**Methods**); interpret plots in the context of a problem and decide on further analysis (**Real-World Problems**)

[Central Limit Theorem: application in analyzing roundoff error](http://awstringer1.github.io/leaf2018/lecture-1-clt-roundoff-error.html)

  - Example courses: **STA261**, STA255, STA220
  - Description: comparing two strategies for rounding the sum of a bunch of numbers: rounding once at the end, vs rounding each number independently before summing. Applying the CLT to the summed roundoff errors; we find the two strategies have the same expected error (of zero), however we analyze graphically and theoretically how much worse the latter strategy is in terms of what errors *might* be realized on any given run.
  - Intended Learning Outcomes:
    - Create simulations in `R` to investigate applied problems (**Computational Thinking**)
    - Intermediate plotting of multiple univariate datasets in `ggplot` (**Methods**)

[Fitting a Gamma distribution to rainfall data via Method of Moments](http://awstringer1.github.io/leaf2018/sta261-lecture2-method-of-moments-gamma.html)

  - Example Courses: **STA261**, STA255
  - Description: illustration of the application of the Method of Moments to data; plot a histogram of rainfall data, guess the family of distributions based on shape; estimate their parameters using the Method of Moments and plot the resulting curve; qualitatively evaluate the fit 
  - Intended Learning Outcomes:
    - Read several datasets into `R`, merge them, and evaluate the integrity of the resulting data (**Computational Thinking**)
    - Plot univariate data using `ggplot` (**Methods**); interpret plots in the context of a problem and decide on further analysis (**Real-World Problems**)
    - Choose a family of distributions based on a plot and estimate the parameters of this distribution (**Methods**); critique the quality of the fitted model qualitatively through intepreting visualizations (**Real-World Problems**)

[Simulating Likelihood Functions](http://awstringer1.github.io/leaf2018/sta261-lecture4-simulating-likelihood.html)

  - Example courses: **STA261**, STA255, STA355
  - Description: plot likelihood function for a single sample; translate the mathematical definition of the likelihood function into a simulation; run this simulation and plot the resulting empirical likelihood function, comparing shape
  - Intended Learning Outcomes:
    - Plot likelihood functions for single random samples (**Methods**)
    - Simulate many datasets from distributions with particular parameter values, and link this to the concept of likelihood by plotting the results (**Methods**)
    - Implement simulations of observed data for various parameter values in a parametric statistical model; plot the resulting frequency curve and compare it to the theoretical likelihood function (**Computational Thinking**)
    
[Maximum Likelihood](http://awstringer1.github.io/leaf2018/sta261-lecture4-maximum-likelihood.html)

  - Example courses: **STA261**, STA255, STA355
  - Description: two examples. One fitting a normal distribution to beeswax melting point data; students fit multiple curves for different values of $\mu$ and $\sigma^{2}$, and compare how reasonable the curves look (in the context of the observed data) with where the corresponding values of the parameters lie on the log-likelihood curves. The other fits a Gamma distribution to the rainfall data previously fit using Method of Moments; this requires a very simple numerical optimization since the Gamma MLE does not have a closed-form solution
  - Intended Learning Outcomes:
    - Be able to apply maximum likelihood estimation to random samples from a known density with one or two unknown parameters (**Methods**)
    - Understand the definition of the likelihood function as the probability that the chosen model and parameters assign to the observed data (**Theory**)
    - Apply maximum likelihood estimation in cases where no closed-form solution exists and numerical optimization must be used (**Methods**)
    - Fit frequency curves to data using the method of maximum likelihood (**Real-World Problems**)
    
[Sampling Distributions of Estimators](http://awstringer1.github.io/leaf2018/sta261-lecture5-sampling-distributions)

  - Example courses: **STA261**, STA255, STA220
  - Description: introduction to the concept of a sampling distribution of an estimator. First do a simulated normal distribution example, illustrating the concept of repeated sampling and comparing the theoretical sampling distribution of the mean with the empirical. Then a real data example: using data on aggregated subway station-level ridership from the TTC, apply the central limit theorem to total ridership summed across stations to evaluate a (fictional) claim that the TTC has 3,000,000 riders on an average weekday. Includes brief discussion of the assumptions required to do this.
  - Intended Learning Outcomes:
    - Simulate repeated sampling to understand this concept, and compare theoretical and empirical results (**Computational Thinking**)
    - Source data from Open Data Toronto and use it, combined with appropriate methodology, to answer an actual question (**Real-World Problems**)
    - Think critically about whether a particular statistical methodology (CLT) is appropriate for answering a particular question on a particular dataset (**Theory**,**Real-World Problems**)
    
### Learnr Tutorials
Hosted tutorials on course concepts writtedn using the **learnr** package in R.

(STA261): [Learnr tutorial on Law of Large Numbers and simulating random variables in R](http://underdog.utstat.toronto.edu:3838/stringer/law-of-large-numbers-simulation)

(STA303): [Learnr tutorial involving fairly complete analysis of a textbook dataset](http://underdog.utstat.toronto.edu:3838/stringer/intro-data-anlaysis-faraway)

# Datasets
Below is a collection of readily available datasets that instructors can use for examples, assignments, and tests. The table includes a description of the dataset, the source, and key features/suggested uses. When the dataset is from an R package, the documentation within that package will provide a qualitative description of the data; here I focussed on only the statistical qualities of the data (e.g. how many variables, data types, and so on) to make it easy to browse the list for a dataset that fits your particular needs. In cases when the dataset is not from an already-documented R package, a bit more context is provided.

| Dataset | Source | Features | Suggested Uses | Comments/Notes |
|---------|--------|----------|----------------|----------------|
| Rossman Store Sale Data | [Kaggle](https://www.kaggle.com/c/rossmann-store-sales/data); stored also [on github](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets/rossman) | Medium-complexity predictive analytics problem. Repository contains two datasets with a many-to-one mapping between them, good for basic merging concepts | Predictive modelling | |
| TTC Subway Ridership Data | Obtained from [Open Data Toronto](https://www.toronto.ca/city-government/data-research-maps/open-data/open-data-catalogue/#75d6b4a2-7f29-b0df-f1eb-cc5bc7f53b68), stored [on github](https://raw.githubusercontent.com/awstringer1/sta261s18supplementary/master/datasets/ttc-subway-2015.csv); see [this tutorial](http://awstringer1.github.io/leaf2018/sta261-lecture5-sampling-distributions) for an example of reading the data into `R` | Contains inflow and outflow rider counts for one "typical" weekday, for each of the TTC's subway stations | Basic summary statistics and plots | There are 74 rows in the data and only 69 stations; the transfer stations Bloor/Yonge, Sheppard/Yonge, Kennedy, St. George and Spadina are each counted as two stations |
| Average temperature in Ann Arbor, Michigan | data(aatemp); R package **faraway** | Two variables, temperature and year, for 115 years ranging from 1854 to 2000 | Simple linear regression | Model assumptions satisfied nicely; point estimate of slope is small but statistically significant |
| Rainfall in Illinois storms | From Rice, *Mathematical Statistics and Data Analysis*. Available freely from his webpage, or from [here](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets), datasets Illinois60.txt - Illinois64.txt. See [this course material](https://awstringer1.github.io/leaf2018/sta261-lecture2-method-of-moments-gamma.html) for commands to read the data into R. | Univariate dataset with measurements of rainfall in inches from 227 Illinois storms | Fitting basic probability models; illustrating application of statistical tests | Dataset is heavily right-skewed; it looks exponential but an exponential distribution is not flexible enough. A gamma distribution fits well. Can be used to show a likelihood ratio test of shape = 1 for a gamma distribution |
| Beeswax melting point data | From Rice, *Mathematical Statistics and Data Analysis*. Available freely from his webpage, or from [here](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets), dataset beeswax.txt. See [this course material](https://awstringer1.github.io/leaf2018/sta261-lecture4-maximum-likelihood.html) for commands to read the data into R. | One continuous response, one continuous predictor | Univariate gaussian curve fitting; simple linear regression | Gaussian distribution fits melting point well; linear regression of melting point on hydrocarbons gives adequate fit with assumptions met |
| PVC Operator Data | data(pvc); R package **faraway** | Continuous outcome and two discrete covariates, a 3x8 balanced full factorial design with 2 replicates in each group | Basic two-factor ANOVA | Model assumptions satisfied nicely; both main effects are significant but no significant interaction; good for introducing simple data analytic principles like plotting basic relationships between variables |
| Warpbreaks data | data(warpbreaks); R package **faraway** | Continuous outcome and two discrete covariates, 3x2 balanced full factorial with 9 replicates per group | Two way ANOVA | Response requires transformation to satisfy assumption of normality for ANOVA, a boxcox reveals the log-transformation does a good job of this. Interaction plots show clear presence of interaction between the two factors, which shows up as being marginally significant at the .05 level in an ANOVA on the logged response. This is good for discussing practical vs statistical significance, and some reasons why what is "obvious" from the plots might not show up as significant in the accompanying inferential procedure, and hence why both are important |
| orings data | data(orings); R package **faraway** | Binomial response (\# orings damaged) and one continuous covariate (temperature on launch day) | Binomial GLM | This is a classic dataset used to introduce binomial regression models. The group size is small which facilitates discussion about model assumptions and distributional approximations. The temperature on the day of launch when the Challenger shuttle exploded was well outside the range of observed temperatures in the dataset, which facilitates discussion about extrapolation |
| Wisconsin Breast Cancer Data | data(wbca); R package **faraway** | Binary response, 9 continuous covariates | Logistic regression, variable selection | Good small dataset to introduce logistic regression; possible to build a simple and well-performing predictive model; some correlation among the covariates, good for teaching variable selection |
| Galapagos Islands species data | data(gala); R package **faraway** | Count response, 5 continuous covariates | Count regression | Some covariates (e.g. Area) do well with a log transformation; good for variable selection; most of the observed counts are actually large enough that a normal linear model looks reasonable, except looking closer reveals that the constant variance assumption is violated, hence a log transformation on the response or a count regression are appropriate; the mean-variance relationship imposed by a Poisson regression is too restrictive, overdispersion is present |
| Salmonella data | data(salmonella); R package **faraway** | Count response, one discrete covariate with 6 levels | Count regression, dose-response model | Simple example of a variable transformation improving model fit. The log-linear model of dose fits the data poorly; transforming dose to log(dose + 1) improves the fit |
| Smoking mortality data | data(femsmoke); R package **faraway** | Count response and three discrete covariates (smoking status, age group, whether or not they died); example of a 3-way table | Intermediate contingency table analysis | Marginalizing over age leads to the conclusion that smokers are less likely to die, because in these data there are more young smokers than old smokers. Good for discussion of both survivorship bias (why are there less old smokers? Are they dying before study inclusion?) and confounding variables- if age weren't measured, we would erroneously conclude that smoking improves health if we intepreted this study causally |
| Hair/Eye colours | data(haireye); R package **faraway** | 4 x 4 table of counts of combinations of hair and eye colours | Simple example of a contingency table that goes a bit beyond the 2 x 2 case, good for example of a test with more than 1 degree of freedom. Hair and eye colour end up showing evidence of not being independent, and the effects of some of the smaller categories (red hair, green eyes, etc) on this conclusion can be discussed |


