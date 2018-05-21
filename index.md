# Description
This site hosts materials created by Alex Stringer in support of the LEAF project

# Materials

## Instructor Tutorials
Tutorials for instructors wishing to integrate computation into their courses.

[Worked example from Horton (2013): probability problem with empirical and analytical solution, and discussion](http://awstringer1.github.io/leaf2018/horton2013-example3-1.html)

## Examples of Student-Facing Materials
Examples of materials that can be worked on with students as part of a course.

### Lecture Supplements
Materials that can be used during lecture as a supplement to traditional slides and blackboard writing.

(STA 261): [Normal Approximation to Binomial](http://awstringer1.github.io/leaf2018/lecture-1-normal-approx-binomial.html)

(STA 261): [Fitting a Gamma distribution to rainfall data](http://awstringer1.github.io/leaf2018/sta261-lecture2-method-of-moments-gamma.html)
### Learnr Tutorials
Hosted tutorials on course concepts writtedn using the **learnr** package in R.

(STA261): [Learnr tutorial on Law of Large Numbers and simulating random variables in R](https://awstringer1.shinyapps.io/sta261-normal-approx-binomial/)

(STA303): [Learnr tutorial involving fairly complete analysis of a textbook dataset](https://awstringer1.shinyapps.io/sta303-week1-intro-data-analysis/)

# Datasets
Below is a collection of readily available datasets that instructors can use for examples, assignments, and tests. The table includes a description of the dataset, the source, and key features/suggested uses. When the dataset is from an R package, the documentation within that package will provide a qualitative description of the data; here I focussed on only the statistical qualities of the data (e.g. how many variables, data types, and so on) to make it easy to browse the list for a dataset that fits your particular needs. In cases when the dataset is not from an already-documented R package, a bit more context is provided.

| Dataset | Source | Features | Suggested Uses | Comments/Notes |
|---------|--------|----------|----------------|----------------|
| Average temperature in Ann Arbor, Michigan | data(aatemp); R package **faraway** | Two variables, temperature and year, for 115 years ranging from 1854 to 2000 | Simple linear regression | Model assumptions satisfied nicely; point estimate of slope is small but statistically significant |
| Rainfall in Illinois storms | From Rice, *Mathematical Statistics and Data Analysis*. Available freely from his webpage, or from [here](https://github.com/awstringer1/leaf2018/tree/gh-pages/datasets), datasets Illinois60.txt - Illinois64.txt. See [this course material](https://awstringer1.github.io/leaf2018/sta261-lecture2-method-of-moments-gamma.html) for commands to read the data into R. | Univariate dataset with measurements of rainfall in inches from 227 Illinois storms | Fitting basic probability models; illustrating application of statistical tests | Dataset is heavily right-skewed; it looks exponential but an exponential distribution is not flexible enough. A gamma distribution fits well. Can be used to show a likelihood ratio test of shape = 1 for a gamma distribution |
| PVC Operator Data | data(pvc); R package **faraway** | Continuous outcome and two discrete covariates, a 3x8 balanced full factorial design with 2 replicates in each group | Basic two-factor ANOVA | Model assumptions satisfied nicely; both main effects are significant but no significant interaction; good for introducing simple data analytic principles like plotting basic relationships between variables |
| Warpbreaks data | data(warpbreaks); R package **faraway** | Continuous outcome and two discrete covariates, 3x2 balanced full factorial with 9 replicates per group | Two way ANOVA | Response requires transformation to satisfy assumption of normality for ANOVA, a boxcox reveals the log-transformation does a good job of this. Interaction plots show clear presence of interaction between the two factors, which shows up as being marginally significant at the .05 level in an ANOVA on the logged response. This is good for discussing practical vs statistical significance, and some reasons why what is "obvious" from the plots might not show up as significant in the accompanying inferential procedure, and hence why both are important |
| orings data | data(orings); R package **faraway** | Binomial response (\# orings damaged) and one continuous covariate (temperature on launch day) | Binomial GLM | This is a classic dataset used to introduce binomial regression models. The group size is small which facilitates discussion about model assumptions and distributional approximations. The temperature on the day of launch when the Challenger shuttle exploded was well outside the range of observed temperatures in the dataset, which facilitates discussion about extrapolation |
 
