# SMS_2022_topicmodeling
Materials for my presentation on topic modeling for the 2022 SMS Research Methods Doctoral and Junior Faculty Consortium.

First time setup in R
=====================
In order to follow the code developed for the workshop (shown in full below under Workshop example), two pieces of software need to be installed beforehand. The first is R, a free software environment for statistical computing and graphics. The second is RStudio, which is a graphical user interface that goes 'over' R, making it more user friendly. It is adamant that R is installed first, and RStudio second.

Before running the code shown below, install R on your system by going to the following page:
https://cran.r-project.org/
Here, OS-specific versions of R can be found. For example, by clicking <a href="https://cran.r-project.org/bin/windows/base/">here</a>, you can download the executable for Windows. For Mac OS X, the install file can be found <a href="https://cran.r-project.org/bin/macosx/">here</a>. Installation using the default settings should do the trick.

Then, after the installation of R is complete, navigate to the following page:
https://www.rstudio.com/
You can download the free version of RStudio on <a href="https://www.rstudio.com/products/rstudio/download/">this page</a>. Again, the default settings should do the trick.

Then, after these steps are completed, it is advisable to run the following lines of code in RStudio once to install required packages. 
```Rscript
# The "tm" package enables the text mining infrastructure that we will use for LDA.
if (!require("tm")) install.packages("tm")
# The "stm" package enables the estimation of the correlated topic model.
if (!require("stm")) install.packages("stm") 
# The "huge" package is used in making the correlations map.
if (!require("huge")) install.packages("huge") 
```

