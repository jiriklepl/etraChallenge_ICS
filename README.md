# etraChallenge_ICS
Analysis stub for course Informatics and cognitive science I

1. clone git project into your own repository
2. Download data from [ETRA dataset](https://etra.acm.org/2019/challenge.html)
3. Edit Analysis.Rmd file and perform your own analysis
4. Send me knitted html file
5. After submission deadline (TBA), push the changes back into the repo, so I can merge the analysis into one file


After that I will merge the file 
# What should be in the report

* Your name and date
* Describe subset of data that you will be using
* Describe hypotheses that you have about the data. 2-3 hypotheses should be enough
* Test the hypotheses using appropriate tools
* Discuss the results

# How to manipulate the data?

Use **dplyr** package. There are tutorials available, but you can also try to fill 00_dplyr.Rmd and send it to me for verification. 
There are several verbs that can be used for data manipulation. Dplyr uses non standard evaluation, so most of the arguments do not need to be written in brackets

There are also *join functions which work similarly as in SQL. They might be useful when combining tables. 