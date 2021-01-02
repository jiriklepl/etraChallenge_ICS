# taken from: https://stackoverflow.com/questions/10646665/how-to-convert-r-markdown-to-html-i-e-what-does-knit-html-do-in-rstudio-0-9

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('Analysis.Rmd', 'Analysis.md') # creates md file
markdownToHTML('Analysis.md', 'Analysis.html') # creates html file