### A package that produces SVG tables ready for publication. It requires no knowledge of LaTeX or ggplot2

# Features:
# A simple function that plots any given preformated table produced by R
# A set of custom functions defining simple tables
# A function that crosstabulates all factors in a data.frame with a single variable
library(soc.ca)
library(ggplot2)
library(gridExtra)
library(soc.report)
par(ask=FALSE)
data(directors)
attach(directors)
setwd("~/My Dropbox/R/soc.tab/R")
fodnote=""
titel=""

load("~/My Dropbox/R/soc.ca/Data/directors.rda")

# Først den specifikke table der skal plottes

tab <- table(ownership_cat, mba)
tab1 <- table(ownership_cat, phd)

cont <- with(directors, data.frame(turnover2004, turnover2007, result2005, rateofprofit, employees, average_salary2007))
fact <- with(directors, data.frame(phd, mba, hd, owner, decoration, member_network, member_america ))

tab.cross(mba, hd)
tab.freq(mba)
grob.tab(tab.cross(mba, hd))

tal.cross(mba, hd)
tal.freq(mba)

report.cross(fact)
report.freq(fact)


