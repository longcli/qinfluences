# 2018-01-12
# Analysis of potential influences on ITBQ
# Cliff Long
# Janis Vollmer
# ITBQ integrated data
# first run data_prep.R
# then load 



# try PLS and PCA
# https://www.r-bloggers.com/supervised-vs-unsupervised-learning-exploring-brexit-with-pls-and-pca/


# LOAD PACKAGES ###############################################################

library(tidyverse)



# LOAD DATA ###################################################################

# load back into environment
d_getstoreddata <- readRDS(file = 'itbq_integrated_data.rds')

dat <- d_getstoreddata

names(dat)
glimpse(dat)



# CORRELATIONS ################################################################


# tmpccf <- d_everything_z %>% 
#   filter(x_inv_accuracy > 0.8, y_mmprop > 0.99) %>% 
#   select(x_inv_accuracy, y_mmprop) 


tmpccf0 <- dat[dat$x_inv_accuracy > 0.8 | d_everything_z$y_mmprop > 0.99, c('f_fac_num','x_yearwk','x_inv_accuracy','y_mmprop')]

ufac <- sort(unique(tmpccf0$f_fac_num))

# tmpccf_001 <- na.omit(tmpccf0[tmpccf0$f_fac_num == ufac[1],])

tmpccf <- na.omit(tmpccf0[tmpccf0$f_fac_num == ufac[1],])


# about the ccf 
# http://www.michaeljgrogan.com/cross-correlation-r/

with(tmpccf, ccf(x = x_inv_accuracy, y = y_mmprop))


library(Hmisc)  # for Lag() function

tmpccf_001$lag_mmprop = Lag(tmpccf_001$y_mmprop, shift = 3)

with(tmpccf_001, pairs(x_inv_accuracy ~ lag_mmprop))
with(tmpccf_001, cor(x = x_inv_accuracy,  y = lag_mmprop, use = "pairwise.complete.obs"))


with(tmpccf_001, pairs(x_inv_accuracy ~ y_mmprop))
with(tmpccf_001, cor(x = x_inv_accuracy,  y = y_mmprop, use = "pairwise.complete.obs"))


plt1 <- ggplot(data = tmpccf_001, aes(x = x_yearwk)) + 
  geom_point(aes(y = x_inv_accuracy)) +
  geom_line(aes(y = x_inv_accuracy), group = 1) +
  theme(axis.text.x = element_blank())

plt1


plt2 <- ggplot(data = tmpccf_001, aes(x = x_yearwk)) + 
  geom_point(aes(y = y_mmprop), color = 'red') + 
  geom_line(aes(y = y_mmprop), color = 'red', group = 1) +
  theme(axis.text.x = element_blank())

plt2


plt3 <- ggplot(data = tmpccf_001, aes(x = x_yearwk)) + 
  geom_point(aes(y = lag_mmprop), color = 'blue') + 
  geom_line(aes(y = lag_mmprop), color = 'blue', group = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plt3




library("cowplot")

plot_grid(plt1, plt2, plt3, 
          align = "v",
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)



# TRANSFER FUNCTION TS MODEL ##################################################

# TRANSFER FUNCTION EXAMPLE
# based on Bowerman (3rd Edition)
# Section 14.1 p. 658
# Table 14.1 sales vs. advertising expenditures 

#  x = advertising expenditures (input)
#  y = sales (output)


library(TSA)
library(forecast)

acf.plot = function(d){
  par(mfrow=c(2,1))
  mlag = length(d)/3
  Acf(d, lag.max = mlag)
  Pacf(d, lag.max = mlag)
  par(mfrow=c(1,1))
}


# dat = read.table("clipboard", header = TRUE)

dat = tmpccf_001

head(dat); tail(dat)

# RECODE USING 'X' AND 'Y'
#   y = visc
#   x = temp

gdat = dat[, c(3,4)]

names(gdat) <- c('x', 'y')

head(gdat)


#view the data

ts.x = ts(gdat$x)
ts.y = ts(gdat$y)
ts.plot(ts.x, ts.y, col = c('red','blue'))


par(mfrow = c(2,1))
ts.plot(ts.x)
ts.plot(ts.y)
par(mfrow = c(1,1))



# PREWHITEN X and Y

# STEP: identify ARIMA model for x (temp) (prewhiten x)

tsdisplay(diff(ts.x), lag.max = length(ts.x)/3)
ndiffs(ts.x)
auto.arima(ts.x)

Arima(ts.x, order = c(2,1,0))


pw.x = Arima(ts.x, order = c(2,1,0), include.drift=FALSE); print(pw.x)  # adjust the arma as needed
# output matches Bowerman Figure 14.3 (except coef signs)

plot(residuals(pw.x))
acf.plot(residuals(pw.x))


# STEP: prewhiten y (use the same arma as pw.x)
#  HERE also use the same coef as pw.x
#  FOR THE BOWERMAN EXAMPLE, THIS MAKES A BIG DIFFERENCE !!!

# USE THIS
pw.y = arima(ts.y, order = c(2,1,0), fixed = coef(pw.x)); print(pw.y)  # Bowerman p. 663 implies fixed=coef(fit.temp)

# do NOT use this
# pw.y = arima(ts.y, order = c(1,1,1)); print(pw.y)  # Bowerman p. 663 implies fixed=coef(fit.temp)

acf.plot(residuals(pw.y))
# output is very close to Bowerman Figure 14.4 


# the 'prewhiten' function from the TSA package
# gives largely the same outcome

tsint <- ts.intersect(ts.x, ts.y)


# MIGHT BE NECESSARY TO UNLOAD DPLYR TO RUN TSA PREWHITEN
# detach("package:broom", unload=TRUE)
# detach("package:janitor", unload=TRUE)
# detach("package:dplyr", unload=TRUE)

pw.tsa = TSA::prewhiten(ts.x, ts.y); print(pw.tsa)




# STEP: determine the lag between prewhitened x and prewhitened y using ccf
#  using ccf(x,y) should see negative lags (x leads y)
#  using ccf(y,x) should see positive lags (y follows x)
#
# From http://monogan.myweb.uga.edu/teaching/ts/ccfNotes.R
#  The 'ccf' function allows a simple way to get cross-correlations.
#  Oddly, 'x' refers to the presumed endogenous variable and 'y' refers to the presumed exogenous variable.
#  This is opposite of what is usually expected.

ccf.val = ccf(residuals(pw.y),residuals(pw.x)); abline(v=0, col = 'red')
# output matches Bowerman Figure 14.5 ccf


# STEP: apply transfer function to 'x'

# use ccf form to determine b, r, s
# lag = b = 2
# s = 1
# r = 2  # because ccf dies down in damped sine wave fashion (Bowerman)
# used in transfer=list(c(r,s)) below, r = denominator (AR) and s is numerator (MA)

# CREATE LAGGED VERSION OF 'X' using 'b'
# include diff if needed

# Bowerman Figure 14.9 indicates model includes diff(y) and diff(x)

lag.x <- diff(lag(ts.x, -b))

data2 <- na.omit(as.data.frame(ts.union(ts.x, lag.x, ts.y)))
head(data2); tail(data2)


# FIT TRANSFER FUNCTION MODEL 

tf.1 <- arimax(data2$ts.y, order=c(2,1,0), xtransf=data.frame(data2$lag.x), 
               transfer=list(c(2,1)), include.mean = FALSE)
print(tf.1)


acf.plot(na.omit(residuals(tf.1)))


# Professor Chan indicates that 'arimax' does not work with 'predict' or 'forecast'
#   but we can plot the fitted values from the model
# new code from Cryer/Chan book

plot(ts.y)
lines(fitted(tf.1), lty=3, col='red')




# PCA #########################################################################
# https://www.r-bloggers.com/supervised-vs-unsupervised-learning-exploring-brexit-with-pls-and-pca/
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


#dat_num <- dat %>% select_if(is.numeric)
#dat_num <- sapply(dat, is.numeric)

names(dat)

sel_cols0 <- c("f_fac_num", "x_yearwk")

# select fac demographic cols
sel_cols_f <- names(dat[,startsWith(colnames(dat), prefix = "f_")])


# select y_ vars
sel_cols_y <- names(dat[,startsWith(colnames(dat), prefix = "y_")])



# select ctn columns
sel_cols_genx <- c("x_prop_shp_ctn_totcalc", 
                   "x_prop_shp_ctn_adot", 
                   "x_prop_shp_ctn_ltl", 
                   "x_prop_shp_ctn_spc", 
                   "x_upc_shelf",
                   "x_upc_bulk", 
                   "x_inv_accuracy")


# select cat1 columns
cat1_cols_ln <- names(dat[,grepl("cat1_prop_ln", colnames(dat))])
cat1_cols_ctn <- names(dat[,grepl("cat1_prop_ctn", colnames(dat))])


# select cat2 columns
cat2_cols_ln <- names(dat[,grepl("cat2_prop_ln", colnames(dat))])
cat2_cols_ctn <- names(dat[,grepl("cat2_prop_ctn", colnames(dat))])


# select cat3 columns
cat3_cols_ln <- names(dat[,grepl("cat3_prop_ln", colnames(dat))])
cat3_cols_ctn <- names(dat[,grepl("cat3_prop_ctn", colnames(dat))])



dat_pca <- dat[dat$f_bunit == 'Business Essentials', names(dat) %in% c(sel_cols_f, 'x_yearwk', sel_cols_y, sel_cols_genx, cat2_cols_ctn)]


names(dat_pca)


# try imputing

install.packages("mice")
library(mice)

md.pattern(dat_pca)

install.packages("VIM")
library(VIM)
mice_plot <- aggr(dat_pca, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(dat_pca), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))



na_count <-sapply(dat_pca, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


dat_pca1 <- na.omit(dat_pca)

dim(dat_pca)
dim(dat_pca1)
names(dat_pca1)
glimpse(dat_pca1)

dat_pca_num <- dat_pca1[, names(dat_pca1) %in% c(sel_cols_genx, cat2_cols_ctn)]



# Plot correlation matrix

dat.matrix <- corrplot::corrplot(cor(dat_pca_num, use = "na.or.complete"), 
                                 order = "hclust", 
                                 tl.cex = 0.6, 
                                 method = "square", type = "full", 
                                 tl.pos = "dt", addrect = 3, 
                                 tl.col = "black", tl.srt = 90)




# PCA 

library(factoextra)


# use initially
pcainit = prcomp(dat_pca_num, scale. = TRUE)


str(pcainit)

head(pcainit$rotation)

fviz_screeplot(pcainit)
fviz_screeplot(pcainit, geom = 'line')


get_eig(pcainit)

## graph of individual values - too busy
# fviz_pca_ind(pca1,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE)     # Avoid text overlapping

# graph of variables
fviz_pca_var(pcainit,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping


# Eigenvalues
eig.val <- get_eigenvalue(pcainit)
eig.val

# Results for Variables
res.var <- get_pca_var(pcainit)
res.var$coord          # Coordinates
res.var$contrib[,1:6]  # Contributions to the PCs
res.var$cos2           # Quality of representation 

# # Results for individuals
# res.ind <- get_pca_ind(pca1)
# res.ind$coord          # Coordinates
# res.ind$contrib        # Contributions to the PCs
# res.ind$cos2           # Quality of representation 


# add categorical variables back in

names(dat_pca1)

with(dat_pca1, table(f_bunit, f_region))

dgroups <- as.factor(dat_pca1$f_region)

levels(dgroups)
length(dgroups)
dim(dat_pca1)
dim(dat_pca_num)
dim(pca1)
str(pcafit)


# Use after see eigenvalues
pcapost = prcomp(dat_pca_num, scale. = TRUE, rank = 10)


pcafit <- pcainit
pcafit <- pcapost


fviz_screeplot(pcafit, addlabels = TRUE)


fviz_pca_ind(pcafit,
             col.ind = dgroups, # color by groups
             palette = c("#CC0000", "#006600", "#669999", "#00CCCC", "#660099"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE)


fviz_pca_var(pcafit,
             col.var = 'cos2', # color by variables
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE)


fviz_pca_biplot(pcafit,
                col.var = 'steelblue', # color by variables
                #palette = c("#CC0000", "#006600", "#669999", "#00CCCC", "#660099"),
                ellipse.type = "confidence",
                legend.title = "Groups",
                label = 'none',
                repel = TRUE)


# PLS =========================================================================

library(caret)
library(pls)

help(package = 'pls')

#Partial Least Squares
gasTrain <- dat_pca1[1:50,]
gasTest <- gasoline[51:60,]


# PREP DATA FOR REVIEW ########################################################
# REWORK AFTER ADD UPC SCAN
# get numeric columns only ----------
# https://github.com/tidyverse/dplyr/issues/497

# method 1 
d_splom1 <- dat %>% purrr::keep(is.numeric)

# method 2
d_splom2 <- dat %>% select_if(is.numeric)


names(d_splom1) %in% names(d_splom2)

# pick one
d_splomx <- d_splom1
d_splomx <- d_splom2


d_splom <- d_splomx %>% 
  select(-c(x_iso_year, x_iso_week)) %>% 
  filter(y_itbprop > 0.8, y_whseprop > 0.1, y_dmgprop > 0.1, y_mmprop > 0.1)



# scatterplot matrix
pairs(d_splom)
cor(d_splom)




###############################################################################
# EXPLORE RELATIONSHIPS 
###############################################################################

names(d_everything2)


# QUESTION --------------------------------------------------------------------
# is there a relationship between MMO and UPC scan
# UPC shelf is less relevant for Facilities BU so focus on Office
# remove PAZ

d_office <- d_everything2 %>% dplyr::filter(bunit == 'Business Essentials', facility_abbreviation != 'PAZ')

names(d_office)

with(d_office, table(pinnacle_group))

d_office <- d_office %>% 
  mutate(log_mmprop = log(mmprop),
         log_whseprop = log(whseprop),
         log_dmgprop = log(dmgprop))

splomvars <- c('mmprop','whseprop', 'log_mmprop', 'log_whseprop', 'upc_shelf','upc_bulk')


pairs(d_office[, splomvars])

library(GGally)
ggpairs(d_office[, splomvars])


fit1 = lm(mmprop ~ factor(region) + factor(fac_num) + fillable_lines + upc_bulk + upc_shelf, data = d_office)

fit2 = lm(whseprop ~ factor(region) + factor(fac_num) + upc_bulk + upc_shelf, data = d_office)


fitx = fit1

fitx = fit2

summary(fitx)

anova(fitx)

d_office$rstudent <- rstudent(fitx)


plot(x = fitted(fitx), y = rstudent(fitx))

# plot residuals vs explanatory variables

with(d_office, plot(x = fac_num, y = rstudent, ylim = c(-5,5)))
grid()
abline(h = c(-3,3), col = 'red', lty = 2)


# plotting with ggplot2

xvarplot <- 'pinnacle_group'
xvarplot <- 'fac_num'
xvarplot <- 'region'
xvarplot <- 'iso_week'
#xvarplot <- 'yearwk'

# aes_string(x = xvarplot, y = 'rstudent')


res_vs_x <- ggplot(d_office, aes_string(x = xvarplot, y = 'rstudent')) +
  geom_point(aes(color = region)) +
  geom_smooth() +
  geom_hline(yintercept = c(-3,3), linetype = 2, color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Seasonal Warehouse Error Patterns', subtitle = 'Residuals by Facility (by week)')

res_vs_x + facet_wrap(~ fac_num)



res_vs_x + facet_grid(fac_num ~ region, scales = 'free')



with(d_office, table(region))




###############################################################################
###############################################################################
# MODEL DIAGNOSTICS
###############################################################################
###############################################################################

# load packages ---------------------------------------------------------------

library(car)
library(lmtest)
library(vrtest)



# set model for review --------------------------------------------------------

fit = fit1

dat <- d_office


# model results ---------------------------------------------------------------

print(summary(fit))




# plot fitted vs residuals ----------------------------------------------------

fitted.y = fit$fitted


# plot residuals vs fitted
plot(fitted.y, residuals(fit))
title("Residuals vs Fitted Values")
grid()




# normality diagnostics -------------------------------------------------------

# test normality using Shapiro-Wilks test 
res.shapiro = shapiro.test(residuals(fit))

print(res.shapiro)

if (res.shapiro$p.value < 0.05){
  print("Nonnormally distributed residuals")} else {
    print("Normally distributed residuals")
  }



# normality plot of residuals -------------------------------------------------

qqPlot(residuals(fit), main = "Normal Plot of Residuals")




# family of influence measures ------------------------------------------------

infl.fit = influence.measures(fit)

print(summary(infl.fit))

id.infl = which(apply(infl.fit$is.inf, 1, any))

print("Most influential observations:")
print(id.infl)




# Cook's Distance -------------------------------------------------------------

# find unusual Cooks Distance

fit.cook = cooks.distance(fit)

print("Influential Cooks D")
print(which(fit.cook > 3*mean(fit.cook)))




# plot Cook's diagnostics -----------------------------------------------------

fit.cook = cooks.distance(fit)

id.c = which(fit.cook > 3*mean(fit.cook))

# plot Cooks Distance
plot(fit.cook)
abline(h = c(1,3)*mean(fit.cook), col = 2)
title("Cook's Distance")
grid()

if (length(id.c) > 0){ text(id.c, fit.cook[id.c], rownames(dat)[id.c], pos = 2, xpd = TRUE) }



# print studentized residuals diagnostics -------------------------------------

# STUDENTIZED RESIDUALS
fit.studres = rstudent(fit)

print("Noteworthy studentized residuals")
print(which(abs(fit.studres) > 3))




# plot studentized residuals diagnostics -------------------------------------

fit.studres = rstudent(fit)

id.sr = which(abs(fit.studres) > 3)

# plot studentized residuals
plot(rstudent(fit), ylim = c(-4,4))
abline(h = c(-3,+3), col = 'red', lty = 3)
title('Studentized Residuals')
grid()


if (length(id.sr) > 0){ text(id.sr, fit.studres[id.sr], rownames(dat)[id.sr], pos = 2, xpd = TRUE) }



# print leverage diagnostics --------------------------------------------------

# LEVERAGE based on HAT MATRIX
fit.hat = hatvalues(fit)

print("Noteworthy leverage values")
print(which(fit.hat > 3*mean(fit.hat)))



# plot leverage diagnostics ---------------------------------------------------

fit.hat = hatvalues(fit)

id.h = which(fit.hat > 3*mean(fit.hat))


# plot leverage
plot(fit.hat)
abline(h = c(1,3)*mean(fit.hat), col = 2)
title('Leverage')
grid()


if (length(id.h) > 0){ text(id.h, fit.hat[id.h], rownames(dat)[id.h], pos = 2, xpd = TRUE) }



# print functional form diagnostics -------------------------------------------

# test for functional form
# conditional mean of residuals equal to zero
# using the RESET test

res.fform = resettest(fit)

if (res.fform$p.value < 0.05){
  print("Functional Form Misspecified [E(e|X) <> 0]")} else {
    print("Functional Form Adequate [E(e|X) = 0]")
  }



# print constant var diagnostics ----------------------------------------------

# test for heteroskedasticity
# using the Breusch-Pagan test

res.bp = bptest(fit)
res.bp$p.value

if (res.bp$p.value < 0.05){
  print("Residuals have NON-constant variance")} else {
    print("Residuals have constant variance")
  }


# print autocorrelation diagnostics -------------------------------------------

# test for autocorrelation
# the Box-Ljung test only tests for specific individual lags

res.box = Box.test(residuals(fit), lag = 1, type = "Ljung-Box")

print("Box-Ljung Test for autocorrelation")

if (res.box$p.value < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }



# the Auto.Q function uses a portmanteau test with multi-lags (10 by default)

portm.multilag = Auto.Q(y = residuals(fit))


print("Portmanteau Test for autocorrelation")

if (portm.multilag$Pvalue < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }


# END CODE ####################################################################
