Rossmann Sales Analysis
================

R Markdown
----------

``` r
RossmannSalesData <- read.csv("Rossmann_data_new.csv")
head(RossmannSalesData)
```

    ##   Store StoreType Assortment CompetitionDistance CompetitionOpenSinceMonth
    ## 1   823         a          c               16210                        11
    ## 2   824         a          a               17570                        NA
    ## 3   825         a          a                 380                         5
    ## 4   826         a          c                7980                         6
    ## 5   827         a          c                 250                         1
    ## 6   828         d          c                3290                        12
    ##   CompetitionOpenSinceYear Promo2 Promo2SinceWeek Promo2SinceYear
    ## 1                     2010      0              NA              NA
    ## 2                       NA      0              NA              NA
    ## 3                     2011      1              40            2014
    ## 4                     2005      0              NA              NA
    ## 5                     2005      0              NA              NA
    ## 6                     2014      0              NA              NA
    ##     PromoInterval DayOfWeek      Date Sales Customers Open Promo
    ## 1                         1 7/13/2015  8811       745    1     1
    ## 2                         1 7/13/2015 10483      1172    1     1
    ## 3 Jan,Apr,Jul,Oct         1 7/13/2015  6211       672    1     1
    ## 4                         1 7/13/2015 10884       824    1     1
    ## 5                         1 7/13/2015 15927      1887    1     1
    ## 6                         1 7/13/2015  7615       745    1     1
    ##   StateHoliday SchoolHoliday MONTH YEAR
    ## 1            0             0     7 2015
    ## 2            0             0     7 2015
    ## 3            0             0     7 2015
    ## 4            0             1     7 2015
    ## 5            0             0     7 2015
    ## 6            0             1     7 2015

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
attach(RossmannSalesData)
ggplot(data = RossmannSalesData,aes(x=Sales)) + geom_histogram(aes(y= ..density..),binwidth = 10,colour="Red",fill="white") + geom_density(alpha="0.6",fill="white")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
#plots
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 3.4.4

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.4.3

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
hist(Customers, probability = TRUE, ylim = c(0, 0.0020))
lines(density(Customers),col = "chocolate3",lwd = 2)
abline(v = mean(Customers),col = "royalblue",lwd = 2)
abline(v = median(Customers),col = "red",lwd = 2)
legend(x = "topright",c("Density plot", "Mean", "Median"),col = c("chocolate3", "royalblue", "red"),lwd = c(2, 2, 2))
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(data = RossmannSalesData,aes(x=Sales)) + geom_histogram(aes(y= ..density..),binwidth = 10,colour="blue",fill="red") + geom_density(alpha="0.6",fill="white")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
boxplot(Sales, col.box="plum", xlab="Sales")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#Model1
m1 <- lm(Sales~ CompetitionDistance + Customers + Customers*CompetitionDistance)
dwtest(m1)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  m1
    ## DW = 1.5082, p-value < 2.2e-16
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
plot(m1)
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-7-1.png)![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-7-2.png)![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-7-3.png)![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
hist(RossmannSalesData$Sales,xlab="Sales",ylab="Frequency of Sales",border="black",col = "lightblue")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
hist(log(Sales),xlab="Sales",ylab="Frequency of Sales",border="black",col = "lightblue")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
hist(RossmannSalesData$Customers,xlab="Customers",ylab="Frequency of Customers",border="black",col = "Green")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
hist(log(Customers),xlab="Customers",ylab="Frequency of Customers",border="black",col = "Green")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#POOLED MODEL
library(plm)
```

    ## Warning: package 'plm' was built under R version 3.4.4

    ## Loading required package: Formula

    ## Warning: package 'Formula' was built under R version 3.4.4

``` r
pool <- plm(Sales~as.factor(Assortment)+as.factor(Promo)+as.factor(Promo2)+as.factor(MONTH)+as.factor(YEAR)+Promo*CompetitionDistance,index = c("Store"),data = RossmannSalesData,model = "pooling")
```

``` r
#The above model is not possible becuse it has duplicate values means for this model to work plm require id and year to be unique
summary(pool)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = Sales ~ as.factor(Assortment) + as.factor(Promo) + 
    ##     as.factor(Promo2) + as.factor(MONTH) + as.factor(YEAR) + 
    ##     Promo * CompetitionDistance, data = RossmannSalesData, model = "pooling", 
    ##     index = c("Store"))
    ## 
    ## Unbalanced Panel: n = 1112, T = 592-942, N = 842152
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -8987.82 -1794.10  -445.43  1237.13 35270.87 
    ## 
    ## Coefficients: (1 dropped because of singularities)
    ##                              Estimate  Std. Error   t-value  Pr(>|t|)    
    ## (Intercept)                5.6269e+03  1.1897e+01  472.9575 < 2.2e-16 ***
    ## as.factor(Assortment)b     2.0097e+03  3.1017e+01   64.7920 < 2.2e-16 ***
    ## as.factor(Assortment)c     7.5624e+02  6.1783e+00  122.4039 < 2.2e-16 ***
    ## as.factor(Promo)1          2.2472e+03  7.4503e+00  301.6233 < 2.2e-16 ***
    ## as.factor(Promo2)1        -8.4753e+02  6.1337e+00 -138.1772 < 2.2e-16 ***
    ## as.factor(MONTH)2          9.1976e+01  1.3659e+01    6.7338 1.654e-11 ***
    ## as.factor(MONTH)3          3.3203e+02  1.3422e+01   24.7386 < 2.2e-16 ***
    ## as.factor(MONTH)4          4.4034e+02  1.3594e+01   32.3926 < 2.2e-16 ***
    ## as.factor(MONTH)5          5.2546e+02  1.3665e+01   38.4539 < 2.2e-16 ***
    ## as.factor(MONTH)6          4.7076e+02  1.3558e+01   34.7206 < 2.2e-16 ***
    ## as.factor(MONTH)7          2.8596e+02  1.3439e+01   21.2779 < 2.2e-16 ***
    ## as.factor(MONTH)8          1.7141e+02  1.5465e+01   11.0835 < 2.2e-16 ***
    ## as.factor(MONTH)9          7.9230e+01  1.5646e+01    5.0639 4.109e-07 ***
    ## as.factor(MONTH)10         1.4947e+02  1.5561e+01    9.6056 < 2.2e-16 ***
    ## as.factor(MONTH)11         5.7869e+02  1.5736e+01   36.7746 < 2.2e-16 ***
    ## as.factor(MONTH)12         2.1834e+03  1.5827e+01  137.9561 < 2.2e-16 ***
    ## as.factor(YEAR)2014        1.5761e+02  6.9347e+00   22.7281 < 2.2e-16 ***
    ## as.factor(YEAR)2015        3.2866e+02  8.4499e+00   38.8955 < 2.2e-16 ***
    ## CompetitionDistance       -3.2678e-02  5.2484e-04  -62.2625 < 2.2e-16 ***
    ## Promo:CompetitionDistance  1.0655e-02  7.8233e-04   13.6189 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    8.1143e+12
    ## Residual Sum of Squares: 6.5154e+12
    ## R-Squared:      0.19704
    ## Adj. R-Squared: 0.19702
    ## F-statistic: 10876.4 on 19 and 842132 DF, p-value: < 2.22e-16

``` r
hist(pool$residuals,col = "orange")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
RossmannSalesData$Promo <- ifelse(Promo==1, "YES", "NO")


class(RossmannSalesData$Promo)
```

    ## [1] "character"

``` r
any(table(RossmannSalesData$Store, RossmannSalesData$YEAR)!=1)
```

    ## [1] TRUE

``` r
#FUNCTION FOR FINDING DUPLICATE IN  R
with(RossmannSalesData, levels(Store)[tapply(YEAR, Store,
                               function(x) any(table(x) > 1))])
```

    ## NULL

``` r
#Pool model
pool_customer <- plm(Customers~as.factor(Assortment)+as.factor(Promo)+as.factor(Promo2)+as.factor(MONTH)+as.factor(YEAR)+Promo*CompetitionDistance,index = "Store",data = RossmannSalesData,model = "pooling")
summary(pool_customer)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = Customers ~ as.factor(Assortment) + as.factor(Promo) + 
    ##     as.factor(Promo2) + as.factor(MONTH) + as.factor(YEAR) + 
    ##     Promo * CompetitionDistance, data = RossmannSalesData, model = "pooling", 
    ##     index = "Store")
    ## 
    ## Unbalanced Panel: n = 1112, T = 592-942, N = 842152
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -1516.956  -211.310   -65.815   124.645  6472.090 
    ## 
    ## Coefficients: (1 dropped because of singularities)
    ##                                 Estimate  Std. Error   t-value  Pr(>|t|)
    ## (Intercept)                   7.6534e+02  1.5230e+00  502.5118 < 2.2e-16
    ## as.factor(Assortment)b        1.2891e+03  3.9707e+00  324.6541 < 2.2e-16
    ## as.factor(Assortment)c        2.6310e+01  7.9091e-01   33.2660 < 2.2e-16
    ## as.factor(Promo)YES           1.5179e+02  9.5375e-01  159.1465 < 2.2e-16
    ## as.factor(Promo2)1           -1.7767e+02  7.8520e-01 -226.2748 < 2.2e-16
    ## as.factor(MONTH)2             1.3994e+01  1.7486e+00    8.0032 1.214e-15
    ## as.factor(MONTH)3             3.2019e+01  1.7182e+00   18.6358 < 2.2e-16
    ## as.factor(MONTH)4             4.9121e+01  1.7402e+00   28.2268 < 2.2e-16
    ## as.factor(MONTH)5             5.4771e+01  1.7493e+00   31.3104 < 2.2e-16
    ## as.factor(MONTH)6             3.9266e+01  1.7357e+00   22.6228 < 2.2e-16
    ## as.factor(MONTH)7             2.2357e+01  1.7205e+00   12.9947 < 2.2e-16
    ## as.factor(MONTH)8             2.3433e+01  1.9798e+00   11.8360 < 2.2e-16
    ## as.factor(MONTH)9             1.9363e+01  2.0029e+00    9.6671 < 2.2e-16
    ## as.factor(MONTH)10            2.7006e+01  1.9920e+00   13.5574 < 2.2e-16
    ## as.factor(MONTH)11            4.6483e+01  2.0145e+00   23.0746 < 2.2e-16
    ## as.factor(MONTH)12            1.6198e+02  2.0261e+00   79.9447 < 2.2e-16
    ## as.factor(YEAR)2014           9.7993e+00  8.8774e-01   11.0384 < 2.2e-16
    ## as.factor(YEAR)2015           1.5292e-01  1.0817e+00    0.1414    0.8876
    ## CompetitionDistance          -8.5667e-03  6.7187e-05 -127.5056 < 2.2e-16
    ## PromoYES:CompetitionDistance -1.1226e-04  1.0015e-04   -1.1209    0.2623
    ##                                 
    ## (Intercept)                  ***
    ## as.factor(Assortment)b       ***
    ## as.factor(Assortment)c       ***
    ## as.factor(Promo)YES          ***
    ## as.factor(Promo2)1           ***
    ## as.factor(MONTH)2            ***
    ## as.factor(MONTH)3            ***
    ## as.factor(MONTH)4            ***
    ## as.factor(MONTH)5            ***
    ## as.factor(MONTH)6            ***
    ## as.factor(MONTH)7            ***
    ## as.factor(MONTH)8            ***
    ## as.factor(MONTH)9            ***
    ## as.factor(MONTH)10           ***
    ## as.factor(MONTH)11           ***
    ## as.factor(MONTH)12           ***
    ## as.factor(YEAR)2014          ***
    ## as.factor(YEAR)2015             
    ## CompetitionDistance          ***
    ## PromoYES:CompetitionDistance    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    1.3566e+11
    ## Residual Sum of Squares: 1.0677e+11
    ## R-Squared:      0.2129
    ## Adj. R-Squared: 0.21288
    ## F-statistic: 11988.9 on 19 and 842132 DF, p-value: < 2.22e-16

``` r
hist(pool_customer$residuals,col = "light green")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
#Fixed models
w <- plm(Sales~as.factor(Promo)+as.factor(MONTH)+Promo*CompetitionDistance,index = "Store",data = RossmannSalesData,model = "within")
summary(w)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = Sales ~ as.factor(Promo) + as.factor(MONTH) + Promo * 
    ##     CompetitionDistance, data = RossmannSalesData, model = "within", 
    ##     index = "Store")
    ## 
    ## Unbalanced Panel: n = 1112, T = 592-942, N = 842152
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -16407.41   -893.90   -132.83    704.86  31708.23 
    ## 
    ## Coefficients: (1 dropped because of singularities)
    ##                                Estimate Std. Error  t-value Pr(>|t|)    
    ## as.factor(Promo)YES          2.2666e+03 4.0541e+00 559.0857   <2e-16 ***
    ## as.factor(MONTH)2            9.5833e+01 7.4315e+00  12.8955   <2e-16 ***
    ## as.factor(MONTH)3            3.3611e+02 7.3023e+00  46.0282   <2e-16 ***
    ## as.factor(MONTH)4            4.3657e+02 7.3962e+00  59.0254   <2e-16 ***
    ## as.factor(MONTH)5            5.2179e+02 7.4345e+00  70.1848   <2e-16 ***
    ## as.factor(MONTH)6            4.7295e+02 7.3767e+00  64.1136   <2e-16 ***
    ## as.factor(MONTH)7            2.9843e+02 7.3141e+00  40.8019   <2e-16 ***
    ## as.factor(MONTH)8            9.2742e+01 8.3019e+00  11.1712   <2e-16 ***
    ## as.factor(MONTH)9            2.7470e+00 8.4031e+00   0.3269   0.7437    
    ## as.factor(MONTH)10           7.0979e+01 8.3550e+00   8.4955   <2e-16 ***
    ## as.factor(MONTH)11           4.9741e+02 8.4503e+00  58.8631   <2e-16 ***
    ## as.factor(MONTH)12           2.1046e+03 8.5029e+00 247.5168   <2e-16 ***
    ## PromoYES:CompetitionDistance 1.0026e-02 4.2601e-04  23.5337   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    3.2296e+12
    ## Residual Sum of Squares: 1.9261e+12
    ## R-Squared:      0.40361
    ## Adj. R-Squared: 0.40282
    ## F-statistic: 43782.7 on 13 and 841027 DF, p-value: < 2.22e-16

``` r
hist(w$residuals,col = "light green")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
plmModel <-  plm(Customers~as.factor(Promo)+as.factor(YEAR)+as.factor(SchoolHoliday),index = "Store",data = RossmannSalesData,model = "within")
summary(plmModel)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = Customers ~ as.factor(Promo) + as.factor(YEAR) + 
    ##     as.factor(SchoolHoliday), data = RossmannSalesData, model = "within", 
    ##     index = "Store")
    ## 
    ## Unbalanced Panel: n = 1115, T = 592-942, N = 844338
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -2341.0480   -76.6715    -7.6084    66.8732  4176.1591 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t-value  Pr(>|t|)    
    ## as.factor(Promo)YES       152.91237    0.32236 474.346 < 2.2e-16 ***
    ## as.factor(YEAR)2014         4.22383    0.36751  11.493 < 2.2e-16 ***
    ## as.factor(YEAR)2015        -9.20164    0.41818 -22.004 < 2.2e-16 ***
    ## as.factor(SchoolHoliday)1  15.89563    0.40597  39.154 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    2.3192e+10
    ## Residual Sum of Squares: 1.8247e+10
    ## R-Squared:      0.21322
    ## Adj. R-Squared: 0.21217
    ## F-statistic: 57128.1 on 4 and 843219 DF, p-value: < 2.22e-16

``` r
hist(plmModel$residuals,col = "Purple")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
##Random effect models
random <-  plm(Sales~as.factor(Promo)+as.factor(YEAR),data = RossmannSalesData,index = c("Store"),model = "random")
summary(random)
```

    ## Oneway (individual) effect Random Effect Model 
    ##    (Swamy-Arora's transformation)
    ## 
    ## Call:
    ## plm(formula = Sales ~ as.factor(Promo) + as.factor(YEAR), data = RossmannSalesData, 
    ##     model = "random", index = c("Store"))
    ## 
    ## Unbalanced Panel: n = 1115, T = 592-942, N = 844338
    ## 
    ## Effects:
    ##                   var std.dev share
    ## idiosyncratic 2507579    1584 0.307
    ## individual    5672154    2382 0.693
    ## theta:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.9727  0.9762  0.9762  0.9759  0.9762  0.9783 
    ## 
    ## Residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -14367    -952    -186       0     680   31830 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)         5797.6865    71.3923  81.209 < 2.2e-16 ***
    ## as.factor(Promo)YES 2309.2197     3.4686 665.748 < 2.2e-16 ***
    ## as.factor(YEAR)2014  161.3380     3.9561  40.782 < 2.2e-16 ***
    ## as.factor(YEAR)2015  202.9991     4.4975  45.136 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    3.2395e+12
    ## Residual Sum of Squares: 2.1172e+12
    ## R-Squared:      0.34642
    ## Adj. R-Squared: 0.34642
    ## F-statistic: 149177 on 3 and 844334 DF, p-value: < 2.22e-16

``` r
hist(random$residuals,col = "lightblue")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
random_customer <- plm(Customers~as.factor(Promo2)+as.factor(YEAR)+ StateHoliday,data = RossmannSalesData,index = c("Store"),model = "random")
summary(random_customer)
```

    ## Oneway (individual) effect Random Effect Model 
    ##    (Swamy-Arora's transformation)
    ## 
    ## Call:
    ## plm(formula = Customers ~ as.factor(Promo2) + as.factor(YEAR) + 
    ##     StateHoliday, data = RossmannSalesData, model = "random", 
    ##     index = c("Store"))
    ## 
    ## Unbalanced Panel: n = 1115, T = 592-942, N = 844338
    ## 
    ## Effects:
    ##                   var std.dev share
    ## idiosyncratic 27474.8   165.8 0.219
    ## individual    98190.1   313.4 0.781
    ## theta:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.9783  0.9810  0.9811  0.9808  0.9811  0.9828 
    ## 
    ## Residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2353.9   -87.8    -8.4     0.1    77.7  4301.8 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error  t-value  Pr(>|t|)    
    ## (Intercept)          835.81944   13.44103  62.1842 < 2.2e-16 ***
    ## as.factor(Promo2)1  -160.41754   18.77992  -8.5420 < 2.2e-16 ***
    ## as.factor(YEAR)2014    5.76899    0.41414  13.9299 < 2.2e-16 ***
    ## as.factor(YEAR)2015   -5.08676    0.47070 -10.8068 < 2.2e-16 ***
    ## StateHolidaya         11.61852    6.32760   1.8362   0.06633 .  
    ## StateHolidayb         67.47732   13.80569   4.8876 1.021e-06 ***
    ## StateHolidayc       -369.75124   19.71561 -18.7542 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    2.3229e+10
    ## Residual Sum of Squares: 2.3204e+10
    ## R-Squared:      0.0010891
    ## Adj. R-Squared: 0.001082
    ## F-statistic: 153.307 on 6 and 844331 DF, p-value: < 2.22e-16

``` r
hist(random_customer$residuals,col = "yellow")
```

![](RossmannSalesAnalysis_files/figure-markdown_github/unnamed-chunk-26-1.png)
