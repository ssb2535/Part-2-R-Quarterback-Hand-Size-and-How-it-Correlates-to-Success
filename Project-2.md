Quarterback Success Based on Hand Size, Height, and Thrown Yards
================
Sarabnidhan Bains
4/19/2021

## Introduction

For this project, the hand size and height of a quarterback will be
evaluated to see if it affected the quarterback’s ability to succeed in
the NFL. Some of the information was gathered from the project I had
completed in SDS 328M. In addition, more data was combined with data
from a pro football statistics website. This dataset contains statistics
from the majority of all relevant quarterbacks from the past 20 years.
My final sample size is 60 quarterbacks, some of which had to be removed
due to injuries or other outlying factors that massively affected their
data. In the National Football League, a large emphasis is often placed
upon hand size and height of a quarterback when they are being evaluated
to be drafted to professional teams. The data collection was done
through a few websites that have the measurements for quarterback hand
size (inches) and height (inches) on record. The success they have
achieved in the league is being measured by yards thrown per game and
whether the quarterback has an appearance in the Pro Bowl (if the
quarterback makes an appearance in the Pro Bowl, he is usually one of
the five best quarterbacks in that year). My motivation for choosing
this topic is my passion for the sport of football. A positive
correlation between hand size/height and yards per game could be
expected. This data can be used for informational purposes or could even
be used to help gauge future incoming quarterbacks if a correlation was
found.

``` r
footballmeasurements <- read.csv("~/Desktop/footballmeasurements.csv")
footballyards <- read.csv("~/Desktop/footballyards.csv")
joinedfootball <- right_join(footballmeasurements, footballyards)
```

    ## Joining, by = "Player"

``` r
joinedfootball
```

    ##            Player   hand height yards Probowl
    ## 1         Bortles  9.375     77 226.3       0
    ## 2  Deshaun Watson  9.750     74 255.7       1
    ## 3      Josh Allen 10.125     77 184.4       1
    ## 4         Mariota  9.875     76 209.6       0
    ## 5           Wentz 10.000     77 253.4       1
    ## 6         Winston  9.375     76 274.1       1
    ## 7            Goff  9.000     76 263.3       1
    ## 8         Darnold  9.250     75 226.5       0
    ## 9      Derek Carr  9.125     75 242.5       1
    ## 10       Brissett  9.750     76 169.5       0
    ## 11   Dak Prescott 10.875     74 246.5       1
    ## 12   Paxton Lynch 10.375     79 158.4       0
    ## 13   Cody Kessler 10.250     73 130.3       0
    ## 14       Mayfield  9.875     73 251.7       0
    ## 15        Manziel  9.875     72 111.7       0
    ## 16        Siemian  9.875     75 210.7       0
    ## 17          Rosen  9.875     76 142.3       0
    ## 18          Kizer  9.875     76 171.2       0
    ## 19     Geno Smith  9.250     75 154.6       0
    ## 20         Savage  9.625     76 153.8       0
    ## 21       Trubisky  9.500     74 208.6       1
    ## 22  Lamar Jackson  9.500     74 139.6       1
    ## 23    CJ Beathard  9.375     74 206.3       0
    ## 24        Mahomes  9.250     75 303.6       1
    ## 25      Garoppolo  9.250     74 165.4       0
    ## 26    Bridgewater  9.250     74 173.9       1
    ## 27          Foles 10.625     78 205.2       1
    ## 28          Brees 10.250     72 281.5       1
    ## 29         Wilson 10.250     71 232.3       1
    ## 30   Tyrod Taylor 10.000     73 136.6       1
    ## 31       Stafford 10.000     75 275.3       1
    ## 32         Newton  9.875     77 232.3       1
    ## 33        Cousins  9.875     75 259.2       1
    ## 34         Flacco  9.625     78 234.3       0
    ## 35           Ryan  9.500     76 270.8       1
    ## 36         Dalton  9.500     74 237.5       1
    ## 37         Cutler  9.375     75 229.6       1
    ## 38          Brady  9.375     76 261.7       1
    ## 39 Roethlisberger  9.375     77 259.4       1
    ## 40         Rivers  9.250     77 260.0       1
    ## 41     Alex Smith  9.250     76 205.2       1
    ## 42    Case Keenum  9.125     73 214.4       0
    ## 43    Eli Manning  9.125     77 241.6       1
    ## 44    Josh Mccown  9.000     76 173.8       0
    ## 45        Rodgers 10.125     74 259.4       1
    ## 46         Palmer  9.875     77 254.1       1
    ## 47      Tannehill  9.000     76 231.8       1
    ## 48          Yates 10.250     76 126.5       0
    ## 49        Sanchez 10.500     74 194.4       0
    ## 50          Tebow 10.125     74 123.5       0
    ## 51           Luck 10.000     76 275.2       1
    ## 52    Fitzpatrick 10.750     74 210.8       0
    ## 53          Favre 10.375     74 237.9       1
    ## 54         Locker  9.625     74 165.6       0
    ## 55        Glennon  9.625     79 178.0       0
    ## 56     Kaepernick  9.125     76 177.8       0
    ## 57       Osweiler  9.875     79 151.4       0
    ## 58          McCoy  9.375     73 155.9       0
    ## 59       Griffin   9.500     74 177.5       1
    ## 60       Bradford  9.500     76 234.3       0

## EDA

``` r
summary(joinedfootball)
```

    ##          Player        hand            height          yards      
    ##  Alex Smith : 1   Min.   : 9.000   Min.   :71.00   Min.   :111.7  
    ##  Bortles    : 1   1st Qu.: 9.375   1st Qu.:74.00   1st Qu.:170.8  
    ##  Bradford   : 1   Median : 9.625   Median :75.00   Median :212.6  
    ##  Brady      : 1   Mean   : 9.708   Mean   :75.25   Mean   :210.0  
    ##  Brees      : 1   3rd Qu.:10.000   3rd Qu.:76.00   3rd Qu.:252.1  
    ##  Bridgewater: 1   Max.   :10.875   Max.   :79.00   Max.   :303.6  
    ##  (Other)    :54                                                   
    ##     Probowl      
    ##  Min.   :0.0000  
    ##  1st Qu.:0.0000  
    ##  Median :1.0000  
    ##  Mean   :0.5333  
    ##  3rd Qu.:1.0000  
    ##  Max.   :1.0000  
    ## 

The mean hand size of the data set is 9.708 inches. The mean height is
75.25 inches. The mean yards per game is 210 yards.The amount of Pro
Bowler quarterbacks is slightly more than non Pro Bowlers.

``` r
joinedfootball_num <- joinedfootball %>%
  select_if(is.numeric)
cor(joinedfootball_num, use = "pairwise.complete.obs")
```

    ##                hand      height       yards     Probowl
    ## hand     1.00000000 -0.11873434 -0.10643548 -0.03047149
    ## height  -0.11873434  1.00000000  0.07990915 -0.03865646
    ## yards   -0.10643548  0.07990915  1.00000000  0.59644848
    ## Probowl -0.03047149 -0.03865646  0.59644848  1.00000000

After running a correlation matrix between all numeric variables, no
real linear relationship is evident between any two numeric variables.

``` r
ggplot(data = joinedfootball) +
  geom_point(mapping = aes(x = hand, y = yards))
```

![](Project-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(data = joinedfootball) +
  geom_point(mapping = aes(x = height, y = yards))
```

![](Project-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> This
non-correlation is evident in these two scatter plots that compare the
quarterback hand size/height compared to average yards they threw per
game in their career.

## MANOVA

``` r
summary(manova(cbind(yards,Probowl)~hand,data=joinedfootball))
```

    ##           Df  Pillai approx F num Df den Df Pr(>F)
    ## hand       1 0.01302  0.37597      2     57 0.6883
    ## Residuals 58

After running a MANOVA test, since the p-value is more than the
significance level, there is not a mean difference in either the yards
per game or whether the quarterback made the Probowl for players with
different sized hands.

``` r
summary(manova(cbind(yards,Probowl)~height,data=joinedfootball))
```

    ##           Df   Pillai approx F num Df den Df Pr(>F)
    ## height     1 0.017951  0.52094      2     57 0.5968
    ## Residuals 58

After running a MANOVA test, since the p-value is more than the
significance level, there is not a mean difference in either the yards
per game or whether the quarterback made the Probowl for players with
different heights.

The assumptions for a MANOVA test include that the data set is made of
random samples and independent observations, has multivariate normality
of the numeric response variables, has homogeneity of within-groups
covariance matrices, has linear relationships among response variables
but no multicollinearity, and no extreme univariate or multivariate
outliers. The assumptions that were met include the random sample and
independent observations, multivariate normality of the numeric response
variables, and homogeneity of within-groups covariance matrices. The
assumptions that were not met include the linear relationships among
response variables but no multicollinearity (there is not a significant
linear relationship among the response variables) and no extreme
univariate or multivariate outliers (contains a few outliers).

## Randomization Test

``` r
res <- cor(joinedfootball$height, joinedfootball$yards, 
                    method = "pearson")
res
```

    ## [1] 0.07990915

``` r
ggscatter(joinedfootball, x = "height", y = "yards", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Linear Regression Model

``` r
summary(joinedfootball_num$hand)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   9.000   9.375   9.625   9.708  10.000  10.875

``` r
summary(joinedfootball_num$height)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   71.00   74.00   75.00   75.25   76.00   79.00

``` r
summary(joinedfootball_num$yards)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   111.7   170.8   212.6   210.0   252.1   303.6

``` r
jfcenter <- mutate(joinedfootball_num, 
       handcent = hand - 9.708,
       heightcent = height - 75.25,
       yardscent = yards - 210)
mymodel1 <- lm(yardscent ~ handcent + heightcent, data = jfcenter)
summary(mymodel1)
```

    ## 
    ## Call:
    ## lm(formula = yardscent ~ handcent + heightcent, data = jfcenter)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -90.458 -41.817   6.257  42.583  89.392 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -0.01825    6.25851  -0.003    0.998
    ## handcent    -10.25309   13.79790  -0.743    0.460
    ## heightcent    1.88034    3.64670   0.516    0.608
    ## 
    ## Residual standard error: 48.48 on 57 degrees of freedom
    ## Multiple R-squared:  0.01592,    Adjusted R-squared:  -0.01861 
    ## F-statistic: 0.461 on 2 and 57 DF,  p-value: 0.633

After running a linear regression model, neither hand size nor height of
a quarterback significantly predict the average yards per game.
Regardless of the significance, the coefficient for hand size
coefficient states that if your hand size increases by 1 inch, the
average yards per game decreases by 10.25 yards. The coefficient for
height coefficient states that if your height increases by 1 inch, the
average yards per game increases by 1.88 yards.

``` r
ggplot(joinedfootball, aes(x = hand, y = yards)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> This
graph visualizes the interaction between hand size and yard thrown per
game.

## Logistic Regression

``` r
joinedfootball1 <- mutate(joinedfootball, 
       handcent = hand - 9.708,
       heightcent = height - 75.25,
       yardscent = yards - 210)
mymodel2 <- glm(Probowl ~ hand + height, data = joinedfootball, family = "binomial")
summary(mymodel2)
```

    ## 
    ## Call:
    ## glm(formula = Probowl ~ hand + height, family = "binomial", data = joinedfootball)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.323  -1.225   1.074   1.116   1.240  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  5.39418   13.23571   0.408    0.684
    ## hand        -0.15611    0.57124  -0.273    0.785
    ## height      -0.04976    0.15108  -0.329    0.742
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 82.911  on 59  degrees of freedom
    ## Residual deviance: 82.746  on 57  degrees of freedom
    ## AIC: 88.746
    ## 
    ## Number of Fisher Scoring iterations: 3

This logistical regression model is designed to predict whether a
quarterback makes the Probowl based on hand size and height. Both hand
size and height significantly predict whether a quarterback will make
the Probowl. Regardless of the significance, the odds change by a factor
of 0.855 for every inch increase in hand size. The odds change by a
factor of 0.951 for every inch increase in height. This was done by
exponentiating the coefficients.

``` r
joinedfootball$prob <- predict(mymodel2, newdata = joinedfootball, type = "response")
joinedfootball$predicted <- ifelse(joinedfootball$prob > .5, 1, 0)
table(actual = joinedfootball$Probowl, prediction = joinedfootball$predicted)
```

    ##       prediction
    ## actual  0  1
    ##      0  3 25
    ##      1  2 30

This is the confusion matrix for the logistic regression model.

``` r
# sensitivity
mean(joinedfootball[joinedfootball$Probowl == 1,]$prob > .5, na.rm = TRUE)
```

    ## [1] 0.9375

``` r
# specificity
mean(joinedfootball[joinedfootball$Probowl == 0,]$prob <= .5, na.rm = TRUE)
```

    ## [1] 0.1071429

The sensitivity, or the proportion of positive results out of the number
of samples which were actually positive, was a proportion of 0.938 while
the specificity proportion is 0.107.
