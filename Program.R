library(car)
library(stargazer)
attach(Big5)


#some of the variables need to be reversed:
#   E2;E4;E6;E8;E10;N2;N4;A1;A3;A5;A7;C2;C4;C6;C8;O2;O4;O6
#If you accidentally run one of the recode lines more than once
# reimport the dataset and re-recode them.

E2 = recode(E2, '1=5; 2=4; 4=2; 5=1' )
E4 = recode(E4, '1=5; 2=4; 4=2; 5=1' )
E6 = recode(E6, '1=5; 2=4; 4=2; 5=1' )
E8 = recode(E8, '1=5; 2=4; 4=2; 5=1' )
E10 = recode(E10, '1=5; 2=4; 4=2; 5=1' )
N2 = recode(N2, '1=5; 2=4; 4=2; 5=1' )
N4 = recode(N4, '1=5; 2=4; 4=2; 5=1' )
A1 = recode(A1, '1=5; 2=4; 4=2; 5=1' )
A3 = recode(A3, '1=5; 2=4; 4=2; 5=1' )
A5 = recode(A5, '1=5; 2=4; 4=2; 5=1' )
A7 = recode(A7, '1=5; 2=4; 4=2; 5=1' )
C2 = recode(C2, '1=5; 2=4; 4=2; 5=1' )
C4 = recode(C4, '1=5; 2=4; 4=2; 5=1' )
C6 = recode(C6, '1=5; 2=4; 4=2; 5=1' )
C8 = recode(C8, '1=5; 2=4; 4=2; 5=1' )
O2 = recode(O2, '1=5; 2=4; 4=2; 5=1' )
O4 = recode(O4, '1=5; 2=4; 4=2; 5=1' )
O6 = recode(O6, '1=5; 2=4; 4=2; 5=1' )

# We are primarily concerned with Neuroticism as that pertains to
# our research question

extra <- (E1+E2+E3+E4+E5+E6+E7+E8+E9+E10)
neuro <- (N1+N2+N3+N4+N5+N6+N7+N8+N9+N10)
agree <- (A1+A2+A3+A4+A5+A6+A7+A8+A9+A10)
open <- (O1+O2+O3+O4+O5+O6+O7+O8+O9+O10)



# recoding the categorical variables to something we can use as a two state
# variable
female <- ifelse(gender==2,1,0)
othlang <- ifelse(engnat==2,1,0)

# calculating the number of observations we are excluding by doing it this
# way

sum(gender>2)   #102
sum(gender==0)  #24

sum(engnat==0)  #70


# Test regression
extrareg <- lm(extra~ female + age + othlang)
summary(extrareg)
plot(extrareg)


# First regression run to test previous literature that women have higher
# neuroticism on average
neuroreg <- lm(neuro~ female + age + othlang)
summary(neuroreg)

linearHypothesis(neuroreg, c("age"))
# age is not statistically significant and so will be excluded from future 
# regressions

# Third regression, wondered if the other personality metrics affect one
# another
neuroreg2 <- lm(neuro~female+othlang+agree+open+extra)
summary(neuroreg2)

# They do correlate with a small effect, they should not however.


othlangfem <- othlang*female
neuroreg3 <- lm(neuro~female+othlang+othlangfem+agree+open+extra)
summary(neuroreg3)

linearHypothesis(neuroreg3, c("agree+open+extra"))
linearHypothesis(neuroreg3, c("female","othlangfem"))
linearHypothesis(neuroreg3, c("female + othlangfem"))
linearHypothesis(neuroreg3, c("othlang"))

neuroreg4 <- lm(neuro~female+othlang+othlangfem)
summary(neuroreg4)
stargazer::stargazer(list(neuroreg3,neuroreg4), type="text")

# Another regression just to test curiosity.
openreg <- lm(Open~ female + othlang +Neuro)
summary(openreg)
plot(openreg)


# Possible restriciton on the data, my attempt to standardize the scores
# I worry the analysis needs to be done on the transformed Z scores of
# the results to be valid. I believe the results are still valid, but 
# it changes the interpretation of the results.
nhat <- sum(neuro)/19719
nstandev <- sqrt(((sum(neuro)^2 - ((sum(neuro)^2)/19719))/19719))