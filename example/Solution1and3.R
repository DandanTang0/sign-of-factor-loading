#This is R code for analysis of the example.
library(lavaan)
# full data, including missingess
example_data1 <- read.csv('CDI-fulldata1.csv',header = T)
## T1
CFA_t1 <- 'F  =~ T11 + T12 + T13 + T14 + T15 + T16 + T17 + T18 +
                 T19 + T110 + T111 + T112 + T113 + T114 + T115 + T116+
                 T117 + T118 + T119 + T120 + T121 + T122 + T123 + T124 +
                 T125 + T126 + T127'
# categorical
x1 <- cfa(CFA_t1, data = example_data1, std.lv = TRUE, ordered = TRUE, group = "age")
fit1 <- fitmeasures(x1,c("chisq", "df",
                      "pvalue", "cfi", "rmsea","srmr","BIC"),output = "matrix")
fit1
## loadings are all negative
summary(x1)

# T2
CFA_t2 <- 'F  =~ T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 +
                 T29 + T210 + T211 + T212 + T213 + T214 + T215 + T216+
                 T217 + T218 + T219 + T220 + T221 + T222 + T223 + T224 +
                 T225 + T226 + T227'
# categorical 
x2 <- cfa(CFA_t2, data = example_data1, std.lv = TRUE, ordered = TRUE)
fit2 <- fitmeasures(x2,c("chisq", "df",
                      "pvalue", "cfi", "rmsea","srmr","BIC"),output = "matrix")
fit2
# loadings are positive
summary(x2)



#This is R code for reanalysis of the example.
# data without missing data
example_data1 <- read.csv('CDI-nomissing2.csv',header = T)
# For Solution 1, if the true value of the first loading is positive,  the first loading can be fixed at 1 by "auto.fix.first = TRUE"
## T1
CFA_t1 <- 'F  =~ T11 + T12 + T13 + T14 + T15 + T16 + T17 + T18 +
                 T19 + T110 + T111 + T112 + T113 + T114 + T115 + T116+
                 T117 + T118 + T119 + T120 + T121 + T122 + T123 + T124 +
                 T125 + T126 + T127'
# categorical
x1 <- cfa(CFA_t1, data = example_data1[example_data1$age == 1, 1:27],auto.fix.first = TRUE , ordered = TRUE)
fit1 <- fitmeasures(x1,c("chisq", "df",
                         "pvalue", "cfi", "rmsea","srmr","BIC"),output = "matrix")
fit1

summary(x1)

#If the first loading is not positive, we can move a positive loading to the first place. 
# for example, if the loading on item T12 is positive, we can set the CFA model as follows:
## T1
CFA_t1 <- 'F  =~ T12 + T11 + T13 + T14 + T15 + T16 + T17 + T18 +
                 T19 + T110 + T111 + T112 + T113 + T114 + T115 + T116+
                 T117 + T118 + T119 + T120 + T121 + T122 + T123 + T124 +
                 T125 + T126 + T127'
# categorical
x1 <- cfa(CFA_t1, data = example_data1[example_data1$age == 1, 1:27],auto.fix.first = TRUE , ordered = TRUE)
fit1 <- fitmeasures(x1,c("chisq", "df",
                         "pvalue", "cfi", "rmsea","srmr","BIC"),output = "matrix")
fit1

summary(x1)

#For Solution 3, the factor variance can be fixed by "std.lv = TRUE" , 
#and the starting values of the loadings can be set at 1 by "start(1)*".

## T1
CFA_t1 <- 'F  =~ start(1)*T11 + start(1)*T12 + start(1)*T13 + start(1)*T14 + start(1)*T15 + start(1)*T16 + start(1)*T17 + start(1)*T18 
            + start(1)*T19 + start(1)*T110 + start(1)*T111 + start(1)*T112 + start(1)*T113 + start(1)*T114 + start(1)*T115 + start(1)*T116+
         start(1)*T117 + start(1)*T118 + start(1)*T119 + start(1)*T120 + start(1)*T121 + start(1)*T122 + start(1)*T123 + start(1)*T124 +
                 start(1)*T125 + start(1)*T126 + start(1)*T127'
# categorical
x1 <- cfa(CFA_t1, data = example_data1[example_data1$age == 1, 1:27], std.lv = TRUE, ordered = TRUE)
fit1 <- fitmeasures(x1,c("chisq", "df",
                         "pvalue", "cfi", "rmsea","srmr","BIC"),output = "matrix")
fit1
## loadings are all negative, but mplus give positive outputs
summary(x1)



















