# COX PROPORTIONAL HAZARDS

## **LOAD LIBRARIES**

```{r}
library(tidyverse)
library(ggpubr)
library(lawstat)
library(survival)
library(survminer)
library(gtsummary)
library(flextable)
```

## **ATTACH DATA**

```{r}
df <- read.csv(file.choose())
attach(df)
View(df)
```

## **DESCRIPTIVE ANALYSIS**

```{r}
table1 <- tbl_summary(df[c("Group","Age","Smoking","Duration","Outcome")], by="Group")
table1
```

## **COX PH ASSUMPTIONS**

1.  Proportional hazards assumption: Survival curves for different strata must have hazard functions that are proportional over the time t.

    ```{r}
    ph <- cox.zph(coxall)
    ph
    ```

2.  Linearity in relationship between the log hazard and the covariates: Relationship between the log hazard and each covariate is linear.

    ```{r}
    ggcoxfunctional(Surv(Duration, Outcome) ~ Age + log(Age) + sqrt(Age), data = df)
    ```

3.  Minimal/no influential observations (outliers).

    ```{r}
    ggcoxdiagnostics(coxall, type = "dfbeta", linear.predictions=F)
    ```

## **COX PH MODELS**

```{r}
coxgroup <- coxph(Surv(Duration, Outcome) ~ Group)
summary(coxgroup)

coxsmoke <- coxph(Surv(Duration, Outcome) ~ Smoking)
summary(coxsmoke)

coxage <- coxph(Surv(Duration, Outcome) ~ Age)
summary(coxage)

coxall <- coxph(Surv(Duration, Outcome) ~ Group + Smoking + Age)
summary(coxall)

table2 <- tbl_uvregression(df[c("Group","Smoking","Age","Outcome")], method=coxph, y=Surv(Duration, Outcome), exponentiate=T)
table2

table3 <- tbl_regression(coxall, exponentiate=T)
table3

table4 <- tbl_merge(tbls=list(table2, table3), tab_spanner=c("Univariate","Multivariate"))
table4
```
