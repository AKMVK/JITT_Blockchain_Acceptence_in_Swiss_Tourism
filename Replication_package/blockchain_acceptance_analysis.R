################################################################################
#  Blockchain Acceptance in Tourism — Reproduction & Extension Analysis
#  Paper: JITT_Blockchain_Acceptance (Information Technology & Tourism)
#
#  This script is organised BY TABLE, matching the final manuscript:
#    Main : Table 4/5 (descriptives), 6/8 (hierarchical regression),
#           7/9 (CB-SEM treatment models: blockchain dummy -> constructs -> booking)
#    Appx : Table A1/A4 (CFA), A2/A5 (Fornell-Larcker), A3/A6 (loadings),
#           A7/A8 (multigroup CB-SEM), A10 (item descriptives)
#  Tables 1-3 (dimensions, personas, demographics) are definitional/descriptive
#  and are not computed here. Each block prints a clearly labelled result.
#
#  Data : Zukunftstechnologien_bereinigt.xlsx   (N = 1244)
#  Tools: lavaan, semTools, psych, dplyr  (install once, see below)
#
#  GROUP CODING  (verified against the questionnaire routing & the original
#  R_commands file — NOTE the asymmetry, it is intentional):
#     Use Case 1 (HOTEL):  HOTEL == 1  -> WITH blockchain      (n = 656)
#                          HOTEL == 2  -> WITHOUT blockchain   (n = 588)
#     Use Case 2 (TOUR) :  TOUR  == 2  -> WITH blockchain      (n = 631)
#                          TOUR  == 1  -> WITHOUT blockchain   (n = 613)
#
#  Manipulation-check evidence for the Use Case 2 coding: the with-collectible
#  group (TOUR == 2) rates the ancillary "additional offer" significantly higher
#  (M = 3.30 vs. 3.03, t = 4.64, p < .001) and books more (M = 3.41 vs. 3.15,
#  t = 3.73, p < .001) — exactly what is expected of the group that actually saw
#  the collectible (the control condition contains no ancillary service).
#  TOUR == 2 = with-blockchain is therefore used throughout.
################################################################################

## ---- 0. Setup ---------------------------------------------------------------
# install.packages(c("readxl","dplyr","lavaan","semTools","psych"))
library(readxl)
library(dplyr)
library(lavaan)
library(semTools)
library(psych)

# --- adjust path if needed ---
data_path <- "Zukunftstechnologien_bereinigt.xlsx"
dat <- read_excel(data_path)
dat <- as.data.frame(dat)

## ---- Construct -> item map --------------------------------------------------
items_UC1 <- list(
  BookingIntention = c("B03Q01_1","B03Q01_2"),
  PriceValue       = c("B03Q02_1","B03Q02_2","B03Q02_3"),
  Attractiveness   = c("B03Q03_1","B03Q03_2"),
  FitSuitability   = c("B03Q03_3","B03Q03_4"),
  PositiveReviews  = c("B03Q04_1","B03Q04_2"),
  Credibility      = c("B03Q04_3","B03Q04_4","B03Q04_5","B03Q04_6")
)
items_UC2 <- list(
  BookingIntention = c("B03Q05_1","B03Q05_2"),
  PriceValue       = c("B03Q06_1","B03Q06_2","B03Q06_3"),
  Attractiveness   = c("B03Q07_1","B03Q07_2"),
  FitSuitability   = c("B03Q07_3","B03Q07_4"),
  AdditionalOffers = c("B03Q07_5","B03Q07_6")
)

# composite (mean) scores, used for the regression tables
for (nm in names(items_UC1)) dat[[paste0("UC1_",nm)]] <- rowMeans(dat[, items_UC1[[nm]]], na.rm = TRUE)
for (nm in names(items_UC2)) dat[[paste0("UC2_",nm)]] <- rowMeans(dat[, items_UC2[[nm]]], na.rm = TRUE)

# blockchain dummies (1 = with blockchain)
dat$BC_Hotel <- ifelse(dat$HOTEL == 1, 1L, 0L)
dat$BC_Tour  <- ifelse(dat$TOUR  == 2, 1L, 0L)

# convenience subsets
uc1 <- dat[!is.na(dat$HOTEL), ]
uc2 <- dat[!is.na(dat$TOUR),  ]


################################################################################
##                                                                            ##
##   PART A  —  USE CASE 1  (HOTEL BOOKING)                                    ##
##                                                                            ##
################################################################################

## ===========================================================================
## TABLE 4 — Descriptive statistics & Welch t-tests (Use Case 1)
## With- vs. without-blockchain group means on every construct.
## ===========================================================================
cat("\n############  TABLE 4 — Descriptives & t-tests (Use Case 1)  ############\n")
desc_ttest <- function(d, prefix, grpvar, with_level) {
  out <- lapply(names(items_UC1), function(nm) {
    v  <- d[[paste0(prefix, nm)]]
    g  <- d[[grpvar]]
    w  <- v[g == with_level]; wo <- v[g != with_level]
    tt <- t.test(w, wo)            # Welch (unequal variance) by default
    data.frame(Construct = nm,
               M_with  = round(mean(w,  na.rm=TRUE),2), SD_with  = round(sd(w,  na.rm=TRUE),2),
               M_wo    = round(mean(wo, na.rm=TRUE),2), SD_wo    = round(sd(wo, na.rm=TRUE),2),
               t = round(tt$statistic,3), p = round(tt$p.value,3))
  })
  do.call(rbind, out)
}
print(desc_ttest(uc1, "UC1_", "HOTEL", with_level = 1), row.names = FALSE)

## ===========================================================================
## TABLE A1 — Construct reliability & validity (CFA, Use Case 1)
## Mean loading, loading range, Cronbach's alpha, CR (omega), AVE.
## ===========================================================================
cat("\n############  TABLE A1 — CFA reliability & validity (Use Case 1)  ############\n")
model_UC1 <- '
  BookingIntention =~ B03Q01_1 + B03Q01_2
  PriceValue       =~ B03Q02_1 + B03Q02_2 + B03Q02_3
  Attractiveness   =~ B03Q03_1 + B03Q03_2
  FitSuitability   =~ B03Q03_3 + B03Q03_4
  PositiveReviews  =~ B03Q04_1 + B03Q04_2
  Credibility      =~ B03Q04_3 + B03Q04_4 + B03Q04_5 + B03Q04_6
'
fit_cfa_UC1 <- cfa(model_UC1, data = uc1, estimator = "ML", missing = "fiml")

reliability_table <- function(fit, cons) {
  std <- standardizedSolution(fit)
  rel <- semTools::compRelSEM(fit)              # CR (omega)
  ave <- semTools::AVE(fit)
  rows <- lapply(names(cons), function(nm) {
    l <- std$est.std[std$op == "=~" & std$lhs == nm]
    a <- psych::alpha(uc_data(fit)[, cons[[nm]]], warnings = FALSE)$total$raw_alpha
    data.frame(Construct = nm,
               MeanLoading = round(mean(l),3),
               Range = paste0(round(min(l),3),"-",round(max(l),3)),
               Alpha = round(a,3),
               CR_omega = round(rel[[nm]],3),
               AVE = round(ave[[nm]],3))
  })
  do.call(rbind, rows)
}
# helper to fetch the data frame lavaan used
uc_data <- function(fit) lavInspect(fit, "data")
print(reliability_table(fit_cfa_UC1, items_UC1), row.names = FALSE)
cat("\nGlobal fit (UC1 CFA):\n")
print(round(fitMeasures(fit_cfa_UC1, c("cfi","tli","rmsea","srmr")),3))

## ===========================================================================
## TABLE A2 — Discriminant validity: Fornell-Larcker criterion (Use Case 1)
## Off-diagonal = latent correlations; diagonal = sqrt(AVE).
## ===========================================================================
cat("\n############  TABLE A2 — Fornell-Larcker (Use Case 1)  ############\n")
fornell_larcker <- function(fit, cons) {
  ave  <- semTools::AVE(fit)
  phi  <- lavInspect(fit, "cor.lv")
  fl   <- phi
  diag(fl) <- sqrt(unlist(ave)[rownames(fl)])
  round(fl, 3)
}
print(fornell_larcker(fit_cfa_UC1, items_UC1))

## ===========================================================================
## TABLE 6 — Hierarchical OLS regression (Use Case 1)
## M1 = predictors; M2 = + blockchain dummy; M3 = + predictor*dummy interactions.
## NOTE: coefficients are UNSTANDARDISED b (raw-metric OLS on the composite scores).
## Model 1 is the baseline direct-effects specification.
## ===========================================================================
cat("\n############  TABLE 6 — Hierarchical regression (Use Case 1)  ############\n")
M1_UC1 <- lm(UC1_BookingIntention ~ UC1_PriceValue + UC1_Attractiveness +
               UC1_FitSuitability + UC1_PositiveReviews + UC1_Credibility, data = uc1)
M2_UC1 <- update(M1_UC1, . ~ . + BC_Hotel)
M3_UC1 <- update(M2_UC1, . ~ . + UC1_PriceValue:BC_Hotel + UC1_Attractiveness:BC_Hotel +
                   UC1_FitSuitability:BC_Hotel + UC1_PositiveReviews:BC_Hotel +
                   UC1_Credibility:BC_Hotel)
cat("\n--- M1 (baseline predictors) ---\n"); print(summary(M1_UC1)$coefficients)
cat("\n--- M2 (+ BC dummy) ---\n");                      print(summary(M2_UC1)$coefficients)
cat("\n--- M3 (+ interactions) ---\n");                  print(summary(M3_UC1)$coefficients)
cat(sprintf("\nR2:  M1=%.3f  M2=%.3f  M3=%.3f | adjR2: %.3f / %.3f / %.3f\n",
            summary(M1_UC1)$r.squared, summary(M2_UC1)$r.squared, summary(M3_UC1)$r.squared,
            summary(M1_UC1)$adj.r.squared, summary(M2_UC1)$adj.r.squared, summary(M3_UC1)$adj.r.squared))

## ===========================================================================
## TABLE 7 — CB-SEM treatment model (Use Case 1)
## Blockchain dummy (BC_Hotel) -> each construct (in parallel); all constructs
## -> booking intention (no direct dummy -> booking path: blockchain acts only
## through the constructs). Residual covariances among constructs are freed.
## ===========================================================================
cat("\n############  TABLE 7 — CB-SEM treatment model (Use Case 1)  ############\n")
sem_UC1 <- '
  # measurement
  BookingIntention =~ B03Q01_1 + B03Q01_2
  PriceValue       =~ B03Q02_1 + B03Q02_2 + B03Q02_3
  Attractiveness   =~ B03Q03_1 + B03Q03_2
  FitSuitability   =~ B03Q03_3 + B03Q03_4
  PositiveReviews  =~ B03Q04_1 + B03Q04_2
  Credibility      =~ B03Q04_3 + B03Q04_4 + B03Q04_5 + B03Q04_6
  # treatment effects: blockchain dummy -> each construct (in parallel)
  Credibility     ~ BC_Hotel
  PositiveReviews ~ BC_Hotel
  PriceValue      ~ BC_Hotel
  Attractiveness  ~ BC_Hotel
  FitSuitability  ~ BC_Hotel
  # constructs -> booking intention (blockchain acts only through the constructs)
  BookingIntention ~ Credibility + PositiveReviews + PriceValue + Attractiveness + FitSuitability
  # residual covariances among the (now endogenous) constructs
  Credibility ~~ PositiveReviews + PriceValue + Attractiveness + FitSuitability
  PositiveReviews ~~ PriceValue + Attractiveness + FitSuitability
  PriceValue ~~ Attractiveness + FitSuitability
  Attractiveness ~~ FitSuitability
'
fit_sem_UC1 <- sem(sem_UC1, data = uc1, estimator = "ML", missing = "fiml")
cat("\n--- POOLED (n = 1244) ---\n")
print(standardizedSolution(fit_sem_UC1)[standardizedSolution(fit_sem_UC1)$op == "~",
      c("lhs","op","rhs","est.std","se","pvalue")], row.names = FALSE)
print(round(fitMeasures(fit_sem_UC1, c("cfi","tli","rmsea","srmr")),3))
cat("\nR2 (Booking):\n"); print(round(inspect(fit_sem_UC1,"r2")["BookingIntention"],3))

## TABLE A7 — Construct -> booking by blockchain condition (Use Case 1)
cat("\n--- TABLE A7: structural paths by condition (UC1) ---\n")
sem_UC1_grp <- '
  BookingIntention =~ B03Q01_1 + B03Q01_2
  PriceValue       =~ B03Q02_1 + B03Q02_2 + B03Q02_3
  Attractiveness   =~ B03Q03_1 + B03Q03_2
  FitSuitability   =~ B03Q03_3 + B03Q03_4
  PositiveReviews  =~ B03Q04_1 + B03Q04_2
  Credibility      =~ B03Q04_3 + B03Q04_4 + B03Q04_5 + B03Q04_6
  BookingIntention ~ Credibility + PositiveReviews + PriceValue + Attractiveness + FitSuitability
'
fit_sem_UC1_mg <- sem(sem_UC1_grp, data = uc1, group = "BC_Hotel",
                      estimator = "ML", missing = "fiml")
print(standardizedSolution(fit_sem_UC1_mg)[standardizedSolution(fit_sem_UC1_mg)$op == "~",
      c("lhs","op","rhs","group","est.std","se","pvalue")], row.names = FALSE)


################################################################################
##                                                                            ##
##   PART B  —  USE CASE 2  (TOUR BOOKING)                                     ##
##                                                                            ##
################################################################################

## ===========================================================================
## TABLE 5 — Descriptive statistics & Welch t-tests (Use Case 2)
## ===========================================================================
cat("\n############  TABLE 5 — Descriptives & t-tests (Use Case 2)  ############\n")
desc_ttest2 <- function(d) {
  out <- lapply(names(items_UC2), function(nm) {
    v <- d[[paste0("UC2_", nm)]]; g <- d$TOUR
    w <- v[g == 2]; wo <- v[g == 1]            # TOUR==2 = with blockchain
    tt <- t.test(w, wo)
    data.frame(Construct = nm,
               M_with = round(mean(w, na.rm=TRUE),2), SD_with = round(sd(w, na.rm=TRUE),2),
               M_wo   = round(mean(wo,na.rm=TRUE),2), SD_wo   = round(sd(wo,na.rm=TRUE),2),
               t = round(tt$statistic,3), p = round(tt$p.value,3))
  })
  do.call(rbind, out)
}
print(desc_ttest2(uc2), row.names = FALSE)

## ===========================================================================
## TABLE A4 — Construct reliability & validity (CFA, Use Case 2)
## ===========================================================================
cat("\n############  TABLE A4 — CFA reliability & validity (Use Case 2)  ############\n")
model_UC2 <- '
  BookingIntention =~ B03Q05_1 + B03Q05_2
  PriceValue       =~ B03Q06_1 + B03Q06_2 + B03Q06_3
  Attractiveness   =~ B03Q07_1 + B03Q07_2
  FitSuitability   =~ B03Q07_3 + B03Q07_4
  AdditionalOffers =~ B03Q07_5 + B03Q07_6
'
fit_cfa_UC2 <- cfa(model_UC2, data = uc2, estimator = "ML", missing = "fiml")
reliability_table2 <- function(fit, cons) {
  std <- standardizedSolution(fit); rel <- semTools::compRelSEM(fit); ave <- semTools::AVE(fit)
  rows <- lapply(names(cons), function(nm) {
    l <- std$est.std[std$op == "=~" & std$lhs == nm]
    a <- psych::alpha(lavInspect(fit,"data")[, cons[[nm]]], warnings = FALSE)$total$raw_alpha
    data.frame(Construct = nm, MeanLoading = round(mean(l),3),
               Range = paste0(round(min(l),3),"-",round(max(l),3)),
               Alpha = round(a,3), CR_omega = round(rel[[nm]],3), AVE = round(ave[[nm]],3))
  }); do.call(rbind, rows)
}
print(reliability_table2(fit_cfa_UC2, items_UC2), row.names = FALSE)
cat("\nGlobal fit (UC2 CFA):\n")
print(round(fitMeasures(fit_cfa_UC2, c("cfi","tli","rmsea","srmr")),3))

## ===========================================================================
## TABLE A5 — Discriminant validity: Fornell-Larcker criterion (Use Case 2)
## ===========================================================================
cat("\n############  TABLE A5 — Fornell-Larcker (Use Case 2)  ############\n")
print(fornell_larcker(fit_cfa_UC2, items_UC2))

## ===========================================================================
## TABLE 8 — Hierarchical OLS regression (Use Case 2)
## ===========================================================================
cat("\n############  TABLE 8 — Hierarchical regression (Use Case 2)  ############\n")
M1_UC2 <- lm(UC2_BookingIntention ~ UC2_PriceValue + UC2_Attractiveness +
               UC2_FitSuitability + UC2_AdditionalOffers, data = uc2)
M2_UC2 <- update(M1_UC2, . ~ . + BC_Tour)
M3_UC2 <- update(M2_UC2, . ~ . + UC2_PriceValue:BC_Tour + UC2_Attractiveness:BC_Tour +
                   UC2_FitSuitability:BC_Tour + UC2_AdditionalOffers:BC_Tour)
cat("\n--- M1 (baseline predictors) ---\n"); print(summary(M1_UC2)$coefficients)
cat("\n--- M2 (+ BC dummy) ---\n");                      print(summary(M2_UC2)$coefficients)
cat("\n--- M3 (+ interactions) ---\n");                  print(summary(M3_UC2)$coefficients)
cat(sprintf("\nR2:  M1=%.3f  M2=%.3f  M3=%.3f | adjR2: %.3f / %.3f / %.3f\n",
            summary(M1_UC2)$r.squared, summary(M2_UC2)$r.squared, summary(M3_UC2)$r.squared,
            summary(M1_UC2)$adj.r.squared, summary(M2_UC2)$adj.r.squared, summary(M3_UC2)$adj.r.squared))

## ===========================================================================
## TABLE 9 — CB-SEM treatment model (Use Case 2)
## Blockchain dummy (BC_Tour) -> each construct (in parallel); all constructs
## -> booking intention (no direct dummy -> booking path). By-condition (A8) follows.
## ===========================================================================
cat("\n############  TABLE 9 — CB-SEM treatment model (Use Case 2)  ############\n")
sem_UC2 <- '
  BookingIntention =~ B03Q05_1 + B03Q05_2
  PriceValue       =~ B03Q06_1 + B03Q06_2 + B03Q06_3
  Attractiveness   =~ B03Q07_1 + B03Q07_2
  FitSuitability   =~ B03Q07_3 + B03Q07_4
  AdditionalOffers =~ B03Q07_5 + B03Q07_6
  # treatment effects: blockchain dummy -> each construct (in parallel)
  AdditionalOffers ~ BC_Tour
  PriceValue       ~ BC_Tour
  Attractiveness   ~ BC_Tour
  FitSuitability   ~ BC_Tour
  # constructs -> booking intention (blockchain acts only through the constructs)
  BookingIntention ~ AdditionalOffers + PriceValue + Attractiveness + FitSuitability
  # residual covariances among the (now endogenous) constructs
  AdditionalOffers ~~ PriceValue + Attractiveness + FitSuitability
  PriceValue ~~ Attractiveness + FitSuitability
  Attractiveness ~~ FitSuitability
'
fit_sem_UC2 <- sem(sem_UC2, data = uc2, estimator = "ML", missing = "fiml")
cat("\n--- POOLED (n = 1244) ---\n")
print(standardizedSolution(fit_sem_UC2)[standardizedSolution(fit_sem_UC2)$op == "~",
      c("lhs","op","rhs","est.std","se","pvalue")], row.names = FALSE)
print(round(fitMeasures(fit_sem_UC2, c("cfi","tli","rmsea","srmr")),3))
cat("\nR2 (Booking):\n"); print(round(inspect(fit_sem_UC2,"r2")["BookingIntention"],3))

## TABLE A8 — Construct -> booking by blockchain condition (Use Case 2)
cat("\n--- TABLE A8: structural paths by condition (UC2) ---\n")
sem_UC2_grp <- '
  BookingIntention =~ B03Q05_1 + B03Q05_2
  PriceValue       =~ B03Q06_1 + B03Q06_2 + B03Q06_3
  Attractiveness   =~ B03Q07_1 + B03Q07_2
  FitSuitability   =~ B03Q07_3 + B03Q07_4
  AdditionalOffers =~ B03Q07_5 + B03Q07_6
  BookingIntention ~ AdditionalOffers + PriceValue + Attractiveness + FitSuitability
'
fit_sem_UC2_mg <- sem(sem_UC2_grp, data = uc2, group = "BC_Tour",
                      estimator = "ML", missing = "fiml")
print(standardizedSolution(fit_sem_UC2_mg)[standardizedSolution(fit_sem_UC2_mg)$op == "~",
      c("lhs","op","rhs","group","est.std","se","pvalue")], row.names = FALSE)


################################################################################
##  ONLINE APPENDIX — standardised loadings & item descriptives               ##
################################################################################
cat("\n############  TABLE A3 — Standardised loadings, all items (UC1)  ############\n")
print(standardizedSolution(fit_cfa_UC1)[standardizedSolution(fit_cfa_UC1)$op=="=~",
      c("lhs","rhs","est.std","se","pvalue")], row.names = FALSE)
cat("\n############  TABLE A6 — Standardised loadings, all items (UC2)  ############\n")
print(standardizedSolution(fit_cfa_UC2)[standardizedSolution(fit_cfa_UC2)$op=="=~",
      c("lhs","rhs","est.std","se","pvalue")], row.names = FALSE)

################################################################################
##  TABLE A10 — Item-level descriptive statistics (full sample, N = 1244)      ##
################################################################################
cat("\n############  TABLE A10 — Item-level descriptives (full sample)  ############\n")
all_items <- c(unlist(items_UC1, use.names = FALSE), unlist(items_UC2, use.names = FALSE))
a10 <- data.frame(
  Item = all_items,
  M    = round(sapply(all_items, function(i) mean(dat[[i]], na.rm = TRUE)), 2),
  SD   = round(sapply(all_items, function(i) sd(dat[[i]],   na.rm = TRUE)), 2),
  row.names = NULL)
print(a10, row.names = FALSE)

cat("\n\n=====================  END OF SCRIPT  =====================\n")
