# load packages 
# install.packages('Matching')
library(Matching)
library(dplyr)
library(magrittr)
library(gtools)

# read in data
df = read.csv('tbi_frame.csv')[,2:46]
cols = colnames(df)[8:40]

# conver these columns into categorical variables (factors)
cols_cat  = c("Subject"          ,    "Sex"                   ,  "Handedness"      ,      "Prior.Resection"   ,  
           "Left.Frontal"     ,    "Right.Frontal"   ,     "Left.Temporal"    ,    "Left.MTl"       ,      "Right.Temporal"  ,     "Right.MTl"   ,         "Left.Parietal"  ,      "Right.Parietal"  ,    
           "Left.Occipital"   ,    "Right.Occipital"   ,   "Left.Limbic"      ,    "Right.Limbic"   ,      "Left.Insula"    ,      "Right.Insula"   ,      "Depth"    ,            "Grid"   ,             
            "Strip"          ,      "Left.Frontal.SOZ"  ,   "Right.Frontal.SOZ" ,   "Left.Temporal.SOZ"   , "Left.MTL.SOZ"   ,      "Right.Temporal.SOZ" ,  "Right.MTL.SOZ"  ,      "Left.Parietal.SOZ" ,  
            "Right.Parietal.SOZ" ,  "Left.Occipital.SOZ" ,  "Right.Occipital.SOZ" , "Left.Limbic.SOZ"  ,    "Right.Limbic.SOZ" ,    "Left.Insula.SOZ"  ,    "Right.Insula.SOZ" ,    "is_TBI"    ,          
              "TBI_severe" )

cols_num = c("Age", "Years.of.Education",  "Age.of.Seizure.Onset", "fr1_recall_rate"   ,   "catfr1_recall_rate" ,  "nfr1", "ncatfr1", "avg_recall_rate")

# convert to correct types
df %<>%mutate_each_(funs(as.factor(.)), cols_cat)
df %<>%mutate_each_(funs(as.numeric(.)), cols_num)


# compute weights to deal with class imbalance (few TBI subjects) to pass them into the regression model
n_pos = sum(df[,'is_TBI'] == 1)
n_tot = dim(df)[1]
pos_weight = n_tot/n_pos
neg_weight = n_tot/(n_tot-n_pos)
weights = rep(0, dim(df)[1])
weights[df[,'is_TBI'] == 1] = pos_weight
weights[df[,'is_TBI'] != 1] = neg_weight


# create a propensity score model 

# demographics model
glm1 <- glm(is_TBI ~ Sex + Age + Age.of.Seizure.Onset + Prior.Resection, data=df, weights = weights, family = 'quasibinomial')


# electrode coverage model
glm2 <- glm(is_TBI ~ Left.Frontal + Right.Frontal + Left.Temporal + Right.Temporal
            + Left.Parietal + Right.Parietal + Left.Occipital + Right.Occipital +
              Left.Limbic + Right.Limbic + Left.Insula + Right.Insula + Depth +
              Grid + Strip + Left.Frontal.SOZ +  Right.Frontal.SOZ +  Left.Temporal.SOZ + Left.MTL.SOZ + Right.Temporal.SOZ + Right.MTL.SOZ + Left.Parietal.SOZ + Right.Parietal.SOZ + Left.Occipital.SOZ + Right.Occipital.SOZ + Left.Limbic.SOZ + Right.Limbic.SOZ+ Left.Insula.SOZ + Right.Insula.SOZ, data=df, weights = weights, family = 'quasibinomial')

# inspect the model to see which variables are signif
summary(glm1)

X1 = glm1$fitted  # get propensity scores, which are the regression's predictions
X2 = glm2$fitted
X3 = df$Handedness  # match handedness exactly
X = cbind(X1,X2,X3)  # create matching criteria
Treat = df$is_TBI
Treat = Treat == 1  # convert is_TBI to true or false, required by the Match package


n_matches = 1
rr_full = Match(Tr = Treat, X = X, M = n_matches, replace = F, exact = c(F,F,T))  # exact specifies whether the matches have to be exact. In this case, we match handedness exactly. M = 2 specifies the number of matches per subject
print(rr_full)

# check match balances for a couple of variables
mb_full = MatchBalance(Treat ~ Sex + Age + Handedness + Age.of.Seizure.Onset + Prior.Resection + Left.Temporal + Left.Frontal + Right.Temporal, data = df, match.out = rr_full, nboots = 1000, paired =  T, print.level = T)


# two step process (not good :( )
n_matches = 3
rr1 = Match(Tr = Treat, X = cbind(X1,X3) , M = n_matches, replace = F, exact = c(F,T))  # exact specifies whether the matches have to be exact. In this case, we match handedness exactly. M = 2 specifies the number of matches per subject
print(rr1)

# check match balances for a couple of variables
mb1 = MatchBalance(Treat ~ Sex + Age + Handedness + Age.of.Seizure.Onset + Prior.Resection + Left.Temporal.SOZ + Left.Frontal.SOZ + Right.Temporal.SOZ, data = df, match.out = rr, nboots = 1000, paired =  T, print.level = T)



rr1$index.treated # get indices for TBI subjects
rr1$index.control # get indices for the matches

indices_treated = rr1$index.treated[seq(1, length(rr1$index.treated),n_matches)]
code =  as.factor(c(indices_treated,rep(indices_treated, each = n_matches)))
indices = c(rr1$index.treated[seq(1, length(rr1$index.treated),n_matches)], rr1$index.control)
df_sub = df[indices,]

Treat = df_sub$is_TBI
Treat = Treat == 1
rr2 =  Matchby(Tr = Treat, X = X2[indices] , M = 1, replace = F, by = code)

mb2 = MatchBalance(Treat ~ Sex + Age + Handedness + Age.of.Seizure.Onset + Prior.Resection + Left.Temporal + Left.Frontal + Right.Temporal, data = df_sub, match.out = rr2, nboots = 1000, paired =  T, print.level = T)



# # save out results
# write.csv(df[TBI,], 'tbi.csv')
# write.csv(df[best,], 'best_matches.csv')
# write.csv(df[second_best,], 'second_best_matches.csv')
