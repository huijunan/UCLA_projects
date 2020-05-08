library(e1071)
library(dplyr)
library(readxl)
library(magrittr)
library(caTools)
library(randomForest)
library(xgboost)
library(caret)
library(rpart)
library(rpart.plot)

original <- read.csv("diabetic_data.csv")

duplicated_pt_nbr <- original %>% 
  select(patient_nbr) %>%
  mutate(d = ifelse(duplicated(patient_nbr), 1, 0)) %>%
  filter(d == 1) %>%
  group_by(patient_nbr) %>%
  summarize(n_distinct(patient_nbr))

diabetes <- original %>%
  # replace ? by NA
  mutate_at(c("race", "diag_1", "diag_2", "diag_3", "medical_specialty"), ~na_if(., "?")) %>% 
  # Delete the 2 columns with more than 50% missing values and two id columns
  select(-weight, -payer_code, -encounter_id) %>% 
  # age transformation
  mutate(age = ifelse(age == "[0-10)",  0, age),
         age = ifelse(age == "[10-20)", 10, age),
         age = ifelse(age == "[20-30)", 20, age),
         age = ifelse(age == "[30-40)", 30, age),
         age = ifelse(age == "[40-50)", 40, age),
         age = ifelse(age == "[50-60)", 50, age),
         age = ifelse(age == "[60-70)", 60, age),
         age = ifelse(age == "[70-80)", 70, age),
         age = ifelse(age == "[80-90)", 80, age),
         age = ifelse(age == "[90-100)", 90, age),
         gender = ifelse(gender=="Male", 1, 0), # 1 if male
         # race transformation
         White = ifelse(race=="Caucasian", 1, 0),
         Black = ifelse(race=="AfricanAmerican", 1, 0),
         # Admission type & source transformation
         admission_source_id = ifelse(admission_source_id==7, 1, 0), # 1 if emergency
         admission_type_id = ifelse(admission_type_id==1, 1, 0), # 1 if emergency
         # discharge disposition transformation
         discharge_disposition_id = ifelse(discharge_disposition_id==1, 1, 0), # 1 if home
         # Test result transformation
         A1C_test = ifelse(A1Cresult!="None", 1, 0),
         A1C_test_high = ifelse(A1Cresult==">7" | A1Cresult==">8", 1, 0),
         glu_test = ifelse(max_glu_serum!="None", 1, 0),
         glu_test_high = ifelse(max_glu_serum==">200" |max_glu_serum==">300", 1, 0),
         # Medicine prescribed & up & down
         Insulin_up = ifelse(insulin=="Up", 1, 0),
         Insulin_down = ifelse(insulin=="Down", 1, 0),
         Insulin = ifelse(insulin!="No", 1, 0),
         diabetesMed = ifelse(diabetesMed=="Yes", 1, 0),
         one = ifelse(patient_nbr %in% duplicated_pt_nbr$patient_nbr, 1, 0),
         Med = ifelse(metformin!="No" | repaglinide!="No" | nateglinide!="No" | chlorpropamide!="No" | 
                        glimepiride!="No" | acetohexamide!="No" | glipizide!="No" | glyburide!="No" | 
                        tolbutamide!="No" | pioglitazone!="No" | rosiglitazone!="No" | acarbose!="No" | 
                        miglitol!="No" | troglitazone!="No" | tolazamide!="No" | examide!="No" | 
                        citoglipton!="No" | glyburide.metformin!="No" | glipizide.metformin!="No" | 
                        glimepiride.pioglitazone!="No" | metformin.rosiglitazone!="No" | 
                        metformin.pioglitazone!="No", 1, 0),
         Med_up = ifelse(metformin=="Up" | repaglinide=="Up" | nateglinide=="Up" | chlorpropamide=="Up" | 
                           glimepiride=="Up" | acetohexamide=="Up" | glipizide=="Up" | glyburide=="Up" | 
                           tolbutamide=="Up" | pioglitazone=="Up" | rosiglitazone=="Up" | acarbose=="Up" | 
                           miglitol=="Up" | troglitazone=="Up" | tolazamide=="Up" | examide=="Up" | 
                           citoglipton=="Up" | glyburide.metformin=="Up" | glipizide.metformin=="Up" | 
                           glimepiride.pioglitazone=="Up" | metformin.rosiglitazone=="Up" | 
                           metformin.pioglitazone=="Up", 1, 0),
         Med_down = ifelse(metformin=="Down" | repaglinide=="Down" | nateglinide=="Down" | chlorpropamide=="Down" | 
                           glimepiride=="Down" | acetohexamide=="Down" | glipizide=="Down" | glyburide=="Down" | 
                           tolbutamide=="Down" | pioglitazone=="Down" | rosiglitazone=="Down" | acarbose=="Down" | 
                           miglitol=="Down" | troglitazone=="Down" | tolazamide=="Down" | examide=="Down" | 
                           citoglipton=="Down" | glyburide.metformin=="Down" | glipizide.metformin=="Down" | 
                           glimepiride.pioglitazone=="Down" | metformin.rosiglitazone=="Down" | 
                           metformin.pioglitazone=="Down", 1, 0),
         # diag_1 transformation       
         diag_1 = as.character(diag_1),
         diag_1 = ifelse(diag_1 %in% c(390:459, 785), "Circulatory", diag_1),
         diag_1 = ifelse(diag_1 %in% c(460:519, 786), "Respiratory", diag_1),
         diag_1 = ifelse(diag_1 %in% c(520:579, 787), "Digestive", diag_1),
         diag_1 = ifelse(diag_1 %in% c(seq(250,250.99, 0.01)), "Diabetes", diag_1),
         diag_1 = ifelse(diag_1 %in% c(800:999), "Injury", diag_1),
         diag_1 = ifelse(diag_1 %in% c(710:739), "Musculoskeletal", diag_1),
         diag_1 = ifelse(diag_1 %in% c(580:629, 788), "Genitourinary", diag_1),
         diag_1 = ifelse(diag_1 %in% c(140:239), "Neoplasms", diag_1),
         diag_1 = ifelse(!diag_1 %in% c("Circulatory", "Respiratory", "Digestive", "Diabetes",
                                        "Injury", "Musculoskeletal", "Genitourinary"), "Other", diag_1),
         # diag_2 transformation 
         diag_2 = as.character(diag_2),
         diag_2 = ifelse(diag_2 %in% c(390:459, 785), "Circulatory", diag_2),
         diag_2 = ifelse(diag_2 %in% c(460:519, 786), "Respiratory", diag_2),
         diag_2 = ifelse(diag_2 %in% c(520:579, 787), "Digestive", diag_2),
         diag_2 = ifelse(diag_2 %in% c(seq(250,250.99, 0.01)), "Diabetes", diag_2),
         diag_2 = ifelse(diag_2 %in% c(800:999), "Injury", diag_2),
         diag_2 = ifelse(diag_2 %in% c(710:739), "Musculoskeletal", diag_2),
         diag_2 = ifelse(diag_2 %in% c(580:629, 788), "Genitourinary", diag_2),
         diag_2 = ifelse(diag_2 %in% c(140:239), "Neoplasms", diag_2),
         diag_2 = ifelse(!diag_2 %in% c("Circulatory", "Respiratory", "Digestive", "Diabetes",
                                        "Injury", "Musculoskeletal", "Genitourinary"), "Other", diag_2),
         # diag_3 transformation 
         diag_3 = as.character(diag_3),
         diag_3 = ifelse(diag_3 %in% c(390:459, 785), "Circulatory", diag_3),
         diag_3 = ifelse(diag_3 %in% c(460:519, 786), "Respiratory", diag_3),
         diag_3 = ifelse(diag_3 %in% c(520:579, 787), "Digestive", diag_3),
         diag_3 = ifelse(diag_3 %in% c(seq(250,250.99, 0.01)), "Diabetes", diag_3),
         diag_3 = ifelse(diag_3 %in% c(800:999), "Injury", diag_3),
         diag_3 = ifelse(diag_3 %in% c(710:739), "Musculoskeletal", diag_3),
         diag_3 = ifelse(diag_3 %in% c(580:629, 788), "Genitourinary", diag_3),
         diag_3 = ifelse(diag_3 %in% c(140:239), "Neoplasms", diag_3),
         diag_3 = ifelse(!diag_3 %in% c("Circulatory", "Respiratory", "Digestive", "Diabetes",
                                        "Injury", "Musculoskeletal", "Genitourinary"), "Other", diag_3),
         diag_1_Circulatory = ifelse(diag_1 == "Circulatory", 1, 0),
         diag_1_Respiratory = ifelse(diag_1 == "Respiratory", 1, 0),
         diag_1_Digestive = ifelse(diag_1 == "Digestive", 1, 0),
         diag_1_diabetes = ifelse(diag_1 == "Diabetes", 1, 0),
         diag_1_Injury = ifelse(diag_1 == "Injury", 1, 0),
         diag_1_Musculoskeletal = ifelse(diag_1 == "Musculoskeletal", 1, 0),
         diag_1_Genitourinary = ifelse(diag_1 == "Genitourinary", 1, 0),
         diag_1_Neoplasms = ifelse(diag_1 == "Neoplasms", 1, 0),
         
         diag_2_Circulatory = ifelse(diag_2 == "Circulatory", 1, 0),
         diag_2_Respiratory = ifelse(diag_2 == "Respiratory", 1, 0),
         diag_2_Digestive = ifelse(diag_2 == "Digestive", 1, 0),
         diag_2_diabetes = ifelse(diag_2 == "Diabetes", 1, 0),
         diag_2_Injury = ifelse(diag_2 == "Injury", 1, 0),
         diag_2_Musculoskeletal = ifelse(diag_2 == "Musculoskeletal", 1, 0),
         diag_2_Genitourinary = ifelse(diag_2 == "Genitourinary", 1, 0),
         diag_2_Neoplasms = ifelse(diag_2 == "Neoplasms", 1, 0),
         
         diag_3_Circulatory = ifelse(diag_3 == "Circulatory", 1, 0),
         diag_3_Respiratory = ifelse(diag_3 == "Respiratory", 1, 0),
         diag_3_Digestive = ifelse(diag_3 == "Digestive", 1, 0),
         diag_3_diabetes = ifelse(diag_3 == "Diabetes", 1, 0),
         diag_3_Injury = ifelse(diag_3 == "Injury", 1, 0),
         diag_3_Musculoskeletal = ifelse(diag_3 == "Musculoskeletal", 1, 0),
         diag_3_Genitourinary = ifelse(diag_3 == "Genitourinary", 1, 0),
         diag_3_Neoplasms = ifelse(diag_3 == "Neoplasms", 1, 0),
         
         change = ifelse(change=="Ch", 1, 0),
         medical_specialty = as.character(medical_specialty),
         medical_specialty = ifelse(is.na(medical_specialty), "Missing", medical_specialty),
         medical_specialty = ifelse(medical_specialty %in% c("Cardiology", "Cardiology-Pediatric",
                                                             "Gastroenterology", "Endocrinology", 
                                                             "Endocrinology-Metabolism", "Hematology", 
                                                             "Hematology/Oncology", "InternalMedicine", 
                                                             "Nephrology", "InfectiousDiseases", 
                                                             "Oncology", "Proctology", "Pulmonology", 
                                                             "Rheumatology", "SportsMedicine", 
                                                             "Urology"), "InternalMedicine", medical_specialty),
         medical_specialty = ifelse(medical_specialty %in% c("Orthopedics", "Orthopedics-Reconstructive", 
                                                             "Osteopath", "Otolaryngology", "Surgeon", 
                                                             "Surgery-Cardiovascular", "Surgery-Cardiovascular/Thoracic", 
                                                             "Surgery-Colon&Rectal", "Surgery-General", 
                                                             "Surgery-Maxillofacial", "Surgery-Neuro", "Surgery-Pediatric", 
                                                             "Surgery-Plastic", "Surgery-PlasticwithinHeadandNeck", 
                                                             "Surgery-Thoracic", "Surgery-Vascular", 
                                                             "SurgicalSpecialty"), "Surgery", medical_specialty),
         medical_specialty = ifelse(!medical_specialty %in% c("Missing", "InternalMedicine", "Surgery",
                                                              "Family/GeneralPractice", 
                                                              "Emergency/Trauma"), "Other", medical_specialty),
         # specialty of admitting physician
         Spec_emergency = ifelse(medical_specialty=="Emergency/Trauma", 1, 0),
         Spec_family = ifelse(medical_specialty=="Family/GeneralPractice", 1, 0),
         Spec_intmed = ifelse(medical_specialty=="InternalMedicine", 1, 0),
         Spec_missing = ifelse(medical_specialty=="Missing", 1, 0),
         Spec_surgery = ifelse(medical_specialty=="Surgery", 1, 0)) %>%
  na.omit() %>%
  select(-metformin, -repaglinide, -nateglinide, -chlorpropamide,
           -glimepiride, -acetohexamide, -glipizide, -glyburide,
           -tolbutamide, -pioglitazone, -rosiglitazone, -acarbose,
           -miglitol, -troglitazone, -tolazamide, -examide,
           -citoglipton, -glyburide.metformin, -glipizide.metformin,
           -glimepiride.pioglitazone, -metformin.rosiglitazone,
           -metformin.pioglitazone, -insulin, -race, -medical_specialty, -A1Cresult, -max_glu_serum) %>%
  mutate(readmitted = ifelse(readmitted == "NO", 1, 0),
         readmitted = as.factor(readmitted)) %>%
  select(-diag_1, -diag_2, -diag_3, -patient_nbr)

data <- diabetes %>%
  filter(Spec_missing==0) %>%
  select(-Spec_missing, -diag_1_Neoplasms, -diag_2_Neoplasms, -diag_3_Neoplasms)

# Getting subset for tuning parameter (validation set)
set.seed(1234)
split_v = sample.split(data$readmitted, SplitRatio = 0.9)
validation_set = subset(data, split_v == FALSE)
new_data = subset(data, split_v == TRUE)
split = sample.split(new_data$readmitted, SplitRatio = 0.8)
training_set = subset(new_data, split == TRUE)
test_set = subset(new_data, split == FALSE)


# logistic regression
model <- glm(readmitted ~ ., family=binomial(link='logit'), data=rbind(training_set, validation_set))
summary(model)
# Run the model on the test set
test.probs <-predict(model, test_set, type='response')
pred.logit <- rep('Down',length(test.probs))
pred.logit[test.probs>=0.5] <- 'Up'

table(test_set$readmitted, pred.logit)

## SVM radial
tune_out <- tune.svm(readmitted ~ ., data=validation_set, 
                     gamma=10^(-4:4),
                     cost=10^(-4:4),kernel="radial") # gamma = 0.125, cost = 1
svm_model <- svm(readmitted ~ ., data=training_set, 
                 method="C-classification", 
                 kernel="radial",
                 cost=tune_out$best.parameters$cost,
                 gamma=tune_out$best.parameters$gamma)
summary(svm_model)
pred_test <-predict(svm_model,test_set)
mean(pred_test==test_set$readmitted)
table(test_set$readmitted, pred_test)

### CART
set.seed(1234)
tree <- rpart(readmitted ~ ., data = rbind(training_set, validation_set), control = rpart.control(cp = 0.0001))
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)
#plot the tree
only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]

par(xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("died","survived"), fill = c("pink", "palegreen3"),
       bty = "n")
#prediction
pred.cart <- tree.pruned %>% 
  predict(test_set, type = "class")
mean(pred.cart == test_set$readmitted)
table(test_set$readmitted, pred.cart)

### Random Forest
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid')
#create tunegrid with 15 values from 1:15 for mtry to tunning model. 
tunegrid <- expand.grid(.mtry = (1:15)) 
rf_gridsearch <- train(readmitted ~ ., 
                       data = validation_set,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid)
print(rf_gridsearch)
plot(rf_gridsearch, main = "Parameter Tuning Process for mtry")

set.seed(1234)
rf.model <- randomForest(x = training_set %>% select(-readmitted), y = training_set$readmitted,
                         xtest = test_set %>% select(-readmitted), ytest = test_set$readmitted,
                         mtry = 7)
summary(rf.model)
rf.pred <- rf.model$test$predicted
mean(rf.pred==test_set$readmitted)
table(test_set$readmitted, rf.pred)


### XGBoost
library(mlr)
set.seed(1234)
trainTask <- makeClassifTask(data = training_set, target = "readmitted")
testTask <- makeClassifTask(data = test_set, target = "readmitted")
validationTask <- makeClassifTask(data = validation_set, target = "readmitted", positive = 1)

xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200
  )
)

xgb_params <- makeParamSet(
  makeIntegerParam("nrounds", lower = 100, upper = 500),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("eta", lower = .1, upper = .5),
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)
control <- makeTuneControlRandom(maxit = 1)
resample_desc <- makeResampleDesc("CV", iters = 4)
tuned_params <- tuneParams(
  learner = xgb_learner,
  task = validationTask,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)

xgb_tuned_learner <- setHyperPars(
  learner = xgb_learner,
  par.vals = tuned_params$x
)

# Re-train parameters using tuned hyperparameters (and full training set)
xgb_model <- train(xgb_tuned_learner, trainTask)
result <- predict(xgb_model, testTask)

mean(result$data$truth==result$data$response)
table(result$data$truth, result$data$response)
