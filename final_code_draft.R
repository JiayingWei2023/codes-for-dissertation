
######################################## Data Pre-processing ########################################
# 1. process the missing values
## (1) the Howell data set
data_original = read.csv("D:\\AEdinburgh\\dissertation\\code\\Howell.csv")
data_original[data_original == 0] = NA # replace 0 with NA
### check the data missing mechanism
library(naniar)  
data_check_missing = data_original[,c(40,61,63,64,65,80,81,82,83,84,85,86)]
vis_miss(data_check_missing) # visualize the missing mechanism
MCAR = data_original[,-c(2,4,61,63,64,65,80,81,82,83,84,85,86)]
library(MissMech)
TestMCARNormality(MCAR) # check whether GLS is MCAR
### deal with GLS: replace with the group mean 
library(dplyr)
data_original = data_original %>%                         
  group_by(PopNum) %>%  
  mutate(GLS = ifelse(is.na(GLS), mean(GLS, na.rm = TRUE), GLS))  
### deal with other missing values: remove these variables
data_original = as.data.frame(data_original[,-c(3,61,63,64,65,80,81,82,83,84,85,86)])

## (2) the Howell Test data set
data_test = read.csv("D:\\AEdinburgh\\dissertation\\code\\HowellTest.csv")
data_test[data_test == 0] = NA # replace 0 with NA                           
### check the data missing mechanism
data_check_missing2 = data_test[,c(43,64,66,67,68,83,84,85,86,87,88,89)]
head(data_check_missing2)
vis_miss(data_check_missing2) # visualize the missing mechanism
### make the data format in normal
# library(tidyr)
data_test = data_test %>% select(-Origin, -Museum.location, -Museum.number, -Comments) # remove irrelevant variables
data_test = data_test %>% rename(Population = Tribe) # uniform variable names
data_test$Population = toupper(data_test$Population) # capitalized all names
### deal with GLS: replace with the group mean 
data_test = data_test %>%                         
  group_by(Population) %>%  
  mutate(GLS = ifelse(is.na(GLS), mean(GLS, na.rm = TRUE), GLS))  
### deal with other missing values: remove these variables and samples
data_test = as.data.frame(data_test[-c(516,517,518,519,520,521,522,523,524), -c(60,62,63,64,79,80,81,82,83,84,85)])


# 2. Split the original data set into training and validation sets
library(caret)
data_original$strata = interaction(data_original$Population, data_original$Sex) # create factors for stratified sampling
set.seed(123)
ratio = createDataPartition(data_original$strata, p = 0.8, list = FALSE, times = 1) # create an index of the training set (80%)
training = data_original[ratio, ] # create training and verification sets
validation = data_original[-ratio, ]
training$strata = NULL # remove the factors
validation$strata = NULL
data_original$strata = NULL

table(training$Population, training$Sex) # check the distribution of training and validation sets
table(validation$Population, validation$Sex)


################################################ EDA ################################################
# 1. overview
## the mean, min, max, upper and lower quartile
measurements = data_original[,4:74] # select the part of numeric variables (4-74 cols)
summary(measurements)

# 2. box plot
library(ggplot2)
variables = names(data_original)[4:74] # List the names of all measurement variables 
## (1) Box plots of sex 
for (var in variables) {
  p = ggplot(data_original, aes_string(x = "Sex", y = var, fill = "Sex")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var, "by Sex"),
         x = "Sex", y = var, fill = "Sex") +
    theme_minimal()
  print(p)
}
## (2) Box plots of populations
for (var in variables) {
  p = ggplot(data_original, aes_string(x = "as.factor(Population)", y = var, fill = "as.factor(Population)")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var, "by Population"),
         x = "Population", y = var, fill = "Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
}


############################################### ANOVA ###############################################
# 1. t test for sex
## 7/71 p values are bigger than 0.05
for (var in variables) {
  t = t.test(data_original[[var]] ~ data_original$Sex)
  cat("\n\nT-test results for", var, ":\n")
  print(t)
}

# 2. one-factor ANOVA for populations
## p values are all significantly less than 0.05
for (var in variables) {
  anova = aov(data_original[[var]] ~ data_original$Population)
  cat("\nANOVA for", var, "\n")
  print(summary(anova))
}

# 3. two-factor ANOVA for interactions
## 44/71 p values of interaction are bigger than 0.05
for (var in variables) {
  formula = as.formula(paste(var, "~ Sex * Population"))
  two_anova = aov(formula, data = data_original)
  cat("\n\nTwo-factor ANOVA for", var, "\n")
  print(summary(two_anova))
}

######################################## Multiple Regression ########################################
# convert Sex and Population into factors
data_original$Sex = as.factor(data_original$Sex)
data_original$Population = as.factor(data_original$Population)

# conduct model for each measured variable separately
results = list()
for (var in variables) {
  formula = as.formula(paste(var, "~ Sex + Population + Sex:Population"))
  model = lm(formula, data = data_original)
  results[[var]] = summary(model)
}

# see the significant parts and R squares
significant_results = list()
r2 = numeric(length(variables))
adj_r2 = numeric(length(variables))

for (i in seq_along(variables)) {
  var = variables[i]
  res = results[[var]]
  sig = res$coefficients[res$coefficients[, "Pr(>|t|)"] < 0.05, , drop = FALSE]
  if (nrow(sig) > 0) {
    significant_results[[var]] = sig
  }
  r2[i] = res$r.squared
  adj_r2[i] = res$adj.r.squared
}

# print all results with significant p-values
for (var in names(significant_results)) {
  cat("Significant results for", var, ":\n")
  print(significant_results[[var]])
  cat("\n\n")
}

# print R square and adjusted R square
r_squared = data.frame(Variable = variables, R_squared = r2, Adjusted_R_squared = adj_r2)
print(r_squared)

# store regression results
library(openxlsx) 
## initializes the table that stores the salient results
coef_names = c("(Intercept)", "SexM", 
                paste("Population", levels(data_original$Population)[-1], sep=""),
                paste("SexM:Population", levels(data_original$Population)[-1], sep=""))
result_matrix = matrix(NA, nrow = length(coef_names), ncol = length(variables))
rownames(result_matrix) = coef_names
colnames(result_matrix) = variables
## fill the table with significant results
for (i in seq_along(variables)) {
  var = variables[i]
  res = results[[var]]
  sig = res$coefficients[res$coefficients[, "Pr(>|t|)"] < 0.05, , drop = FALSE]
  
  if (nrow(sig) > 0) {
    significant_results[[var]] = sig
    for (coef_name in rownames(sig)) {
      coef_name_clean = gsub(" ", "", coef_name, fixed = TRUE)
      coef_name_clean = gsub(":", ":", coef_name_clean, fixed = TRUE)
      
      if (coef_name_clean %in% rownames(result_matrix)) {
        result_matrix[coef_name_clean, var] <- sig[coef_name, "Estimate"]
      }
    }
  }
  r2[i] = res$r.squared
  adj_r2[i] = res$adj.r.squared
}
## create an empty data frame and store the results
result_df = as.data.frame(result_matrix, stringsAsFactors = FALSE)
result_df = cbind(Coefficient = rownames(result_df), result_df)
write.xlsx(result_df, "C:\\Users\\86136\\Desktop\\multiple_results.xlsx")






############################################ PCA and LDA ############################################
library(MASS) # lda
library(e1071) # confusionMatrix

# 1. perform PCA on training set
## (1) perfoem PCA
training_pca = training[, !(names(training) %in% c('ID','Sex','Population'))]
training_scale = scale(training_pca)
pca = prcomp(training_scale, scale. = TRUE)
## (2) check the effect of PCA
ev_pca = summary(pca)$importance[2,]
cumu_pca = summary(pca)$importance[3,]
print(ev_pca) 
print(cumu_pca) 
## (3) draw scree plots
scree_pca = data.frame(
  Principal_Component = 1:length(ev_pca),
  Explained_Variance = ev_pca
)
ggplot(scree_pca, aes(x = Principal_Component, y = Explained_Variance)) +
  geom_bar(aes(fill = "Explained Variance"), stat = "identity") +
  geom_line(aes(y = cumu_pca, color = "Cumulative Variance"), size = 1) +
  geom_point(aes(y = cumu_pca, color = "Cumulative Variance"), size = 2) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Explained Variance") +
  scale_fill_manual(name = "Legend", values = c("Explained Variance" = "steelblue")) +
  scale_color_manual(name = "Legend", values = c("Cumulative Variance" = "red")) +
  theme_minimal()
## (4) select the number of principal components
num_components = 14
training_reduced = data.frame(pca$x[, 1:num_components])
training_reduced$Population = training$Population
training_reduced$Sex = training$Sex

# 2. build LDA model and predict on validation set
## (1) perform LDA on Sex and Population separately
lda_sex = lda(Sex ~ . - Population, data = training_reduced)
lda_pop = lda(Population ~ . - Sex, data = training_reduced)
## (2) transform the validation set based on PCA result
validation_pca = validation[, !(names(validation) %in% c('ID','Sex','Population'))]
validation_scale = scale(validation_pca)
validation_reduced = data.frame(predict(pca, validation_scale)[, 1:num_components])
validation_reduced$Population = validation$Population
validation_reduced$Sex = validation$Sex
## (3) predict and evaluate the effects of Sex model
lda_pre1 = predict(lda_sex, validation_reduced)$class
### confusion matrix
lda_pre1 = factor(lda_pre1)
validation_reduced$Sex = factor(validation_reduced$Sex)
lda_confu1 = confusionMatrix(lda_pre1, validation_reduced$Sex)
print(lda_confu1)
### F1 score
precision = lda_confu1$byClass["Pos Pred Value"] # extract Precision
recall = lda_confu1$byClass["Sensitivity"] # extract Recall
f1_lda1 = 2 * (precision * recall) / (precision + recall) # calculate F1-score
print(as.numeric(f1_lda1))
## (4) predict and evaluate the effects of Population model
lda_pre2 = predict(lda_pop, validation_reduced)$class
### confusion matrix
lda_pre2 = factor(lda_pre2)
validation_reduced$Population = factor(validation_reduced$Population)
lda_confu2 = confusionMatrix(lda_pre2, validation_reduced$Population)
print(lda_confu2)
### F1 score
precision = lda_confu2$byClass[, "Pos Pred Value"]
recall = lda_confu2$byClass[, "Sensitivity"]
precision[is.na(precision)] = 0 # replace NAs with 0
recall[is.na(recall)] = 0
f1_scores_lda = 2 * (precision * recall) / (precision + recall) # calculate the F1-score for each category
f1_scores_lda[is.na(f1_scores_lda)] = 0 # replace NAs with 0
support = rowSums(lda_confu2$table) # extract Support
f1_lda2 = sum(f1_scores_lda * support) / sum(support) # calculate the weighted average F1-score
print(as.numeric(f1_lda2))


########################################### Decision Tree ###########################################
library(rpart)
library(rpart.plot)
# 1. conduct decision tree model
sex_tree = rpart(Sex ~ . - Population - ID, data = training, method = "class")
pop_tree = rpart(Population ~ . - Sex - ID, data = training, method = "class")
# 2. visualize decision tree
rpart.plot(sex_tree)
rpart.plot(pop_tree)
# 3. predict the validation set and calculate the confusion matrix (for Sex)
dt_pre1 = predict(sex_tree, validation, type = "class")
dt_pre1 = factor(dt_pre1)
validation$Sex = factor(validation$Sex)
dt_confu1 = confusionMatrix(dt_pre1, validation$Sex)
print(dt_confu1)
## F1 score
precision = dt_confu1$byClass["Pos Pred Value"] # extract Precision
recall = dt_confu1$byClass["Sensitivity"] # extract Recall
f1_dt1 = 2 * (precision * recall) / (precision + recall) # calculate F1-score
print(as.numeric(f1_dt1))
# 4. predict the validation set and calculate the confusion matrix (for Population)
dt_pre2 = predict(pop_tree, validation, type = "class")
dt_pre2 = factor(dt_pre2)
validation$Population = factor(validation$Population) 
dt_confu2 = confusionMatrix(dt_pre2, validation$Population)
print(dt_confu2)
## F1 score
precision = dt_confu2$byClass[, "Pos Pred Value"]
recall = dt_confu2$byClass[, "Sensitivity"]
precision[is.na(precision)] = 0 # replace NAs with 0
recall[is.na(recall)] = 0
f1_scores_dt = 2 * (precision * recall) / (precision + recall) # calculate the F1-score for each category
f1_scores_dt[is.na(f1_scores_dt)] = 0 # replace NAs with 0
support = rowSums(dt_confu2$table) # extract Support
f1_dt2 = sum(f1_scores_dt * support) / sum(support) # calculate the weighted average F1-score
print(as.numeric(f1_dt2))


################################################ SVM ################################################
# 1. data prepare
## (1) standardize all the numeric variables
prepare1 = preProcess(training[, !(names(training) %in% c("ID", "Sex", "Population"))], method = c("center", "scale"))
training_standardized = training
training_standardized[, !(names(training) %in% c("ID", "Sex", "Population"))] = predict(prepare1, training[, !(names(training) %in% c("ID", "Sex", "Population"))])
prepare2 = preProcess(validation[, !(names(validation) %in% c("ID", "Sex", "Population"))], method = c("center", "scale"))
validation_standardized = validation
validation_standardized[, !(names(validation) %in% c("ID", "Sex", "Population"))] = predict(prepare2, validation[, !(names(validation) %in% c("ID", "Sex", "Population"))])
## (2) transfer data into factor
training_standardized$Sex = as.factor(training_standardized$Sex)
training_standardized$Population = as.factor(training_standardized$Population)
validation_standardized$Sex = as.factor(validation_standardized$Sex)
validation_standardized$Population = as.factor(validation_standardized$Population)

# 2. SVM for gender
svm_sex1 = svm(Sex ~ . - Population - ID, data = training_standardized, kernel = "linear")
sex_predictions1 = predict(svm_sex1, validation_standardized)
sex_predictions1 = factor(sex_predictions1)
validation_standardized$Sex = factor(validation_standardized$Sex)  
svm_confu1 = confusionMatrix(sex_predictions1, validation_standardized$Sex)
print(svm_confu1)
## F1 score
precision = svm_confu1$byClass["Pos Pred Value"] # extract Precision
recall = svm_confu1$byClass["Sensitivity"] # extract Recall
f1_svm1 = 2 * (precision * recall) / (precision + recall) # calculate F1-score
print(as.numeric(f1_svm1))

# 3. SVM for populations
svm_pop1 = svm(Population ~ . - Sex - ID, data = training_standardized, kernel = "linear")
pop_predictions1 = predict(svm_pop1, validation_standardized)
all_levels = union(levels(validation_standardized$Population), levels(pop_predictions1))
validation_standardized$Population = factor(validation_standardized$Population, levels = all_levels)
pop_predictions1 = factor(pop_predictions1, levels = all_levels)
svm_confu2 = confusionMatrix(pop_predictions1, validation_standardized$Population)
print(svm_confu2)
## F1 score
precision = svm_confu2$byClass[, "Pos Pred Value"]
recall = svm_confu2$byClass[, "Sensitivity"]
precision[is.na(precision)] = 0 # replace NAs with 0
recall[is.na(recall)] = 0
f1_scores_svm = 2 * (precision * recall) / (precision + recall) # calculate the F1-score for each category
f1_scores_svm[is.na(f1_scores_svm)] = 0 # replace NAs with 0
support = rowSums(svm_confu2$table) # extract Support
f1_svm2 = sum(f1_scores_svm * support) / sum(support) # calculate the weighted average F1-score
print(as.numeric(f1_svm2))





###################################### SVM with Original Data #######################################
# Due to the conflict of packages, we need to close the file, reopen it, and then run this section and beyond 
# 1. data prepare
## (1) the Howell data set
data_original = read.csv("D:\\AEdinburgh\\dissertation\\code\\Howell.csv")
data_original[data_original == 0] = NA # replace 0 with NA
### deal with GLS: replace with the group mean 
library(dplyr)
data_original = data_original %>%                         
  group_by(PopNum) %>%  
  mutate(GLS = ifelse(is.na(GLS), mean(GLS, na.rm = TRUE), GLS))  
### deal with other missing values: remove these variables
data_original = as.data.frame(data_original[,-c(3,61,63,64,65,80,81,82,83,84,85,86)])

## (2) the Howell Test data set
data_test = read.csv("D:\\AEdinburgh\\dissertation\\code\\HowellTest.csv")
data_test[data_test == 0] = NA # replace 0 with NA                           
### make the data format in normal
# library(tidyr)
data_test = data_test %>% select(-Origin, -Museum.location, -Museum.number, -Comments) # remove irrelevant variables
data_test = data_test %>% rename(Population = Tribe) # uniform variable names
data_test$Population = toupper(data_test$Population) # capitalized all names
### deal with GLS: replace with the group mean 
data_test = data_test %>%                         
  group_by(Population) %>%  
  mutate(GLS = ifelse(is.na(GLS), mean(GLS, na.rm = TRUE), GLS))  
### deal with other missing values: remove these variables and samples
data_test = as.data.frame(data_test[-c(516,517,518,519,520,521,522,523,524), -c(60,62,63,64,79,80,81,82,83,84,85)])

## (3) transfer data into factor
data_original$Sex = as.factor(data_original$Sex)
data_original$Population = as.factor(data_original$Population)
data_test$Sex = as.factor(data_test$Sex)
data_test$Population = as.factor(data_test$Population)
## (4) standardize all the numeric variables
library(caret)
prepare3 = preProcess(data_original[, !(names(data_original) %in% c("ID", "Sex", "Population"))], method = c("center", "scale"))
data_original_standardized = data_original
data_original_standardized[, !(names(data_original) %in% c("ID", "Sex", "Population"))] = predict(prepare3, data_original[, !(names(data_original) %in% c("ID", "Sex", "Population"))])
prepare4 = preProcess(data_test[, !(names(data_test) %in% c("ID", "Sex", "Population"))], method = c("center", "scale"))
data_test_standardized = data_test
data_test_standardized[, !(names(data_test) %in% c("ID", "Sex", "Population"))] = predict(prepare4, data_test[, !(names(data_test) %in% c("ID", "Sex", "Population"))])
## (5) split the common populations
common_populations = intersect(data_original_standardized$Population, data_test_standardized$Population)
data_test_common = data_test_standardized %>% filter(Population %in% common_populations)


# 2. SVM for gender
library(e1071)
## (1) build the model
svm_sex2 = svm(Sex ~ . , data = data_original_standardized %>% select(-Population, -ID), kernel = "linear", cost = 1, probability = TRUE)
## (2) predict
sex_predictions2 = predict(svm_sex2, data_test_standardized %>% select(-Population, -ID), probability = TRUE)
sex_probabilities2 = attr(sex_predictions2, "probabilities") # extract the probability of prediction
## (3) confusion matrix
confusionMatrix(sex_predictions2, data_test$Sex)
## (4) ROC-AUC
library(ROCR)
library(pROC)
data_test$Sex = as.factor(data_test$Sex)
positive_class = levels(data_test$Sex)[2]
sex_probabilities_positive = sex_probabilities2[, positive_class]
roc_curve = roc(response = data_test$Sex, predictor = sex_probabilities_positive, levels = rev(levels(data_test$Sex)))
auc_value = auc(roc_curve)
roc_data = data.frame(
  fpr = 1 - rev(roc_curve$specificities),  
  tpr = rev(roc_curve$sensitivities)      
)
ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  ggtitle(paste("ROC Curve for Sex Classification (AUC =", round(auc_value, 2), ")")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_minimal()

## (5) which ones are misclassified
sex_misclassified = data_test_standardized[sex_predictions2 != data_test_standardized$Sex, ]
sex_misclassified$Correct_Sex = data_test_standardized$Sex[sex_predictions2 != data_test_standardized$Sex]
sex_misclassified$Predicted_Sex = sex_predictions2[sex_predictions2 != data_test_standardized$Sex]
print(sex_misclassified[, c("ID", "Correct_Sex", "Predicted_Sex", "Population")])


# 3. SVM for populations
## (1) build the model
svm_pop2 = svm(Population ~ . - Sex - ID, data = data_original_standardized, kernel = "linear", cost = 1)
## (2) predict the common data set
pop_predictions2_common = predict(svm_pop2, data_test_common)
data_test_common$Predicted_Pop = pop_predictions2_common
all_levels = union(levels(data_test_common$Population), levels(pop_predictions2_common))
data_test_common$Population = factor(data_test_common$Population, levels = all_levels)
pop_predictions2_common = factor(pop_predictions2_common, levels = all_levels)
confusionMatrix(pop_predictions2_common, data_test_common$Population)
## (3) predict the whole data set
pop_predictions2_all = predict(svm_pop2, data_test_standardized)
data_test_standardized$Population = factor(data_test_standardized$Population)
all_levels = union(levels(data_test_standardized$Population), levels(pop_predictions2_all))
data_test_standardized$Population = factor(data_test_standardized$Population, levels = all_levels)
pop_predictions2_all = factor(pop_predictions2_all, levels = all_levels)
confusionMatrix(pop_predictions2_all, data_test_standardized$Population)


####################################### Geographical Location #######################################
# create region data frame
north_africa = data.frame(
  name = c("EGYPT", "NUBIA", "AFALOU-BOU-RHUMMEL 5", "JEBEL MOYA", "AFALOU-BOU-RHUMMEL 9", 
           "DJEBEL IROUD 1", "EGYPTIAN E?", "AFALOU-BOU-RHUMMEL 9", "DJEBEL IRHOUD 1"),
  longitude = c(30.8025, 30.4914, 5.0023, 31.3095, 5.0023, 6.0607, 30.8025, 5.0023, 6.0607),
  latitude = c(26.8206, 20.6566, 36.6564, 12.9811, 36.6564, 31.8639, 26.8206, 36.6564, 31.8639)
)

south_africa = data.frame(
  name = c("ZULU", "HOTTENTOT", "WILLEY'S KOPJE III", "BUSHMAN", "BUSH-BANTU", 
           "FISH HOEK", "BROKEN HILL I", "FINGIRA 2", "COLOURED", 
           "BUSHMAN?", "WILLEY'S KOPJE II", "MATJES RIVER"),
  longitude = c(31.4946, 18.4241, 24.0000, 26.7634, 25.0000, 18.4323, 28.0530, 34.0000, 18.4210, 26.7634, 24.0000, 23.0471),
  latitude = c(-28.5306, -33.9249, -29.0000, -24.6583, -30.0000, -34.1342, -13.8410, -14.0000, -33.9180, -24.6583, -29.0000, -34.0406)
)

east_africa = data.frame(
  name = c("ELMENTEITA B", "TWA RWANDA", "TEITA", "PYGMY", "NAKURU IX", 
           "HAYA TRIBE", "ELMENTEITA A"),
  longitude = c(36.2500, 29.8739, 38.3500, 23.0000, 36.0667, 31.5000, 36.2500),
  latitude = c(-0.4500, -1.9403, -3.4167, 1.0000, -0.2833, -1.5000, -0.4500)
)

west_africa = data.frame(
  name = c("DAHOMEY", "DOGON"),
  longitude = c(2.6035, -3.3390),
  latitude = c(6.4969, 14.4494)
)

northern_europe = data.frame(
  name = c("NORSE", "LAPP", "KYNDELOSE", "ERTEBOELLE", "BERG", "NORWAY", 
           "BORREB", "BORREBY", "KYNDELOESE"),
  longitude = c(8.4689, 23.6167, 12.5556, 8.7143, 5.3200, 8.4689, 10.4883, 11.4490, 12.5556),
  latitude = c(60.4720, 67.1500, 55.6770, 56.9560, 60.3900, 60.4720, 59.3910, 55.3490, 55.6770)
)

central_europe = data.frame(
  name = c("AVAR", "ZALAVAR", "MAGYAR", "PREDMOST III", "PREDMOST IV", "MLADEC 1"),
  longitude = c(19.5033, 17.1171, 19.5033, 17.9713, 17.9713, 17.0709),
  latitude = c(47.1625, 46.6453, 47.1625, 49.4786, 49.4786, 49.6961)
)

southern_europe = data.frame(
  name = c("PHILLIPI", "MUGE", "GRIMALDI", "NEOLITHIC"),
  longitude = c(24.2927, -8.7500, 7.4206, 15.0000),
  latitude = c(41.0151, 39.0500, 43.7333, 40.0000)
)

eastern_europe = data.frame(
  name = c("MARKINA GORA", "KURGAN #4", "KURGAN #3"),
  longitude = c(38.9000, 39.5000, 39.5000),
  latitude = c(57.8333, 50.4000, 50.4000)
)

western_europe = data.frame(
  name = c("CHANCELADE", "LA CHAPELLE", "TEVIEC 16", "TEVIEC 11", 
           "LA FERRASSIE I", "STEINHEIM", "CRO MAGNON 1", "ABRI PATAUD"),
  longitude = c(0.5000, 1.2300, -4.3700, -4.3700, 1.1858, 9.2500, 1.1700, 1.1500),
  latitude = c(45.0000, 45.0320, 48.3000, 48.3000, 44.9400, 48.7167, 44.0833, 44.9300)
)

western_asia = data.frame(
  name = c("SKHUL V", "DJEBEL QAFZEH 6", "NATUFIAN", "SHANIDAR 1", "IRAN", "HOTU #2"),
  longitude = c(35.0000, 35.6667, 35.5000, 44.1667, 53.6880, 54.0000),
  latitude = c(32.6667, 32.5000, 32.8333, 36.8333, 32.4279, 37.3833)
)

southern_asia = data.frame(
  name = c("MADRAS", "COLOMBO???", "VEDDA", "INDIAN"),
  longitude = c(80.2707, 79.8612, 81.0000, 78.0000),
  latitude = c(13.0827, 6.9271, 7.5000, 21.0000)
)

eastern_asia = data.frame(
  name = c('BURIAT', 'AINU', 'HAINAN', 'OROCHI', 'MOYORO I', 'ONKOROMANAI 1A', 'TSUKUMO #33', 
           'TSUKUMO #14', 'UKI', 'RYUKYUS', 'BUNUN', 'HAINAN ABORIGINAL', 'UPPER CAVE', 'N JAPAN', 
           'ANYANG', 'YAKUT', 'JAPANESE?', 'TAKASAGO #1', 'TSUKUMO #58', 'TSUKUMO #3', 
           'TSUKUMO #34', 'OTA', 'KOREA', 'FOLKLO', 'PAIWAN', 'S JAPAN', 'ANYANG', 
           'CHUKCHI', 'CHINESE', 'REBUNGE', 'OROK', 'TSUKUMO #27', 'TSUKUMO #4', 
           'YOSHIGO', 'KOFUN PERIOD', 'SOUTH JAPAN', 'TAIWAN ABORIGINAL', 'YAMI', 
           'YAYOI', 'ATAYAL', 'TAIWAN ABORIGINAL', 'MOYORO I.', 'BURIAT'),
  longitude = c(107.0000, 143.0000, 110.0000, 138.0000, 141.3500, 141.3500, 133.2333, 
                133.2333, 130.0000, 127.6830, 120.8830, 110.0000, 116.0000, 140.0000, 
                114.3054, 129.3000, 139.6917, 120.0000, 133.2333, 133.2333, 
                133.2333, 139.7000, 127.7669, 120.9675, 120.8330, 131.0000, 114.3054, 
                179.0000, 104.1954, 141.0330, 142.2500, 133.2333, 133.2333, 
                136.0000, 140.0000, 131.0000, 120.8830, 121.7500, 
                130.0000, 121.0000, 120.8830, 141.3500, 107.0000),
  latitude = c(52.0000, 45.0000, 19.0000, 52.5000, 44.0500, 43.0000, 34.6833, 
               34.6833, 32.0000, 26.0000, 23.5000, 19.0000, 40.0000, 40.0000, 
               36.0976, 62.0000, 36.2048, 23.0000, 34.6833, 34.6833, 
               34.6833, 35.7000, 35.9078, 24.0000, 23.0000, 32.0000, 36.0976, 
               66.0000, 35.8617, 45.4167, 53.0000, 34.6833, 34.6833, 
               34.0000, 36.0000, 32.0000, 23.5000, 22.0000, 
               33.0000, 24.0000, 23.5000, 44.0500, 52.0000)
)

southeast_asia = data.frame(
  name = c('DYAK', 'NEGRITO', 'BANTON I.', 'BURMA', 'MARINDUQUE, PULO TRES REY', 'ANDAMAN I', 'ANDAMANS', 'ANDAMAN ???', 'NICOBAR IS', 'ANDAMAN', 'ANDAMAN I?', 'ANDAMAN IS', 'MASBATE', 'ATCHINESE', 'BATTAK'),
  longitude = c(113.0000, 101.0000, 122.1000, 95.9956, 121.5000, 93.0000, 93.0000, 93.0000, 93.0000, 93.0000, 93.0000, 93.0000, 123.4000, 95.3000, 99.0000),
  latitude = c(1.0000, 15.0000, 12.6000, 21.9162, 13.5000, 13.0000, 13.0000, 13.0000, 8.0000, 13.0000, 13.0000, 13.0000, 12.3000, 5.5500, 2.3000)
)

australia = data.frame(
  name = c('TASMANIA', 'S MAORI', 'TASMANIAN ?', 'AUSTRALIAN', 'AUSTRALOID', 'MAORI', 
           'N MAORI', 'TASMANIAN', 'N.TERRITORY', 'AUSTRALI', 'CAIRNS DIST.', 'KEILOR', 'MORIORI'),
  longitude = c(146.3159, 172.6362, 146.3159, 133.7751, 133.7751, 172.6362, 174.8850, 
                146.3159, 133.7751, 133.7751, 145.7666, 144.9000, 176.5580),
  latitude = c(-42.0409, -41.2706, -42.0409, -25.2744, -25.2744, -41.2706, -41.2706, 
               -42.0409, -19.4914, -25.2744, -16.9203, -37.9000, -44.0000)
)

south_america = data.frame(
  name = c('U S NEGRO', 'TIER.D FUEGO',"PERU"),
  longitude = c(-55.0000, -68.7500, -75.0152),
  latitude = c(-10.0000, -54.8000, -9.1900)
)

north_america = data.frame(
  name = c('INDIAN KNOLL', 'SAN MIGUEL CHANNEL IS', 'PECOS PUEBLO', 'ARIKARA', 'SANTA CR', 'SANTA CRUZ'),
  longitude = c(-86.9667, -120.3678, -105.6828, -100.3510, -122.0308, -122.0308),
  latitude = c(37.4333, 34.0326, 35.5803, 47.0750, 37.3541, 37.3541)
)

pacific_islands = data.frame(
  name = c('SOLOMON IS', 'MARQUESAS', 'NEW HEBRIDES', 'GILBERT IS', 'SOCIETY IS', 'HERVEY IS', 
           'SAMOA', 'ROTA, MARIANAS', 'TOLAI', 'EASTER I', 'GUAM', 'NEW GUINEA', 'N CALEDONIA', 
           'FIJI', 'SANTA CRUZ IS', 'TINIAN MARIANAS', 'TUAMOTUS', 'MARIANAS', 'MOKAPU', 
           'BAINING', 'TAHITI', 'CAROLINE IS', 'TONGA', 'EASTER I.', 'NEW IRELAND', 'LOYALTY IS', 
           'VANUATU'),
  longitude = c(160.0000, -139.0000, 167.0000, 173.0000, -149.5000, -159.7731, 
                -172.0000, 145.2000, 152.0000, -109.3500, 144.7500, 147.0000, 165.5000, 
                179.0000, 166.6670, 145.6190, -140.0000, 145.7500, -157.8000, 
                151.8500, -149.4068, 150.5000, -175.0000, -109.3500, 152.5000, 167.2500, 
                168.0000),
  latitude = c(-9.6000, -10.0000, -15.5000, 1.2500, -17.5000, -19.2822, 
               -13.5833, 14.1360, -4.0000, -27.1167, 13.4443, -5.0000, -21.3000, 
               -17.7134, -10.7500, 15.0000, -18.0000, 15.0000, 21.4833, 
               -4.2167, -17.6509, 7.5000, -21.1789, -27.1167, -3.5000, -20.9167, 
               -16.0000)
)

atlantic_islands = data.frame(
  name = c('CANARY IS'),
  longitude = c(-15.6000),
  latitude = c(28.1000)
)

north_pole = data.frame(
  name = c('ESKIMO', 'CTRL ARCTIC'),
  longitude = c(-60.0000, 0.0000),
  latitude = c(75.0000, 90.0000)
)

# add a new col as a label
north_africa$region = 'North Africa'
south_africa$region = 'South Africa'
east_africa$region = 'East Africa'
west_africa$region = 'West Africa'
northern_europe$region = 'Northern Europe'
central_europe$region = 'Central Europe'
southern_europe$region = 'Southern Europe'
eastern_europe$region = 'Eastern Europe'
western_europe$region = 'Western Europe'
western_asia$region = 'Western Asia'
southern_asia$region = 'Southern Asia'
eastern_asia$region = 'Eastern Asia'
southeast_asia$region = 'Southeast Asia'
australia$region = 'Australia'
south_america$region = 'South America'
north_america$region = 'North America'
pacific_islands$region = 'Pacific Islands'
atlantic_islands$region = 'Atlantic Islands'
north_pole$region = 'North Pole'

whole_pop = rbind(north_africa, south_africa, east_africa, west_africa, northern_europe, central_europe, southern_europe, eastern_europe, western_europe, western_asia, southern_asia, eastern_asia, southeast_asia, australia, south_america, north_america, pacific_islands, atlantic_islands, north_pole)

library(maps)
library(ggplot2)
world_map = map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  geom_point(data = whole_pop, aes(x = longitude, y = latitude, color = region), size = 2) +
  scale_color_manual(values = c('North Africa' = 'limegreen', 'South Africa' = 'red', 'East Africa' = 'cyan', 
                                'West Africa' = 'sienna1', 'Northern Europe' = 'turquoise4', 'Central Europe' = 'grey52', 
                                'Southern Europe' = 'blue', 'Eastern Europe' = 'yellow2', 'Western Europe' = 'indianred', 
                                'Western Asia' = 'navy', 'Southern Asia' = 'darkgreen', 'Eastern Asia' = 'darkred', 
                                'Southeast Asia' = 'gold3', 'Australia' = 'deeppink', 'South America' = 'dodgerblue', 
                                'North America' = 'magenta', 'Pacific Islands' = 'lightskyblue3', 
                                'Atlantic Islands' = 'purple', 'North Pole' = 'black')) +
  theme_minimal() +
  labs(title = "Locations on World Map by Region", x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", 
        legend.box = "horizontal", 
        legend.box.just = "center") +
  guides(color = guide_legend(nrow = 4))



####################################### SVM with New Variable #######################################
# 1. create a new variable: Region
geo_classification = list(
  "North Africa" = c('EGYPT', 'NUBIA', 'AFALOU-BOU-RHUMMEL 5', 'JEBEL MOYA', 'AFALOU-BOU-RHUMMEL 9', 'DJEBEL IROUD 1', 'EGYPTIAN E?', 'AFALOU-BOU-RHUMMEL 9', 'DJEBEL IRHOUD 1'),
  "South Africa" = c('ZULU', 'HOTTENTOT', "WILLEY'S KOPJE III", 'BUSHMAN', 'BUSH-BANTU', 'FISH HOEK', 'BROKEN HILL I', 'FINGIRA 2', 'COLOURED', 'BUSHMAN?', "WILLEY'S KOPJE II", 'MATJES RIVER'),
  "East Africa" = c('ELMENTEITA B', 'TWA RWANDA', 'TEITA', 'PYGMY', 'NAKURU IX', 'HAYA TRIBE', 'ELMENTEITA A'),
  "West Africa" = c('DAHOMEY', 'DOGON'),
  "Northern Europe" = c('NORSE', 'LAPP', 'KYNDELOSE', 'ERTEBOELLE', 'BERG', 'NORWAY', 'BORREB', 'BORREBY', 'KYNDELOESE'),
  "Central Europe" = c('AVAR', 'ZALAVAR', 'MAGYAR', 'PREDMOST III', 'PREDMOST IV', 'MLADEC 1'),
  "Southern Europe" = c('PHILLIPI', 'MUGE', 'GRIMALDI', 'NEOLITHIC'),
  "Eastern Europe" = c('MARKINA GORA', 'KURGAN #4', 'KURGAN #3'),
  "Western Europe" = c('CHANCELADE', 'LA CHAPELLE', 'TEVIEC 16', 'TEVIEC 11', 'LA FERRASSIE I', 'STEINHEIM', 'CRO MAGNON 1', 'ABRI PATAUD' ),
  "Western Asia" = c('SKHUL V', 'DJEBEL QAFZEH 6', 'NATUFIAN', 'SHANIDAR 1', 'IRAN', 'HOTU #2'),
  "Southern Asia" = c('MADRAS', 'COLOMBO???', 'VEDDA', 'INDIAN'),
  "Eastern Asia" = c('BURIAT', 'AINU', 'HAINAN', 'OROCHI', 'MOYORO I', 'ONKOROMANAI 1A', 'TSUKUMO #33', 'TSUKUMO #14', 'UKI', 'RYUKYUS', 'BUNUN', 'HAINAN ABORIGINAL', 'UPPER CAVE', 'N JAPAN', 'ANYANG', 'YAKUT', 'JAPANESE?', 'TAKASAGO #1', 'TSUKUMO #58', 'TSUKUMO #3', 'TSUKUMO #34', 'OTA', 'KOREA', 'FOLKLO', 'PAIWAN', 'S JAPAN', 'ANYANG', 'CHUKCHI', 'CHINESE', 'REBUNGE', 'OROK', 'TSUKUMO #27', 'TSUKUMO #4', 'YOSHIGO', 'KOFUN PERIOD', 'SOUTH JAPAN', 'TAIWAN ABORIGINAL', 'YAMI', 'YAYOI', 'ATAYAL', 'TAIWAN  ABORIGINAL', 'MOYORO I.', 'BURIAT'),
  "Southeast Asia" = c('DYAK', 'NEGRITO', 'BANTON I.', 'BURMA', 'MARINDUQUE, PULO TRES REY', 'MASBATE', 'ATCHINESE', 'BATTAK', 'ANDAMAN I', 'ANDAMANS', 'ANDAMAN ???', 'NICOBAR IS', 'ANDAMAN', 'ANDAMAN I?', 'ANDAMAN IS'),
  "Australia" = c('TASMANIA', 'S MAORI', 'TASMANIAN ?', 'AUSTRALIAN', 'AUSTRALOID', 'MAORI', 'N MAORI', 'TASMANIAN', 'N.TERRITORY', 'AUSTRALI', 'CAIRNS DIST.', 'KEILOR', 'MORIORI'),
  "South America" = c('U S NEGRO', 'TIER.D FUEGO', 'PERU'),
  "North America" = c('INDIAN KNOLL', 'SAN MIGUEL CHANNEL IS', 'PECOS PUEBLO', 'ARIKARA', 'SANTA CR', 'SANTA CRUZ' ),
  "Pacific Islands" = c('SOLOMON IS', 'MARQUESAS', 'NEW HEBRIDES', 'GILBERT IS', 'SOCIETY IS', 'HERVEY IS', 'SAMOA', 'ROTA, MARIANAS', 'TOLAI', 'EASTER I', 'GUAM', 'NEW GUINEA', 'N CALEDONIA', 'FIJI', 'SANTA CRUZ IS', 'SANTA CRUZ IS', 'TINIAN MARIANAS', 'TUAMOTUS', 'MARIANAS', 'MOKAPU', 'BAINING', 'TAHITI', 'CAROLINE IS', 'TONGA', 'EASTER I.', 'NEW IRELAND', 'LOYALTY IS', 'VANUATU'),
  "Atlantic Islands" = c('CANARY IS'),
  "North Pole" = c('ESKIMO', 'CTRL ARCTIC'),
  "Unknown" = c('UNKNOWN', 'EUROPEAN?' )
)

data_original$Region = NA
data_test$Region = NA

for (pop in names(geo_classification)) {
  data_original$Region[data_original$Population %in% geo_classification[[pop]]] = pop
}
for (pop in names(geo_classification)) {
  data_test$Region[data_test$Population %in% geo_classification[[pop]]] = pop
}

data_original = data_original[data_original$Region != "Unknown", ] # delete unknown location data
data_test = data_test[data_test$Region != "Unknown", ]

# 2. data prepare
## (1) transfer data into factor
data_original$Sex = as.factor(data_original$Sex)
data_original$Region = as.factor(data_original$Region)
data_test$Sex = as.factor(data_test$Sex)
data_test$Region = as.factor(data_test$Region)
## (2) standardize all the numeric variables
prepare5 = preProcess(data_original[, !(names(data_original) %in% c("ID", "Sex", "Population", "Region"))], method = c("center", "scale"))
data_original_standardized = data_original
data_original_standardized[, !(names(data_original) %in% c("ID", "Sex", "Population", "Region"))] = predict(prepare5, data_original[, !(names(data_original) %in% c("ID", "Sex", "Population", "Region"))])
prepare6 = preProcess(data_test[, !(names(data_test) %in% c("ID", "Sex", "Population", "Region"))], method = c("center", "scale"))
data_test_standardized = data_test
data_test_standardized[, !(names(data_test) %in% c("ID", "Sex", "Population", "Region"))] = predict(prepare6, data_test[, !(names(data_test) %in% c("ID", "Sex", "Population", "Region"))])

# 3. SVM for gender
## (1) build the model
svm_sex3 = svm(Sex ~ . , data = data_original_standardized %>% select(-Population, -ID, -Region), kernel = "linear", cost = 1)
## (2) predict
sex_predictions3 = predict(svm_sex3, data_test_standardized %>% select(-Population, -ID, -Region))
## (3) confusion matrix
confusionMatrix(sex_predictions3, data_test$Sex)


# 4. SVM for region
## (1) build the model
svm_pop3 = svm(Region ~ . , data = data_original_standardized %>% select(-Sex, -ID, -Population), kernel = "linear", cost = 1)
## (2) predict the whole data set
pop_predictions3_common = predict(svm_pop3, data_test_standardized %>% select(-Population, -ID, -Sex))
all_levels = union(levels(data_test$Region), levels(pop_predictions3_common))
data_test$Region = factor(data_test$Region, levels = all_levels)
pop_predictions3_common = factor(pop_predictions3_common, levels = all_levels)
confusionMatrix(pop_predictions3_common, data_test$Region)










