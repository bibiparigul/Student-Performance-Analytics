
# =========================
# 0. SETUP
# =========================
cat("\014")   # clear console
rm(list = ls())

# Install packages (run once only)
# install.packages(c("psych","corrplot","glmnet","factoextra"))

library(psych)
library(tidyverse)
install.packages("skimr")
library(skimr)


# =========================
# 1. LOAD DATA
# =========================
student_data <- read.csv("StudentPerformanceFactors.csv")

library(readxl)
student_data <- read.csv ("D:/Edu/Masters/semester 4/econometrics/porject/StudentPerformanceFactors.csv")

# =========================
# 2. View Dataset Structure
# =========================
# First rows
head(student_data)

# Structure of dataset
str(student_data)

# Dimensions
print(dim(student_data))

# Variable names
names(student_data)

# ======================================
# 3. Categorical Variables into Factors
# ======================================
student_data$Gender <- as.factor(student_data$Gender)
student_data$School_Type <- as.factor(student_data$School_Type)
student_data$Motivation_Level <- as.factor(student_data$Motivation_Level)
student_data$Family_Income <- as.factor(student_data$Family_Income)
student_data$Teacher_Quality <- as.factor(student_data$Teacher_Quality)
student_data$Peer_Influence <- as.factor(student_data$Peer_Influence)
student_data$Distance_from_Home <- as.factor(student_data$Distance_from_Home)
student_data$Parental_Involvement <- as.factor(student_data$Parental_Involvement)
student_data$Parental_Education_Level <- as.factor(student_data$Parental_Education_Level)
student_data$Internet_Access <- as.factor(student_data$Internet_Access)
student_data$Extracurricular_Activities <- as.factor(student_data$Extracurricular_Activities)
student_data$Learning_Disabilities <- as.factor(student_data$Learning_Disabilities)
student_data$Access_to_Resources <- as.factor(student_data$Access_to_Resources)

# =========================
# 4. DATA CLEANING
# =========================
# Missing values per variable
colSums(is.na(student_data))

# Total missing values
sum(is.na(student_data))

# =========================
# 5. DESCRIPTIVE STATISTICS
# =========================
# Numerical summary
summary(student_data)

# Detailed descriptive statistics
psych::describe(student_data)

# Frequency tables
table(student_data$Gender)
table(student_data$School_Type)
table(student_data$Motivation_Level)
table(student_data$Family_Income)

# =========================
# 6. VISUALIZATION
# =========================
hist(student_data$Exam_Score,
     main = "Histogram of Exam Scores",
     xlab = "Exam Score")

boxplot(student_data$Exam_Score,
        main = "Boxplot of Exam Score")

plot(student_data$Hours_Studied,
     student_data$Exam_Score,
     main = "Hours Studied vs Exam Score",
     xlab = "Hours Studied",
     ylab = "Exam Score")

plot(student_data$Attendance,
     student_data$Exam_Score,
     main = "Attendance vs Exam Score",
     xlab = "Attendance",
     ylab = "Exam Score")

plot(student_data$Previous_Scores,
     student_data$Exam_Score,
     main = "Previous Scores vs Exam Score",
     xlab = "Previous Scores",
     ylab = "Exam Score")

# =========================
# 7. CORRELATION
# =========================
install.packages("corrplot")
library(corrplot)
numeric_data <- student_data %>%
  select_if(is.numeric)

cor_matrix <- cor(numeric_data)
print(round(cor_matrix, 2))

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = NULL)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black")

# =========================
# 8. Data Cleaning 2 
# =========================
student_data[student_data == ""] <- NA
colSums(is.na(student_data))
student_data <- na.omit(student_data)
dim(student_data)

# ===========================
# 9. Linear Regression Model 
# ===========================
lm_model <- lm(Exam_Score ~ Hours_Studied +
                 Attendance +
                 Previous_Scores +
                 Sleep_Hours +
                 Tutoring_Sessions +
                 Physical_Activity,
               data = student_data)
summary(lm_model)
lm_model2 <- lm(Exam_Score ~ Hours_Studied +
                  Attendance +
                  Previous_Scores +
                  Tutoring_Sessions +
                  Physical_Activity,
                data = student_data)

summary(lm_model2)

# ===============================
# 10. Multicollinearity Test (VIF) 
# ===============================
install.packages("car")
install.packages("lmtest")
library(car)
library(lmtest)

vif(lm_model2)
hist(residuals(lm_model2),
     main = "Residual Histogram",
     xlab = "Residuals")
qqnorm(residuals(lm_model2))
qqline(residuals(lm_model2))
shapiro.test(residuals(lm_model2))
bptest(lm_model2)
dwtest(lm_model2)

plot(lm_model2$fitted.values,
     residuals(lm_model2),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")

abline(h = 0, col = "red")

#Cook’s Distance
plot(cooks.distance(lm_model2),
     type = "h",
     main = "Cook's Distance")
# =========================
# 11. Logistic Regression 
# =========================
student_data$Pass_Fail <- ifelse(student_data$Exam_Score >= 67, 1, 0)
student_data$Pass_Fail <- as.factor(student_data$Pass_Fail)
table(student_data$Pass_Fail)
log_model <- glm(Pass_Fail ~ Hours_Studied +
                   Attendance +
                   Previous_Scores +
                   Tutoring_Sessions +
                   Physical_Activity,
                 data = student_data,
                 family = binomial)
summary(log_model)
exp(coef(log_model))

predicted_prob <- predict(log_model, type = "response")

predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

table(Predicted = predicted_class,
      Actual = student_data$Pass_Fail)
#POC Curve
install.packages("pROC")
library(pROC)
roc_curve <- roc(student_data$Pass_Fail,
                 predicted_prob)

plot(roc_curve)
auc(roc_curve)

# =========================
# 12. cluster Analysis 
# =========================
install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)

cluster_data <- student_data[, c(
  "Hours_Studied",
  "Attendance",
  "Sleep_Hours",
  "Previous_Scores",
  "Tutoring_Sessions",
  "Physical_Activity",
  "Motivation_Level",
  "Family_Income",
  "Teacher_Quality",
  "Exam_Score"
)]

str(cluster_data)

#Compute Gower Distance Matrix
gower_dist <- daisy(cluster_data,
                    metric = "gower")
#Hierarchical Clustering
hc_model <- hclust(gower_dist,
                   method = "ward.D2")

plot(hc_model,
     labels = FALSE,
     main = "Hierarchical Clustering Dendrogram")

#No of clusters
clusters <- cutree(hc_model, k = 3)
student_data$Cluster <- as.factor(clusters)
table(student_data$Cluster)
aggregate(cbind(Hours_Studied,
                Attendance,
                Previous_Scores,
                Tutoring_Sessions,
                Exam_Score) ~ Cluster,
          data = student_data,
          mean)
#cluster visualization
fviz_cluster(list(data = scale(student_data[, c(
  "Hours_Studied",
  "Attendance",
  "Previous_Scores",
  "Exam_Score"
)]),
cluster = clusters))