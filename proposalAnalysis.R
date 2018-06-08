library(dplyr)

### Loading Training dataset ###
train <- read.csv("train.csv", stringsAsFactors = FALSE)

dim(train)
str(train)
summary(train)

### Loading Test Dataset ###
test <- read.csv("test.csv", stringsAsFactors = FALSE)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
str(test)
summary(test)

### Checking difference between test and training dataset ###
testNames <- colnames(test)
trainNames <- colnames(train)
setdiff(trainNames, testNames)

### Loading resources dataset ###
resources <- read.csv("resources.csv", stringsAsFactors = FALSE)
colnames(resources)
resources <- resources%>%mutate(TotalSpend = quantity*price)


### Checking difference of IDs between train, test and resource datasets ###
m1 <- setdiff(unique(resources$id),unique(train$id))
m2 <- setdiff(unique(test$id),m1)

### Left Joining Train and Resource dataset ###
merged_train <- merge(x=train, y = resources, by="id", all.x = TRUE )

### Finding total number of teachers ###
total_teachers <- length(unique(merged_train$teacher_id))
total_schools <- length(unique(merged_train$school_state))
teacher_in_schools <- merged_train%>%group_by(school_state)%>%summarise(no_of_teachers = n_distinct(teacher_id))
sum(teacher_in_schools$no_of_teachers)

### Looks like there are some teachers which belong to multiple college_states ###
teachers_in_schools <- merged_train%>%select(teacher_id, school_state)%>%group_by(teacher_id)%>%summarise(no_of_schools = n_distinct(school_state))

### Number of teachers in 1,2 and 3 schools respectively ###
no_of_teachers_in_schoolStates <- teachers_in_schools%>%group_by(no_of_schools)%>%summarise(No_of_teachers = n_distinct(teacher_id))

### Analysing Grades ###
grades_group <- unique(merged_train$project_grade_category)
## number of teachers teachomh every grade in every state ###
teachers_teachingGrades_every_state <- train%>%group_by(school_state,project_grade_category)%>%summarise(Count = n_distinct(teacher_id))
## Teachers teaching multiple grades over all##
teachers_teaching_grades <- train%>%group_by(teacher_id)%>%summarise(No_of_grades = n_distinct(project_grade_category))

### Analysing subjects and their sub categories ###
total_main_subjects <- unique(train$project_subject_categories)
total_sub_subjects <- unique(train$project_subject_subcategories)

### Number of approved projects ###
approved_projects <- train%>%filter(project_is_approved==1)
nrow(approved_projects)
## Looks like majority of the projects have been alloted money (84% of the projects are approved) ##
rejected_projects <- train%>%filter(project_is_approved==0)
nrow(rejected_projects)

#### Can we conclude that we will achieve atleast 84% accuracy in the training set only if i approve every application.
#### It is not a well balanced dataset though but also is not severly imbalanced(84% - 1 , 16% - 0)


#### Highest rejection rate for any school state is 3.23% which is not much. Probably we can drop school state here
# library(lubridate)
# months <- month(time_stamps)
# year <- year(time_stamps)
# time_of_day <- format(ymd_hms(merged_train$project_submitted_datetime), "%H:%M:%S")
# 
# part_of_day <- c()
# for(x in 1:nrow(merged_train)){
#   hr <- hour(merged_train$project_submitted_datetime[x])
#   if(hr>0 && hr<=5){
#     print("mid night")
#     part_of_day <- c(part_of_day,"mid night")
#   }else if(hr>5 && hr<=12){
#     print("morning")
#     part_of_day <- c(part_of_day,"morning")
#   }else if(hr>12 && hr<=17){
#     print("afternoon")
#     part_of_day <- c(part_of_day,"afternoon")
#   }else if(hr>17 && hr<=18){
#     print("evening")
#     part_of_day <- c(part_of_day,"evening")
#   }else{
#     print("night")
#     part_of_day <- c(part_of_day,"night")
#   }
#   
# }
#   

finalDf <- cbind(merged_train$project_is_approved ,months, year, time_of_day,part_of_day)
## Model Building
# 
# training_set[-3] = scale(training_set[-3])
# test_set[-3] = scale(test_set[-3])
# 


### EDA towards target variable

### There are 104414 teachers teaching in 51 schools

### Prefix Analysis towards acceptance
prefixes <- unique(merged_train$teacher_prefix)
no_of_married_females <- merged_train%>%filter(teacher_prefix=="Mrs.")%>%filter(project_is_approved==1)%>%nrow()
no_of_unmarried_females <- merged_train%>%filter(teacher_prefix=="Ms.")%>%filter(project_is_approved==1)%>%nrow()
no_of_males <- merged_train%>%filter(teacher_prefix=="Mr.")%>%filter(project_is_approved==1)%>%nrow()
no_of_dr <- merged_train%>%filter(teacher_prefix=="Dr.")%>%filter(project_is_approved==1)%>%nrow()
no_of_teachers <- merged_train%>%filter(teacher_prefix=="Teacher")%>%filter(project_is_approved==1)%>%nrow()
no_of_blank <- merged_train%>%filter(teacher_prefix=="")%>%filter(project_is_approved==1)%>%nrow()


no_of_married_females1 <- merged_train%>%filter(teacher_prefix=="Mrs.")%>%nrow()
no_of_unmarried_females1 <- merged_train%>%filter(teacher_prefix=="Ms.")%>%nrow()
no_of_males1 <- merged_train%>%filter(teacher_prefix=="Mr.")%>%nrow()
no_of_dr1 <- merged_train%>%filter(teacher_prefix=="Dr.")%>%nrow()
no_of_teachers1 <- merged_train%>%filter(teacher_prefix=="Teacher")%>%nrow()
no_of_blank1 <- merged_train%>%filter(teacher_prefix=="")%>%nrow()

prefix_table <- data.frame("Prefix"=c("Married Females", "Unmarried females", "Total Females", "Males","Dr.","Teachers", "Blank"), 
                           "Count"=c(no_of_married_females,no_of_unmarried_females,no_of_married_females+no_of_unmarried_females,no_of_males,no_of_dr,no_of_teachers,no_of_blank),
                           "Total_Submitted"=c(no_of_married_females1,no_of_unmarried_females1,no_of_married_females1+no_of_unmarried_females1,no_of_males1,no_of_dr1,no_of_teachers1,no_of_blank1))
acceptence_rate=c()
for(i in c(1:nrow(prefix_table))){
  acceptence_rate = c(acceptence_rate,(prefix_table$Count[i]/prefix_table$Total_Submitted[i])*100)
}
prefix_table_final <- cbind(prefix_table,acceptence_rate)

### Since proposal acceptance rate is same for almost all prefixes , WE CAN DROP PREFIX COLUMN

### School state analysis towards acceptance
school_state <- unique(merged_train$school_state)
school_state_table <- merged_train%>%group_by(school_state)%>%summarise("Total Count"=n())
school_state_table1 <- merged_train%>%filter(project_is_approved==1)%>%group_by(school_state)%>%summarise("Accepted Count"=n())
school_state_combined <- cbind(school_state_table,"Accepted Count"=school_state_table1$`Accepted Count`)
acceptence_rate_school <- c()
for(i in c(1:nrow(school_state_combined))){
  acceptence_rate_school=c(acceptence_rate_school,(school_state_combined$`Accepted Count`[i]/school_state_combined$`Total Count`[i])*100)
}
school_state_table_final <- cbind(school_state_combined,"Acceptence Rate"=acceptence_rate_school)
school_state_table_final <- school_state_table_final%>%arrange(-`Acceptence Rate`)
### Highest % acceptance is around 86% whereas lowest % acceptance is 72%. Since a difference can be observed in acceptance rate school state
### SHOULD BE TAKEN INTO CONSIDERATION
###

### School Grade analaysis towards acceptance rate
school_grades <- unique(merged_train$project_grade_category)
school_grade_table <- merged_train%>%group_by(project_grade_category)%>%summarise("Total Count"=n())
school_grade_table1 <- merged_train%>%group_by(project_grade_category)%>%summarise("Accepted Count"=n())
school_grade_combined <- cbind(school_grade_table,"Accepted Count"=school_grade_table1$`Accepted Count`)
acceptence_rate_schoolgrade <- c()
for(i in c(1:nrow(school_grade_combined))){
  acceptence_rate_schoolgrade=c(acceptence_rate_schoolgrade,(school_grade_combined$`Accepted Count`[i]/school_grade_combined$`Total Count`[i])*100)
}
school_grade_table_final <- cbind(school_grade_combined,"Acceptence Rate"=acceptence_rate_schoolgrade)
school_grade_table_final <- school_grade_table_final%>%arrange(-`Acceptence Rate`)

### Max projects were accepted for grade 3-5 and least for grade 9-12 but difference in accep % is not much.just 3%.
### CAN BE DROPPED INITIALLY BUT SHOULD BE CONSIDERED AFTER 1ST DRAFT OF THE MODEL
###

### Subject sub Category towards proposal selection
school_subjects <- unique(merged_train$project_subject_categories)
school_subject_table <- merged_train%>%group_by(project_subject_categories)%>%summarise("Total Count"=n())
school_subject_table1 <- merged_train%>%filter(project_is_approved==1)%>%group_by(project_subject_categories)%>%summarise("Accepted Count"=n())
school_subject_combined <- merge(x=school_subject_table, y = school_subject_table1, by="project_subject_categories", all.x = TRUE )
school_subject_combined[is.na(school_subject_combined)] <- 0
acceptence_rate_subjects <- c()
for(i in c(1:nrow(school_subject_combined))){
  acceptence_rate_subjects=c(acceptence_rate_subjects,(school_subject_combined$`Accepted Count`[i]/school_subject_combined$`Total Count`[i])*100)
}
school_subject_table_final <- cbind(school_subject_combined,"Acceptence Rate"=acceptence_rate_subjects)
school_subject_table_final <- school_subject_table_final%>%arrange(-`Acceptence Rate`)

### So max acceptance rate is for history and civics and least for warmth and hunger ... variation is quite enough
## SHOULD BE CONSIDERED FOR MAKING MODEL
###



### sub Subject Category towards proposal selection
school_sub_subjects <- unique(merged_train$project_subject_subcategories)
school_sub_subject_table <- merged_train%>%group_by(project_subject_subcategories)%>%summarise("Total Count"=n())
school_sub_subject_table1 <- merged_train%>%filter(project_is_approved==1)%>%group_by(project_subject_subcategories)%>%summarise("Accepted Count"=n())
school_sub_subject_combined <- merge(x=school_sub_subject_table, y = school_sub_subject_table1, by="project_subject_subcategories", all.x = TRUE )
school_sub_subject_combined[is.na(school_sub_subject_combined)] <- 0
acceptence_rate_sub_subjects <- c()
for(i in c(1:nrow(school_sub_subject_combined))){
  acceptence_rate_sub_subjects=c(acceptence_rate_sub_subjects,(school_sub_subject_combined$`Accepted Count`[i]/school_sub_subject_combined$`Total Count`[i])*100)
}
school_sub_subject_table_final <- cbind(school_sub_subject_combined,"Acceptence Rate"=acceptence_rate_sub_subjects)
school_sub_subject_table_final <- school_sub_subject_table_final%>%arrange(-`Acceptence Rate`)


## SHOULD BE CONSIDERED FOR MAKING MODEL
###

base_table <- data.frame("school_state"=merged_train$school_state,
                         "project_grade_category"=merged_train$project_grade_category,
                         "project_subject_categories"=merged_train$project_subject_categories,
                         "project_subject_subcategories"=merged_train$project_subject_subcategories,
                         "teacher_number_of_previously_posted_projects"=merged_train$teacher_number_of_previously_posted_projects,
                         "quantity"=merged_train$quantity,
                         "price"=merged_train$price,
                         "project_is_approved"=merged_train$project_is_approved)


library(caTools)
split = sample.split(base_table$project_is_approved , SplitRatio = 0.75)
training_set = subset(base_table, split == TRUE)
test_set = subset(base_table, split == FALSE)

training_set$price = scale(training_set$price)
training_set$quantity = scale(training_set$quantity)
training_set$teacher_number_of_previously_posted_projects=scale(training_set$teacher_number_of_previously_posted_projects)
#training_set1 <- data.frame(training_set$price, training_set$quantity, training_set$teacher_number_of_previously_posted_projects,training_set$project_is_approved)
#colnames(training_set1) <- c("price","quantity","previously_posted","approved")

test_set$price = scale(test_set$price)
test_set$quantity = scale(test_set$quantity)
test_set$teacher_number_of_previously_posted_projects=scale(test_set$teacher_number_of_previously_posted_projects)
#test_set1 <- data.frame(test_set$price, test_set$quantity, test_set$teacher_number_of_previously_posted_projects, test_set$project_is_approved)
#colnames(test_set1) <- c("price","quantity","previously_posted","approved")

training_set$project_subject_subcategories <- NULL
test_set$project_subject_subcategories <- NULL

##### Training and Testing logistic regression ####
model <- glm(training_set$project_is_approved ~ ., data = training_set, family = binomial)
#test_set <- data.frame("quantity"=merged_train$quantity[800001:length(merged_train$teacher_prefix)], "price"=merged_train$price[800001:length(merged_train$teacher_prefix)])
prob_pred = predict(model, type = 'response', newdata = test_set[,-8])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_act = test_set[,7]
results <- data.frame(y_act,y_pred)

cm = table(results[,1], results[,2])
cm
training_set$project_is_approved <- as.numeric(training_set$project_is_approved)
accurary = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
precision <- cm[2,2]/(cm[2,2]+cm[2,1])
recall <- cm[2,2]/(cm[2,2]+cm[1,1])

model_performance_logistic <- data.frame("Parameter"=c("Accuracy","Precision","Recall"),"Value"=c(accurary*100, precision*100, recall*100))
View(model_performance_logistic)


#### END #######




# #### Training and testing svm classifier #####
# library(e1071)
# classifier = svm(formula = training_set$project_is_approved ~ .,
#                  data = training_set,
#                  type = 'C-classification',
#                  kernel = 'linear')
# 
# prob_pred = predict(naive_classifier, type = 'response', newdata = test_set)
# y_pred = ifelse(prob_pred > 0.5, 1, 0)
# y_act = test_set[,7]
# results <- data.frame(y_act,y_pred)
# 
# cm = table(results[,1], results[,2])
# cm
# training_set$project_is_approved <- as.numeric(training_set$project_is_approved)
# accurary = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
# precision <- cm[2,2]/(cm[2,2]+cm[2,1])
# recall <- cm[2,2]/(cm[2,2]+cm[1,1])
# 
# model_performance_naive <- data.frame("Parameter"=c("Accuracy","Precision","Recall"),"Value"=c(accurary*100, precision*100, recall*100))
# View(model_performance_naive)
# ### END ####

### Traing Naive Byes###
library(e1071)

training_set$project_is_approved <- factor(training_set$project_is_approved)
naive_classifier = naiveBayes(x = training_set[-7],
                        y = training_set$project_is_approved)
prob_pred = predict(naive_classifier, newdata = test_set[-7])

y_act = test_set[,7]
results <- data.frame(y_act,prob_pred)

cm = table(results[,1], results[,2])
cm
training_set$project_is_approved <- as.numeric(training_set$project_is_approved)
accurary = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
precision <- cm[2,2]/(cm[2,2]+cm[2,1])
recall <- cm[2,2]/(cm[2,2]+cm[1,1])

model_performance_naive <- data.frame("Parameter"=c("Accuracy","Precision","Recall"),"Value"=c(accurary*100, precision*100, recall*100))
View(model_performance_naive)
### END ####


### Training with KNN ###
library(class)
training_set$project_is_approved <- factor(training_set$project_is_approved)
#training_set[-7] <- scale(training_set[-7])tr
training_set1 <- training_set[,-c(1:3)]
test_set1 <- test_set[,-c(1:3)]
training_set1$teacher_number_of_previously_posted_projects <- as.double(training_set1$teacher_number_of_previously_posted_projects)
training_set1$quantity <- as.double(training_set1$quantity)
training_set1$price <- as.double(training_set1$price)

y_pred = knn(train = training_set1[, -4],
             test = test_set1[, -4],
             cl = training_set1[ , 4],
             k = 3,
             l=2,
             prob = TRUE,
             use.all = TRUE)

y_act = test_set[,7]
results <- data.frame(y_act,prob_pred)

cm = table(results[,1], results[,2])
cm
training_set$project_is_approved <- as.numeric(training_set$project_is_approved)
accurary = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
precision <- cm[2,2]/(cm[2,2]+cm[2,1])
recall <- cm[2,2]/(cm[2,2]+cm[1,1])

model_performance_knn <- data.frame("Parameter"=c("Accuracy","Precision","Recall"),"Value"=c(accurary*100, precision*100, recall*100))
View(model_performance_knn)
## END ##



save.image("sample.RData")
