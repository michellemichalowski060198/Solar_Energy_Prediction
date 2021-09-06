####################################
#########GROUP ASSIGNMENT###########
####################################

#define the paths 

station_info_path <- "/Users/cdalenbrook/Documents/MBD/R Programming/Group Project/project(1)/station_info.csv";
solar_data_path <- "/Users/cdalenbrook/Documents/MBD/R Programming/Group Project/project(1)/solar_dataset.RData";
additional_data_path <- "/Users/cdalenbrook/Documents/MBD/R Programming/Group Project/project(1)/additional_variables.RData";


#required packages
library(DataExplorer)
library(inspectdf)
library(psych)
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(Hmisc)
library(tidyr)
library(ggplot2)
library(leaflet)
library(missForest)
library(gbm)
library(foreach)
library(doParallel)
library(ggpubr)
library(Amelia)
library(data.table)


#load the data 
solar_data <- readRDS(solar_data_path);
additional_data <- readRDS(additional_data_path);

#check if data frame 
is.data.frame(solar_data)
is.data.frame(additional_data)

#join data 
data <- cbind(solar_data, additional_data)

#prepare data for following steps by excluding the weather stations
data_EDA <- data[,-c(0:98)]
data_EDA$Date <- NULL

####################################
#####Exploratory Data Analysis######
####################################

#overall overview
DataExplorer::create_report(data[,c(2:98)])
#reference: https://boxuancui.github.io/DataExplorer/


inspect_cor(data_EDA[,c(1:50)])
inspect_num(data_EDA[,c(1:50)])

#how does the data look like?
str(data)

sapply(data, class)

#####################################
#descriptive statistics

#compute descriptive statistics for each column with pych package
descriptive_statistics <- describe(data_EDA)

#give rownames a column 
descriptive_statistics <- cbind(variable_names = rownames(descriptive_statistics), descriptive_statistics)

rownames(descriptive_statistics) <- NULL

ranges <- descriptive_statistics %>%
            select(variable_names, range)
  
  
#visualize the ranges with ggplot2

ggplot(data=ranges, aes(x=range)) +
  geom_histogram(aes(y=..density..), bins = 30, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(range)),
             color="red", linetype="dashed", size=1) +
  theme_minimal()

#we see that there a few observations with a really high range 
#we might need to normalize that later depending on the 
#algorithm we choose

descriptive_statistics_dependents <- describe(data[,c(2:98)])

descriptive_statistics_dependents <- cbind(variable_names = rownames(descriptive_statistics_dependents),
                                           descriptive_statistics_dependents)
#####################################
#compute all correlations

#correlations in between variables
correlations <- rcorr(as.matrix(data_EDA))
correlations

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

table_correlations <- flattenCorrMatrix(correlations$r, correlations$P)


is.matrix(table_correlations)

is.data.frame(table_correlations)

##format the data frame 
table_correlations <- table_correlations %>%
  mutate(cor = round(cor,2),
         p = round(p,2)) 

#create table with very high correlations
table_high_correlation <- table_correlations %>%
  arrange(desc(cor)) %>%
  select(row, column, cor) %>%
  top_n(10)
table_high_correlation

#create table with very low correlations

table_low_correlation <- table_correlations %>%
  arrange(desc(cor)) %>%
  select(row, column, cor) %>%
  top_n(10)


#correlations with dependent variable
correlations_with_dep <- rcorr(as.matrix(data[c(1:5113),]))

#table with correlations
table_correlations_dep <- flattenCorrMatrix(correlations_with_dep$r, correlations_with_dep$P)

#table with high correlations 
table_low_correlation_dep <- table_correlations_dep %>%
  arrange(desc(cor)) %>%
  filter(grepl('^PC', column) == TRUE) %>%
  select(row, column, cor) %>%
  top_n(100)

#table with low correlations 
table_low_correlation_dep <- table_correlations_dep %>%
  arrange(cor) %>%
  filter(grepl('^PC', column) == TRUE) %>%
  select(row, column, cor) %>%
  top_n(100)

#####################################
#outliers
FindOutliers <- function(data) {
  lowerq = quantile(data, na.rm=TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
  length(result)
}
sapply(data, FindOutliers)

#example of the dependent variables 
(boxplot(data[,c(2:5)])$out)

#example of the pre-processed features 
(boxplot(data[,c(110:115)])$out)

#example of the additional data 
(boxplot(data[,c(500:505)])$out)

# for now we leave the outliers in the data set 
# might delete them later on in the script for 
# the ML algorithm input

#####################################
#visualize distributions of columns

#dependent variables
data[c(1:5113),c(2:13)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(14:25)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(26:37)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(38:49)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(50:61)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(62:73)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(74:85)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 

data[c(1:5113),c(86:99)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(value)),
             color="black", linetype="dashed", size=1) +
  theme_minimal() 


#independent variables 

first_10 <- data[,c(100:110)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  theme_minimal()                         

second_10 <- data[,c(111:120)] %>%                   
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +   
  geom_density(alpha=.2, fill="#FF6666") +
  theme_minimal()

#too many to visualize all of them 
#but helped to get a general understanding -> independent variables look normally distributed


#####################################
#QQ plots

#check how normally dsitributed the data is 
#The Q-Q plot is a graph that can be used to test a variable 
#for the presence of a normal distribution.

#we won't run a QQ plot for each column because there are too many 
data_QQ <- data[c(1:5113),c(3:11)]
par(mfrow=c(3,3))
for (i in 1:ncol(data_QQ)){
                # [,1: ncol(data_QQ)])){  
  qqnorm(data_QQ[[i]])
  qqline(data_QQ[[i]])
}

#example for Rmarkdown 
qqnorm(data$PC12)
qqline(data$PC12)


#####################################
#visualize on a map

#visualization of weather stations on a map with leaflet

stations <- read.csv(station_info_path, header = TRUE, sep = ",");

map <- leaflet(stations) %>% addTiles() %>% 
  setView(-98.574020, 35.408600, zoom = 5.5) %>% 
  addCircles(~elon, ~nlat, weight = 3, radius=40, 
             color="#0000FF", stroke = TRUE, fillOpacity = 0.8);
map;


#####################################
#time series visualization  
solar_data_timeseries <- solar_data[1:400,1:99]
solar_data_timeseries$Date <- as.Date.character(solar_data_timeseries$Date, format = c("%Y%m%d"))
ggplot(solar_data_timeseries, aes(x = Date, y = ACME))  + 
  geom_line(col="black") +
  scale_x_date(date_labels = "%b") +
  theme_minimal() +
  labs(title = "Time series", 
       x = "Date", 
       y = "Energy production") 
##################################### 
#impute missing values 

#count the NA's per column
na_count <- sapply(data_EDA, function(y) sum(length(which(is.na(y)))))

na_count <- as.data.frame(na_count)

na_count %>%
  arrange(desc(na_count))

#there are only NA's in the additonal variables

#visualize it
missmap(additional_data, col=c("red", "grey"))

na_count <- cbind(variable_names = rownames(na_count), na_count);
na_count;
rownames(na_count) <- NULL

#keep only the variables that have NA values
na_count_v2 <- na_count[grepl('^V', na_count[,"variable_names"]),]
na_count_v2;

#impute the missing values using missForest
data_EDA <- missForest(data_EDA, ntree=10);

#check if any NA's still in columns
na_count <- sapply(data_EDA, function(y) sum(length(which(is.na(y)))))

na_count <- as.data.frame(na_count)

na_count %>%
  arrange(desc(na_count))


####################################
#####Kaggle Challenge###############
####################################

#prediction per weather station per date 

library(data.table);
library(gbm);
library(foreach);
library(doParallel);

set.seed(123); 

#Make a column with seasons so that the date column can be used
solar_data$Month <- month(as.POSIXct(solar_data$Date, format="%Y%m%d"));

#SPLIT DATA INTO TRAIN, TEST & VAL SET (USED IN FINAL KAGGLE SUBMISSION)
solar_data_train <- solar_data[1:5113,2:457];
dim(solar_data_train)

# NOT DONE AS IT WORSENED RESULTS
# remove outliers from training data (all vals greater or less than 3*s.d. from mean get taken out)
#dim(solar_data_train)
#solar_data_train <- solar_data_train[!apply(sapply(solar_data_train, function(x) abs(scale(x)) >= 3), 1, any), ]
#dim(solar_data_train)

#calculate amount of training data (0.9% of training data)
num_train <- as.integer(nrow(solar_data_train)*0.9);
train_index <- sample(1:num_train, num_train);  

#split training data into train and test sets
train <- solar_data_train[train_index,];
dim(train)
test <- solar_data_train[-train_index,];
dim(test)

#define the validation data (i.e. rows where weather stations are null)
validation <-solar_data[5114:6909,];
dim(validation)


### DIFFERENT SPLITTING (include additional data - not used in final kaggle submission)
combined_data <- cbind(solar_data[1:5113,2:457], additional_data[1:5113,])
dim(combined_data)
colnames(combined_data)

train_index <- sample(1:4600, 4600);

train_combined <- combined_data[train_index,];
dim(train_combined)
test_combined <- combined_data[-train_index,];
dim(test_combined)

validation_combined <-combined_data[5114:6909,];
dim(validation_combined)


#CREATE ERROR FUNCTION MAE 
MAE <- function(pred_vals, true_vals){
  n <- length(pred_vals);
  diff_sum <- 0;
  for(i in 1:n){
    diff_sum <- diff_sum + abs(pred_vals[i] - true_vals[i]);
  }
  MAE <- diff_sum/n;
  return(MAE);
}


#Test with one column that everything is working as intended 
model <- gbm(ACME ~ PC1+PC2+PC3,distribution="laplace", data=train, n.trees=100, interaction.depth =6, shrinkage=0.05)	
y_pred <- predict.gbm(model, test, n.trees=100)
MAE(y_pred, test[,"ACME"])


#create formulas used in the the for each loop 
colnames(solar_data[,100:457])

PCA_cols_formula <- paste(colnames(solar_data[,100:457]),collapse="+");
PCA_cols_formula <- paste0("~", PCA_cols_formula);

PCA_plus_additional_formula <- paste(colnames(combined_data[,99:457]),collapse="+");
PCA_plus_additional_formula <- paste("~", PCA_plus_additional_formula);

### PARALLELIZE MODEL GENERATION
registerDoParallel(cores = detectCores());

###hyperparameter tuning
interaction_depth <- seq(from = 1, to = 10, by=1);
shrinkage <- seq(from= 0, to = 1, by = 0.05;

####use train_combined and validation_combined if you want to use extra data too & REPLACE PCA COLUMNS FORMULA TO 
grid_results <- foreach(i=colnames(train[,1:98]), .combine = rbind) %dopar%{
  formula <- as.formula(paste0(i, PCA_cols_formula));
  model <- gbm(formula, distribution="laplace", data=train, n.trees=1000, interaction.depth=6, shrinkage=0.05)	
  #USED DURING MODEL CREATION/TUNING:
  #predictions_test <- predict.gbm(model, test, n.trees=1000);
  #MAE_val <- MAE(predictions_test, test[,1:98]);
  predictions_val<- predict.gbm(model, validation, n.trees=1000)
}

grid_results <- as.data.frame(grid_results);
grid_transpose <- transpose(grid_results);

dim(grid_transpose);
colnames(grid_transpose) <- colnames(solar_data[,2:99]);

predictions_df <- cbind(solar_data[5114:6909,1], grid_transpose);

dim(predictions_df);

write.csv(predictions_df,"/Users/cdalenbrook/Documents/MBD/R Programming/Group Project/project(1)/results3.csv", row.names = FALSE)


stopCluster();


