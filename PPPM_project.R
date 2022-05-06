library(readxl)
library(dplyr)
library(tidyr)
library(survival)
library(survminer)
library(ConsensusClusterPlus)

Bonn_Radiology_rawdata<-read_excel("Final table_big data project_Radiology Bonn.xlsx")

Bonn_data<-Bonn_Radiology_rawdata[,c(5,9,10,12,14:16,26,31,40:44,67,73,79,85,91,97)]
# Reorder the columns such that categorical attributes are in the first part and 
# Continous variables are in the middle and the classiffier(Survival) is the last column
Bonn_data<-Bonn_data[,c(1:4,7:20,5:6)]

#### PREPROCESSING #####

# Replace NA with 0 in categorical variables
na.zero <- function (x) {
  x[is.na(x)] <- 0
  x}
Bonn_data[,2:3]<-lapply(Bonn_data[,2:3],na.zero)
# Refactor the gender to '0' for Female and '1' for Males
Bonn_data$Gender<-factor(Bonn_data$Gender,levels = c("F","M"),labels = c("0","1"))
# Refactor the Systemisce.Erk such that patinets with no Sys.Erk will be signed to '0'
# and patients with one or more additional diseases will be signed to '1'
Bonn_data$Systemic_Diseases<-as.factor(ifelse(Bonn_data$Systemic_Diseases==0,0,1))
# Convert other columns to factors 
Bonn_data$Therapy<-as.factor(Bonn_data$Therapy-1)
Bonn_data$Primary_Tumor <-as.factor(Bonn_data$Primary_Tumor)
# Convert columns to numeric
cols_num<-c(8:12)
Bonn_data[cols_num] <- sapply(Bonn_data[cols_num],as.numeric)

Bonn_data<-as.data.frame(Bonn_data)


##### SUBGROUPS DIVISION #####

# Create subgroups of tumors type seperately
HCC_data<-filter(Bonn_data,Primary_Tumor == 1)
CRC_data<-filter(Bonn_data, Primary_Tumor == 2)
BC_data<-filter(Bonn_data, Primary_Tumor == 3)
# create sub table with the 3 tumors together "filtered"
filtered_data<-bind_rows(HCC_data, CRC_data, BC_data)

##### OUTLIERS #####
#General finger rule about outliers. You need to have a good reason to change
# an outlier, because any change has an underlined assumption. So either you
# can omit an outlier or you should have a good explanation why you changed it.
# any case, If there are more than 1-2% ountliers, I would suspect it is essential
# and leave it. 

#This function was taken from the net. it finds outliers and plot them,
#and can replace them with NA if decided. 
#Just change dt with the data and var with the variable
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


############################## PLOTSSS #############################  
# Plotting function for ploting all the variables in a data set. Note that
# you need to specify the categorical and continous columns. 
plot_the_data <- function(df,color){
  #specify categorical variables
  categoric_df<-df[,c(1:4)] 
  #specify continous variables
  contious_df<-df[,c(5:19)]
  for(i in 1:length(categoric_df)) {
    plot(categoric_df[,i],main = names(categoric_df[i]),col = color)
  }
  contious_df<-as.data.frame(contious_df)
  for(i in 1:length(contious_df)){
    hist(contious_df[,i],main = names(contious_df[i]),
         xlab = names(contious_df[i]),col=color)}
}

#plot all the data
plot_the_data(Bonn_data, "gray")
#plot each tumor group
plot_the_data(HCC_data,"blue")
plot_the_data(CRC_data, "brown")
plot_the_data(BC_data,"green")
plot_the_data(filtered_data,"yellow")

# keep all the variables names here so it is east to cope
vars<- c("Gender","Primary_Tumor","Systemic_Diseases", "Therapy",
         "Age", "MMP_9","MMP_2","I_normalized", "II_normalized",
         "III_normalized","IV_normalized", "IV_III_before_therapy","Calgranulin",
         "Catalase","Profilin","RhoA",
         "SOD2","Trx")

# plot single variables
single_var <- data.frame(
  Gender = factor(c("Female","Male"), levels=c("Female","Male")),
  Occurrences = c(65, 93)
)
ggplot(data=single_var, aes(x=Gender, y=Occurrences, fill = Gender)) +
  geom_bar(colour = "black",stat="identity") +guides(fill=FALSE)+
  ggtitle("Gender") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram and density line
ggplot(data=Bonn_data, aes(Bonn_data$Survival)) + 
  geom_histogram(aes(
#    y =..density..
    ), 
                 breaks=seq(0, 65, by = 5), 
                 col="black", 
                 fill="royalblue2", 
                 alpha=.5) + 
  geom_density(col=2) + 
  geom_vline(xintercept = c(8,13), size = 1, colour = c( "red", "brown"),
             linetype = "dashed")+
  labs(x= "Time (months)")+
  ggtitle("Survival") +
  theme(plot.title = element_text(hjust = 0.5))

# Pie chart
df.DiseaseType = data.frame("Disease" = c("Hepatocellular_Carcinoma",
                                          "Metastatic_Colorectal_Cancer",
                                          "Metastatic_Breast_Cancer", "Other_Metastases"),
                "Patients" = c(57,51,15,35))
ggplot(df.DiseaseType, aes(x="", y=Patients, fill=Disease))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))

# Single scatter plot
library("ggpubr")
ggscatter(Bonn_data, x = "Survival", y = "I_normalized",
            add = "reg.line",                         # Add regression line
            conf.int = FALSE,                          # Add confidence interval
            #color = "surv_group",
          #palette = "jco",   # Color by groups 
          #shape = "Therapy" # Change point shape by groups
          )+     
    stat_cor(aes(color = Therapy), label.x = 35) 

# Multiple scatter plots against few variables
Bonn_data %>%
  gather(-Calgranulin, -Therapy, key = "var",
         value = "value") %>%
  ggplot(aes(x = value, y = Calgranulin, color = Therapy)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

### Multiple box plots against one variable ####
library(reshape2)
library(data.table)
#normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# df<-hc_data[hc_data$Therapy == 1 & hc_data$Primary_Tumor == 1 & hc_data$cluster == 3,]
df<-hc_data
# normalize all the biomarkers
for (i in 6:16) {
  df[i] <- normalize(df[i])
}

my_comparisons <- list( c("1", "2"),c("2", "3"),c("1", "3"))
setnames(df,c('MMP_9','MMP_2','I_normalized','III_normalized','IV_normalized','Calgranulin'), c('MMP 9','MMP 2',"Comet class I", "Comet class III", "Comet class IV", 'Calgranulin A'), skip_absent = TRUE)

df.m <- melt(df[c(19,6:16)], id.var = "cluster")

# Pairwise wilcox-test between groups
library(rstatix)
stat.test <- na.omit(df.m) %>%
  group_by(variable) %>%
  wilcox_test(value ~ cluster) %>%
  adjust_pvalue(method = "BH") %>%
  mutate(y.position = 35)
stat.test$p.adj <- format(stat.test$p.adj, digits = 3)
stat.test

# make the plot
ggplot(data = df.m, aes(x=cluster, y=value, xlab(size=15))) + 
  geom_boxplot() +
  facet_wrap(~variable,ncol = 4) +
  ylim(0,1.4) +
  # ggtitle("Distribution of the biomarker-patterns in HCC patients of best survival group (Nr. 3), who underwent TACE") +
  ggtitle("Distribution of the biomarker-patterns stratified by Consensus Clustering",) +
  ylab("Biomarker level") +
  xlab("Groups stratified by Consensus Clustering") +
  stat_pvalue_manual(stat.test, label = "p.adj",size = 3.1, y.position = c(1.07,1.2,1.35)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.2, "lines"),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(face = "bold"))

  
##### PCA TSNE ############
#pca
set.seed(222)
pairs(Bonn_data[,c(5:12,19)],gap=1/10, col = Bonn_data$Primary_Tumor, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("0","1","Cv2","Cv3"), pch = 16, col = c("black","red","green","blue"),xpd = T, cex = 2, y.intersp = 0.5)
#pca
Bonn_PCA<-filtered_data[complete.cases(filtered_data),]
Bonn_PCA <- prcomp(scale(Cox_data[6:17]))
plot(Bonn_PCA$x, col = Cox_data$cluster)

#tsne
set.seed(123)
library(Rtsne)
bonn_tsne<-Cox_data
bonn_tsne$Gender<-as.numeric(bonn_tsne$Gender)
bonn_tsne$Therapy<-as.numeric(bonn_tsne$Therapy)-1
bonn_tsne$Systemic_Diseases<-as.numeric(bonn_tsne$Systemic_Diseases)
bonn_tsne<-bonn_tsne[complete.cases(bonn_tsne),]
Bonn_uniqe<- unique(bonn_tsne)
tsne_out<-Rtsne(Bonn_uniqe[6:17],pca=FALSE,perplexity=3,theta= 0.0)
plot(tsne_out$Y, col = factor(Bonn_uniqe$cluster), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Primary_Tumor), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Systemic_Diseases), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Therapy), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Age), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$MMP_9), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$MMP_2), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$I_normalized), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$II_normalized), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$III_normalized), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$IV_normalized), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$IV_III_before_therapy), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Calgranulin), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Catalase), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Profilin), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$RhoA), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$SOD2), asp = 1)
plot(tsne_out$Y, col = factor(Bonn_uniqe$Trx), asp = 1)


############# KAPLAN MEIER SURVIVAL PLOT #########################
#First clean the environment, then load the libraries exept from 
#ggplot2, then run manually the first bimatker with the group
#you are interested (it stays fix to the size of the specific group)

# Function to run and plot Kaplan Meier
KM_func <- function(data,biomarker_att,title){
  #find median of the biomarker column
  BioM_median<-median(biomarker_att, na.rm = T) #HOW TO TREAT NA'S?
  # Create and add new column to data 'BioMarker_High_Low'
  BioMarker_High_Low<-ifelse(biomarker_att > BioM_median ,"High","Low")
  ###q<-quantile(biomarker_att, c(.33, .66),na.rm = T)
  ###BioMarker_High_Low<-ifelse(biomarker_att > q[2],"High", ifelse(biomarker_att > q[1],"Middle","Low"))
  data$BioMarker_High_Low<-factor(BioMarker_High_Low)
  #Create Kaplan Meier model
  surv_object<-Surv(time = data$Survival
                    ,event = data$event)
  fit1 <- survfit(surv_object ~ BioMarker_High_Low, data = data)
  #summary(fit1)
  ggsurv <- ggsurvplot(fit1, data = data, pval = TRUE, title=title,
             legend = "right", legend.labs = c("High","Low"))
  ggsurv$plot + theme(plot.title = element_text(hjust = 0.5, size = 20))
}

#create single kaplan meier without a function
  data<-Bonn_data
  biomarker_att<-data$Therapy
  title<-"Therapy"
  #find median of the biomarker column
  BioM_median<-median(biomarker_att, na.rm = T) #HOW TO TREAT NA'S?
  # Create and add new column to data 'BioMarker_High_Low'
  BioMarker_High_Low<-ifelse(biomarker_att > BioM_median ,"High","Low")
  ## find 33 and 66 percentile 
  #q<-quantile(biomarker_att, c(.33, .66),na.rm = T)
  #BioMarker_High_Low<-ifelse(biomarker_att > q[2],"High",
  #                           ifelse(biomarker_att > q[1],"Middle","Low"))
  data$BioMarker_High_Low<-factor(BioMarker_High_Low)
  #Create Kaplan Meier model
  surv_object<-Surv(time = data$Survival
                    ,event = data$event)
  fit1 <- survfit(surv_object ~ Therapy, data = data)
  #summary(fit1)
  ggsurv<- ggsurvplot(fit1, data = data, pval = TRUE,title = title, xlab = "Time (months)",legend = "right",legend.labs = c("SIRT", "TACE"))
  ggsurv$plot + theme(plot.title = element_text(hjust = 0.5, size = 20))
#specify data
KM_data<-Bonn_data
#run Kaplan Meier for each biomarkers
KM_func(KM_data,KM_data$Catalase, "Catalase")
KM_func(KM_data,KM_data$Calgranulin, "Calgranulin")
KM_func(KM_data,KM_data$Profilin, "Profilin")
KM_func(KM_data,KM_data$RhoA, "RhoA")
KM_func(KM_data,KM_data$SOD2, "SOD2")
KM_func(KM_data,KM_data$Trx, "Trx")
KM_func(KM_data,KM_data$I_normalized, "Class I CA")
KM_func(KM_data,KM_data$II_normalized, "Class II CA")
KM_func(KM_data,KM_data$III_normalized, "Class III CA")
KM_func(KM_data,KM_data$IV_normalized, "Class IV CA")
KM_func(KM_data,KM_data$IV_III_before_therapy, "IV_III_before_therapy")
KM_func(KM_data,as.numeric(KM_data$MMP_9), "MMP_9")
KM_func(KM_data,as.numeric(KM_data$MMP_2), "MMP_2")
KM_func(KM_data,KM_data$Age, "Age")

#clean the environment
remove(biomarker_att,BioMarker_High_Low,event, surv_object,title)


############## K-MEANS #####################
# To make it more easy to cluster we use the filtered dataset
library(factoextra)
library(NbClust)
bonn_kmeans<-CRC_data
bonn_kmeans$Gender<-as.numeric(bonn_kmeans$Gender)-1
bonn_kmeans$Therapy<-as.numeric(bonn_kmeans$Therapy)-1
bonn_kmeans$Systemic_Diseases<-as.numeric(bonn_kmeans$Systemic_Diseases)-1
bonn_kmeans<-bonn_kmeans[complete.cases(bonn_kmeans),]
bonn_kmeans[-2]<-scale(bonn_kmeans[-2])

fviz_nbclust(bonn_kmeans[-2],kmeans,method = "wss")
NbClust(data = bonn_kmeans[-2], diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")
set.seed(123)
km.res <- kmeans(bonn_kmeans[5:19], 3)
# Visualize
fviz_cluster(km.res, data = bonn_kmeans[5:19],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
#lets try another ploting method
bonn_kmeans$cluster<-as.factor(km.res$cluster)
ggplot(bonn_kmeans, aes(Survival, y = Age, colour = Primary_Tumor)
       ) + geom_point()
str(km.res)
#compare the clusters to type of disease (columns are type disease)
table(km.res$cluster, bonn_kmeans$Primary_Tumor)
km.res$centers



############## SVM #########################
set.seed(1101)
library(e1071)
svm_bonn<-na.omit(Bonn_data)
svm_bonn$Survival<-as.factor(ifelse(svm_bonn$Survival>median(
  svm_bonn$Survival),"High","Low"))
svm_fit = svm(svm_bonn$Survival ~., data = svm_bonn, scale = F, kernel = "linear", cost = 10)
attach(svm_bonn)
bonngrid<-make.grid(svm_bonn)
ygrid = predict(svm_fit, bonngrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)


############## Cox Hazard Regression ##############
#For every biomarker we build two models, a null mudel and biomarker model
#the null model conatains only the confounders, and the biomarker model 
#contins the null model+one biomarker. We build a model for each biomarker,
#which is essentially a Univariate model, because the size of the data set 
#is small and then not efficient to use a Multivariate model for all the
# variables together (which is the common use of Cox regeression) 

#note to self - there is no need to scale and center the data in cox models
#the results of the regression are the same

#Load data set.
#drop the columns of CA which are dependant on the other CA columns
Cox_Bonn<-na.omit(Bonn_data[,-c(9,12)])

# #function to rum multople cox regeressions
# varlist <- names(Cox_Bonn)[6:18]  ## get the variables i want to involve in loop
# tmpfun <- function(x) as.formula(paste("Surv(Survival, event)",x,sep="~"))
# models <- lapply(varlist, function(x) {
#   coxph(tmpfun(x), data = Cox_Bonn, ties="efron")
#   #coxph(substitute(Surv(Survival, event) ~ i, list(i = as.name(x))), data = Cox_Bonn)
# })

# ###### try#######
# Cox_Bonn<-na.omit(Bonn_data[,-c(9,12)])
# biomarkeres<-names(Cox_Bonn[6:18])
# purrr::map(biomarkeres,~step(coxph(as.formula(paste("Surv(Survival, event) ~ Therapy* + Gender + Age + Primary_Tumor*", .x)),data= Cox_Bonn)))
# for (i in 1:length(biomarkeres)){
#   res.cox.t <- step(coxph(Surv(Survival, event)
#                         ~ Therapy* biomarkeres[i] + Gender + Age
#                         + (frailty(Primary_Tumor))*biomarkeres[i], data = Cox_Bonn))
#   
# }
                  


Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$MMP_9),]
# MMP_9
biomarker<-Cox_data$MMP_9
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* MMP_9 + Gender + Age
                      + (frailty(Primary_Tumor))*MMP_9, data = Cox_data))
#build null model
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
summary(res.cox)
#anova test to compare the two models
anova_test<-anova(null.cox,res.cox)
#save the pvalue
Pvals_vector<-anova_test$`P(>|Chi|)`[2]
#extract the linear predictors from the model for each observation
risk_matrix<-as.data.frame(res.cox$linear.predictors)

#There are 3 assumption that needs to be checked when performing cox model:
#assumption 1: Proportional hazard ratio assumption. This assumption is essential
#for the cox model to be reliable
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
#Assumption 2: Influential observations assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
#Assumption 3:non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ MMP_9,
                + log(MMP_9) +sqrt(MMP_9),
                data = Cox_data)
#Plot Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(MMP_9 = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())

# MMP_2
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$MMP_2),]
biomarker<-Cox_data$MMP_2
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* MMP_2 + Gender + Age
                      + (frailty(Primary_Tumor))*MMP_2, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$MMP_2<-res.cox$linear.predictors
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
ggcoxfunctional(Surv(Survival, event) ~ MMP_2,
                + log(MMP_2) +sqrt(MMP_2),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(MMP_2 = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#I_normalized
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$I_normalized),]
biomarker<-Cox_data$I_normalized
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* I_normalized + Gender + Age
                      + (frailty(Primary_Tumor))*I_normalized, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$CA_I<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
ggcoxfunctional(Surv(Survival, event) ~ I_normalized,
                + log(I_normalized) +sqrt(I_normalized),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(I_normalized = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#II_normalized
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$II_normalized),]
biomarker<-Cox_data$II_normalized
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* II_normalized + Gender + Age
                      + (frailty(Primary_Tumor))*II_normalized, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$CA_II<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ II_normalized,
                + log(II_normalized) +sqrt(II_normalized),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(II_normalized = c("High","Low"), 
                                        Therapy = c('1', '2'),
                                        Age = rep(mean(Age, na.rm = TRUE), 2)))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#III_normalized
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$III_normalized),]
biomarker<-Cox_data$I_normalized
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* III_normalized + Gender + Age
                      + (frailty(Primary_Tumor))*III_normalized, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$CA_III<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ III_normalized,
                + log(III_normalized) +sqrt(III_normalized),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(III_normalized = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#IV_normalized
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$IV_normalized),]
biomarker<-Cox_data$IV_normalized
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* IV_normalized + Gender + Age
                      + (frailty(Primary_Tumor))*IV_normalized, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$CA_IV<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ IV_normalized,
                + log(IV_normalized) +sqrt(IV_normalized),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(IV_normalized = c("High","Low"), 
                                        Therapy = c('1', '2'),
                                        Age = rep(mean(Age, na.rm = TRUE), 2)))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())



#Calgranulin
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$Calgranulin),]
biomarker<-Cox_data$Calgranulin
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* Calgranulin + Gender + Age
                      + (frailty(Primary_Tumor))*Calgranulin, data = Cox_data,x=TRUE))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$calgranulin_A<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ Calgranulin,
                + log(Calgranulin) +sqrt(Calgranulin),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data,cpositions = c(0.02,0.12,0.3), fontsize = 1)
biomark_df <- with(Cox_data, data.frame(Calgranulin = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())
ggsurvplot(survfit(res.cox), data = Cox_data, color = "#2E9FDF",
           ggtheme = theme_minimal(),legend = "none", title = "Survival probability against Time")

#Catalase
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$Catalase),]
biomarker<-Cox_data$Catalase
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox1 <- step(coxph(Surv(Survival, event)
                      ~ Therapy* Catalase + Gender + Age
                      + (frailty(Primary_Tumor))*Catalase, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$catalase<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ Catalase,
                + log(Catalase) +sqrt(Catalase),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(Catalase = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#Profilin
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$Profilin),]
biomarker<-Cox_data$Profilin
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* Profilin + Gender + Age
                      + (frailty(Primary_Tumor))*Profilin, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$Profilin<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
ggcoxfunctional(Surv(Survival, event) ~ Profilin,
                + log(Profilin) +sqrt(Profilin),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(Profilin = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#RhoA
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$RhoA),]
biomarker<-Cox_data$RhoA
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* RhoA + Gender + Age
                      + (frailty(Primary_Tumor))*RhoA, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
#anova test and save pvalue
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$RhoA<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ RhoA,
                + log(RhoA) +sqrt(RhoA),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(RhoA = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


#SOD2
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$SOD2),]
biomarker<-Cox_data$SOD2
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* SOD2 + Gender + Age
                      + (frailty(Primary_Tumor))*SOD2, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
#anova test and save pvalue
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$SOD2<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ SOD2,
                + log(SOD2) +sqrt(SOD2),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(SOD2 = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())

#Trx
Cox_data<-Cox_Bonn[complete.cases(Cox_Bonn$Trx),]
biomarker<-Cox_data$Trx
Cox_data$Biomark_median<-factor(ifelse(biomarker > median(biomarker) ,"High","Low"))
#build model with biomarker
res.cox <- step(coxph(Surv(Survival, event)
                      ~ Therapy* Trx + Gender + Age
                      + (frailty(Primary_Tumor))*Trx, data = Cox_data))
null.cox <- step(coxph(Surv(Survival, event)
                       ~ Therapy + Gender + Age
                       + frailty(Primary_Tumor), data = Cox_data))
#anova test and save pvalue
anova_test<-anova(null.cox,res.cox)
Pvals_vector<-c(Pvals_vector, anova_test$`P(>|Chi|)`[2])
summary(res.cox)
risk_matrix$TRX<-res.cox$linear.predictors
# test Proportional hazard ratio assumption
test.ph<-cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
# test Influential observation assumption
ggcoxdiagnostics(res.cox, type = "dfbeta",linear.predictions = F,
                 ggtheme = theme_bw())
# testing non-linearity assumption
ggcoxfunctional(Surv(Survival, event) ~ Trx,
                + log(Trx) +sqrt(Trx),
                data = Cox_data)
# Survival curves
ggforest(res.cox, data = Cox_data)
biomark_df <- with(Cox_data, data.frame(Trx = c("High","Low"), 
                                        Therapy = c('1', '2')))
fit <- survfit(res.cox, newdata = biomark_df)
ggsurvplot(fit,data = Cox_data, conf.int = TRUE, legend.labs=c("High", "Low"),
           ggtheme = theme_minimal())


# look on the p-values of the models after correction for multiple comparisons
p.adjust(Pvals_vector)



###### C-indexes for Fitted and Predicted models#####

#This function recieves train set and test set. If you want to run the funcrion
#on all the data without splitting then dont supply test set.
# The term 'Fitted' reffers to the training set, i.e a subgroup which contains
# the majority of observations. The term 'Predicted' reffers to the test set 
# which is predicted, i.e the model predicts the test's risk without seeing 
# those samples before.
# The function builds the cox model for each biomarker (Univariate) on the 
#train set and save the linear predictors for the samples of the train set.
# Then it predicts the risk for the train set and save separately their linear
# predictors.
# then it calculates the C-index for the fitted model.
# At the end the function returns a list with 1) linear predictors matrix of 
# the fitted model. 2) linear predictors matrix of the predicted model. 3)C
# index of the fitted models
predict_matrix<-function(trainData,testData=NULL){
  # build a cox model for the biomarker on the train set
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* MMP_9 + Gender + Age
                        + (frailty(Primary_Tumor))*MMP_9, data = trainData),trace=0)
  # add the vector of linear predictors to the train and test matrixes
  train_matrix<-as.data.frame(res.cox$linear.predictors)
  if(!missing(testData)){test_matrix<-as.data.frame(predict(res.cox,testData,type = "lp"))}
  # save the C-index of the model
  CI_fitted<-as.data.frame(res.cox$concordance[6])
  # do the same for all biomarkers
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* MMP_2 + Gender + Age
                        + (frailty(Primary_Tumor))*MMP_2, data = trainData),trace=0)
  train_matrix$MMP_2<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$MMP_2<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* I_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*I_normalized, data = trainData),trace=0)
  train_matrix$I_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$I_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* III_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*III_normalized, data = trainData),trace=0)
  train_matrix$III_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$III_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* IV_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*IV_normalized, data = trainData),trace=0)
  train_matrix$IV_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$IV_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* Calgranulin + Gender + Age
                        + (frailty(Primary_Tumor))*Calgranulin, data = trainData),trace=0)
  train_matrix$Calgranulin<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Calgranulin<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox<-step(coxph(Surv(Survival, event)
                        ~ Therapy* Catalase + Gender + Age
                        + (frailty(Primary_Tumor))*Catalase, data = trainData),trace=0)
  train_matrix$Catalase<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Catalase<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* Profilin + Gender + Age
                        + (frailty(Primary_Tumor))*Profilin, data = trainData),trace=0)
  train_matrix$Profilin<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Profilin<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* RhoA + Gender + Age
                        + (frailty(Primary_Tumor))*RhoA, data = trainData),trace=0)
  train_matrix$RhoA<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$RhoA<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* SOD2 + Gender + Age
                        + (frailty(Primary_Tumor))*SOD2, data = trainData),trace=0)
  train_matrix$SOD2<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$SOD2<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  res.cox<-step(coxph(Surv(Survival, event)
                      ~ Therapy* Trx + Gender + Age
                      + (frailty(Primary_Tumor))*Trx, data = trainData),trace=0)
  train_matrix$Trx<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Trx<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  if(!missing(testData)){
    return(list("train" = train_matrix,"test" = test_matrix,"CI"=CI_fitted))}
  return(list("train" = train_matrix,"CI"=CI_fitted))
}


# Run 10 times 10 Cross Valodation so we will get the predicted models 
# score 10 time, and then we will boxplot it
library(caret)
library(dynpred)
Cox_data<-na.omit(Bonn_data[,-c(9,12)])
# outer loop
CI_fitted_biggest<-data.frame()
CI_predicted_biggest<-data.frame()
for (i in 1:1) {
  # split the data into 10 folds with caret package
  flds <- createFolds(Cox_data$Survival, k = 10, list = T, returnTrain = F)
  # Create empty dataframes for the outputs of the 10 fold CV
  C_index_fitted<-data.frame()
  L_predictors<-data.frame()
  # Inner loop: Perform 10 fold cross validation
  # NOTE: THIS IS ONLY ONE RUN OF 10 FOLD CV. ESSENSIALY THE 10 FOLD SPLITs
  # THE DATA SUCH THAT EVERY SAMPLE WILL BE PREDICTED EXACTLY ONE TIME.
  for(i in flds){
    testData <- Cox_data[i, ]
    trainData <- Cox_data[-i, ]
    
    fitted_cox<-predict_matrix(trainData,testData)
    
    C_index_fitted<-rbind(C_index_fitted,t(fitted_cox$CI))
    L_predictors<-rbind(L_predictors,fitted_cox$test)
  }
  
  names(L_predictors)[1]<-"MMP_9"
  #add to the predict table the survival and event for each sample(essentially
  #to calculate C index)
  L_predictors<-cbind(L_predictors,Cox_data[as.numeric(rownames(L_predictors)),17:18])
  #Extract C-index for every biomarker. This C-index is for the predicted table.
  C_indexes_predict<-as.data.frame(cindex(Surv(Survival, event)~ MMP_9, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ MMP_2, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ I_normalized, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ III_normalized, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ IV_normalized, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ Calgranulin, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ Catalase, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ Profilin, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ RhoA, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ SOD2, data = L_predictors)[3])
  C_indexes_predict<-cbind(C_indexes_predict,cindex(Surv(Survival, event)~ Trx, data = L_predictors)[3])

  CI_fitted_biggest<-rbind(CI_fitted_biggest,C_index_fitted)
  CI_predicted_biggest<-rbind(CI_predicted_biggest,C_indexes_predict)
}

# calculate the average of evry 10 folds fitted model, so we will end up with
# only 10 C indexes for the fitted model, like the number of predicted CI 
CI_fitted_means<-data.frame()
for (i in 0:9) {
  repetition<-CI_fitted_biggest[(i*10):(i*10+10),]
  CI_fitted_means<-rbind(CI_fitted_means,colMeans(repetition[sapply(repetition, is.numeric)]))
}

names(CI_fitted_means)<-c("MMP_9","MMP_2","I_normalized","III_normalized",
                  "IV_normalized","Calgranulin","Catalase",
                  "Profilin","RhoA", "SOD2","Trx")
names(CI_predicted_biggest)<-c("MMP_9","MMP_2","I_normalized","III_normalized",
                            "IV_normalized","Calgranulin","Catalase",
                            "Profilin","RhoA", "SOD2","Trx")

# Form box plots of the C-index for the fitted models
boxplot(CI_fitted_means)
boxplot(CI_predicted_biggest)


## SOME EXPLANATIONS
# There is a certatin problem in the approach abbove with the CV.
# The matrix generated for predictions in each 10-fold (inner loop) seems 
# it is a matrix of predictions predicted by one model. But what really happends
# is that in every fold different train set (90% of samples) is used, and so
# essentially every such model (built in each fold) is a different classier.
# And then essentially each one of those different classifiers predict different
# 10% of the hole data set (which after 10-folds formed a predicted matrix for
# all the observations), and so the C index for the predicted matrix is nor 
# really one model but rather one C index for multiple models. 
# The Ideal approach would have calculated CI after every fold, i.e for every
# 10% predicted by one train set.This approach is only good when the test set
# is big enough to have the same sidtribution as the train. 
# The problem in this project is the small number
# of observations, so 10% is 11 samples and it is meaningless to calculate CI
# for 11 observations. That is why we preffered do that in the first approach 
# described even thoug we are calculating one CI for a matrix built over different
# models (even though the different between the models is only 10% of observations
# but they are still different models).



###### Hierarchial Consensus Clustering -  All the data set #####

#First we do clustering on all the data to see how many clusters are. 
#Later we will validate the clustering method by using ps.cluster


hc_data<-na.omit(Bonn_data[,-c(9,12)])
# Form a matrix of the lineat predictors our from 100% of the data. 
risk_matrix_all<-predict_matrix(hc_data)
names(risk_matrix_all[[1]])[1]<-"MMP_9"
risk_matrix_T<-t(risk_matrix_all$train)

#Generate clusters with consensus hierarchial clustering
dc = sweep(risk_matrix_T,1, apply(risk_matrix_T,1,median,na.rm=T))
rcc = ConsensusClusterPlus(dc,maxK=5,reps=100,pItem=0.8,pFeature=1,clusterAlg="hc")
resICL = calcICL(rcc,title="example")
# WE CHOOSE K=3 AS THE OPTIMAL NUMBER OF CLUSTERS
#add the classficitaion to the dataframe
rcc[[3]][["consensusClass"]]
hc_data$cluster<-as.factor(rcc[[3]][["consensusClass"]])

# Draw a Kaplan Meier Plot to see association with survival
data<-hc_data
surv_object<-Surv(time = data$Survival
                ,event = data$event)
fit1 <- survfit(surv_object ~ cluster, data = data)
#summary(fit1)

# hc_data$cluster <- factor(hc_data$cluster, levels = c("1","2","3"), labels = c("A","B","C"))
ggsurv<- ggsurvplot(fit1, data = data, pval = TRUE,title = "Survival plot for stratified groups by Consensus Clustering",
                    xlab = "Time (months)",legend = "right",
                    legend.labs = c("cluster 1","cluster 2","cluster 3")
                    #,theme(plot.title = element_text(hjust = 0.5))
                    )
ggsurv$plot + theme(plot.title = element_text(hjust = 0.5, size = 15))
    

# We want to check wether there is association between the clusters and
# one of the confounders. Essentialy the cox model we used for each biomarkder
# had already taked the confounders into account, but it is still good
# to check it. We will use anova for AGE (anova is kind of extension of 
# t-test, that can be applied for more than two groups. 
# for TUmor type, addotional disease, Gender we use chisquare test of independence because
# it compares categorical groups.
chisq.test(hc_data$cluster, hc_data$Primary_Tumor)
chisq.test(hc_data$cluster, hc_data$Gender)
chisq.test(hc_data$cluster, hc_data$Systemic_Diseases)
chisq.test(hc_data$cluster, hc_data$Therapy)
res.anova<-aov(Age ~ cluster, data = hc_data)
summary(res.anova)

###### PREDICTION STRENGTH FOR CLUSTERING  ######
#Here we want to see the generalization of the Consensus clustering used above
#We are using a cross 
#library(class)
library(genefu)
library(nnet)
#empty lists for saving the prediction strength score
#Initiate a vector for prediction scored of 'true' value
ps_score<-0
ps_Score_random<-0
#outer loop. number of desired repeats/2 (the 2-folds inside the loop doubles it again)
for (i in 1:50) {
  #Random sampling the cluster labels generated from clustering all the data (done previously)
  partitions<-sample(rownames(hc_data))
  # split to train and test
  train<-c(partitions[1:54])
  test<-c(partitions[55:108])
  #2-fold loop. the if condition exchange the test and train every loop
  for (i in 1:2) {
    if(i==1){
      hc_train_samples<-hc_data[train,]
      hc_test_samples<-hc_data[test,]
    } else {
      hc_train_samples<-hc_data[test,]
      hc_test_samples<-hc_data[train,]
    }
    # the function builds a model and returns a matrix of fitted linear predictors of the train set,
    # and also returns a matrix of linear predictors predicted for the test set.
    hc_fitted<-predict_matrix(hc_train_samples,hc_test_samples)
    #save the train and test matrixes
    train_matrix<-hc_fitted$train
    c1_test_matrix<-hc_fitted$test
    #save copy 2 of the test matrix
    fitted_test<-predict_matrix(hc_test_samples)
    c2_test_matrix<-fitted_test$train
    
    #technical issue
    names(c1_test_matrix)[1]<-"MMP_9"
    names(c2_test_matrix)[1]<-"MMP_9"
    names(train_matrix)[1]<-"MMP_9"
    
    # Hierarchial Clustering on train set
    matrix_T<-t(train_matrix)
    dc = sweep(matrix_T,1, apply(matrix_T,1,median,na.rm=T))
    rcc = ConsensusClusterPlus(dc,maxK=3,reps=10,pItem=0.8,pFeature=1,clusterAlg="hc", plot = FALSE)
    #add the labels for each observation to the train matrix
    train_matrix$cluster<-as.factor(rcc[[3]][["consensusClass"]])
    
    # Hierarchial clustering for copy 1 of test set
    matrix_T<-t(c1_test_matrix)
    dc = sweep(matrix_T,1, apply(matrix_T,1,median,na.rm=T))
    rcc = ConsensusClusterPlus(dc,maxK=3,reps=10,pItem=0.8,pFeature=1,clusterAlg="hc", plot = FALSE)
    #add the labels for each observation to the train matrix
    c1_test_matrix$cluster<-as.factor(rcc[[3]][["consensusClass"]])
    
    # Apply Logistic Regression Classifier
    # first the function needs manually defined a refference. I choose cluster 1
    train_matrix$cluster<-relevel(train_matrix$cluster, ref = 1)
    # fit the logistic regression
    multlogreg_fit <- multinom(cluster ~ MMP_9 + MMP_2 + I_normalized + III_normalized + IV_normalized 
                               + Calgranulin + Catalase + Profilin
                               + RhoA + SOD2 +Trx , data = train_matrix,trace = 0)
    
    # classify the copy 2 with the logistic regresiion model
    c2_test_matrix$cluster_predicted<-predict(multlogreg_fit,newdata = c2_test_matrix,"class")

    # check prediction strength and save the score
    ps_score<-rbind(ps_score,ps.cluster(c1_test_matrix$cluster,c2_test_matrix$cluster_predicted)[[1]])
    # Random Permutation option 1: Create random permutation of the classified test set lables, and check PS.
    ps_Score_random<-rbind(ps_Score_random,ps.cluster(c1_test_matrix$cluster,sample(c2_test_matrix$cluster_predicted))[[1]])
    }
}

#random permutation option 2: randomness generated by shuffle the original 
# labels clustered on fitted model of all the data
ps_Score_random<-0
for (i in 1:100) {
  partitions<-sample(hc_data$cluster)
  train<-partitions[1:54]
  test<-partitions[55:108]
  ps_Score_random<-rbind(ps_Score_random,ps.cluster(train,test)[[1]])
}

#compare the true value and the random permutation (I drop the first score from every vector
# because it was the initiation with score 0)
ps_score<-as.data.frame(ps_score)
ps_score$type<-as.factor("11 Biomarkers")
ps_Score_random<-as.data.frame(ps_Score_random)
ps_Score_random$type<-as.factor("4 Biomarkers")

ps_score_combined<-rbind(ps_score[-1,],ps_Score_random[-1,])
ggplot(ps_score_combined, aes(x=type, y=V1,fill=type)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4,color = "blue")+
  labs(title = "Prediction Strength scores")+
  theme(plot.title = element_text(hjust = 0.5))

boxplot(ps_score[-1,1], ps_Score_random[-1,1])
sum(ps_score[,1])/(length(ps_score[,1])-1)
sum(ps_Score_random[,1])/(length(ps_Score_random[,1])-1)
cbind(ps_score[,1],ps_Score_random[0:101,1])

#check normal distribution and run wilcoxon/t tests accordingly
shapiro.test(ps_score[-1,1])
shapiro.test(ps_Score_random[-1,1])
wilcox.test(ps_score[-1,1],ps_Score_random[-1,1],paired = TRUE)
t.test(ps_score[-1,1],ps_Score_random[-1,1],paired = TRUE)


#### MANUAL DIVISION TO SURVIVAL GROUPS ####

HCC_data<-na.omit(Bonn_data[Bonn_data$Primary_Tumor=="1",-c(9,12)])
#create a cummulative ditribution table
x<-HCC_data$Survival
cbind( Freq=table(x), Cumul=cumsum(table(x)), relative=prop.table(table(x)))
#label the patient according to the survival length
surv_div<-ifelse(HCC_data$Survival<=6,"Short",
                 ifelse(HCC_data$Survival<24,"Middle","Long"))
HCC_data$surv_group<-factor(surv_div)

HCC_short<-HCC_data[HCC_data$Survival<=7,]
HCC_long<-HCC_data[HCC_data$Survival>7,]

par(mfrow=c(1,1))
dev.off()


#### FEATURE SELECTED ####

predict_matrix<-function(trainData,testData=NULL){
  # build a cox model for the biomarker on the train set
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* MMP_2 + Gender + Age
                        + (frailty(Primary_Tumor))*MMP_2, data = trainData),trace=0)
  # add the vector of linear predictors to the train and test matrixes
  train_matrix<-as.data.frame(res.cox$linear.predictors)
  if(!missing(testData)){test_matrix<-as.data.frame(predict(res.cox,testData,type = "lp"))}
  # save the C-index of the model
  CI_fitted<-as.data.frame(res.cox$concordance[6])
  # do the same for all biomarkers
  
  # res.cox <- step(coxph(Surv(Survival, event)
  #                       ~ Therapy* MMP_2 + Gender + Age
  #                       + (frailty(Primary_Tumor))*MMP_2, data = trainData),trace=0)
  # train_matrix$MMP_2<-res.cox$linear.predictors
  # if(!missing(testData)){test_matrix$MMP_2<-predict(res.cox,testData,type = "lp")}
  # CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  # 
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* I_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*I_normalized, data = trainData),trace=0)
  train_matrix$I_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$I_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* III_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*III_normalized, data = trainData),trace=0)
  train_matrix$III_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$III_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* IV_normalized + Gender + Age
                        + (frailty(Primary_Tumor))*IV_normalized, data = trainData),trace=0)
  train_matrix$IV_normalized<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$IV_normalized<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* Calgranulin + Gender + Age
                        + (frailty(Primary_Tumor))*Calgranulin, data = trainData),trace=0)
  train_matrix$Calgranulin<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Calgranulin<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
# 
#   res.cox<-step(coxph(Surv(Survival, event)
#                       ~ Therapy* Catalase + Gender + Age
#                       + (frailty(Primary_Tumor))*Catalase, data = trainData),trace=0)
#   train_matrix$Catalase<-res.cox$linear.predictors
#   if(!missing(testData)){test_matrix$Catalase<-predict(res.cox,testData,type = "lp")}
#   CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
# 
  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* Profilin + Gender + Age
                        + (frailty(Primary_Tumor))*Profilin, data = trainData),trace=0)
  train_matrix$Profilin<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$Profilin<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  # res.cox <- step(coxph(Surv(Survival, event)
  #                       ~ Therapy* RhoA + Gender + Age
  #                       + (frailty(Primary_Tumor))*RhoA, data = trainData),trace=0)
  # train_matrix$RhoA<-res.cox$linear.predictors
  # if(!missing(testData)){test_matrix$RhoA<-predict(res.cox,testData,type = "lp")}
  # CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  res.cox <- step(coxph(Surv(Survival, event)
                        ~ Therapy* SOD2 + Gender + Age
                        + (frailty(Primary_Tumor))*SOD2, data = trainData),trace=0)
  train_matrix$SOD2<-res.cox$linear.predictors
  if(!missing(testData)){test_matrix$SOD2<-predict(res.cox,testData,type = "lp")}
  CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])

  # res.cox<-step(coxph(Surv(Survival, event)
  #                     ~ Therapy* Trx + Gender + Age
  #                     + (frailty(Primary_Tumor))*Trx, data = trainData),trace=0)
  # train_matrix$Trx<-res.cox$linear.predictors
  # if(!missing(testData)){test_matrix$Trx<-predict(res.cox,testData,type = "lp")}
  # CI_fitted<-rbind(CI_fitted,res.cox$concordance[6])
  
  if(!missing(testData)){
    return(list("train" = train_matrix,"test" = test_matrix,"CI"=CI_fitted))}
  return(list("train" = train_matrix,"CI"=CI_fitted))
}


hc_data<-na.omit(Bonn_data[,-c(6,9,12,14,16,18)])
ps_score_7<-0
ps_Score_random_7<-0
#outer loop. number of desired repeats/2 (the 2-folds inside the loop doubles it again)
for (i in 1:50) {
  #Random sampling the cluster labels generated from clustering all the data (done previously)
  partitions<-sample(rownames(hc_data))
  # split to train and test
  train<-c(partitions[1:54])
  test<-c(partitions[55:108])
  #2-fold loop. the if condition exchange the test and train every loop
  for (i in 1:2) {
    if(i==1){
      hc_train_samples<-hc_data[train,]
      hc_test_samples<-hc_data[test,]
    } else {
      hc_train_samples<-hc_data[test,]
      hc_test_samples<-hc_data[train,]
    }
    # the function builds a model and returns a matrix of fitted linear predictors of the train set,
    # and also returns a matrix of linear predictors predicted for the test set.
    hc_fitted<-predict_matrix(hc_train_samples,hc_test_samples)
    #save the train and test matrixes
    train_matrix<-hc_fitted$train
    c1_test_matrix<-hc_fitted$test
    #save copy 2 of the test matrix
    fitted_test<-predict_matrix(hc_test_samples)
    c2_test_matrix<-fitted_test$train
    
    # #technical issue
    names(c1_test_matrix)[1]<-"MMP_2"
    names(c2_test_matrix)[1]<-"MMP_2"
    names(train_matrix)[1]<-"MMP_2"
    # 
    # Hierarchial Clustering on train set
    matrix_T<-t(train_matrix)
    dc = sweep(matrix_T,1, apply(matrix_T,1,median,na.rm=T))
    rcc = ConsensusClusterPlus(dc,maxK=3,reps=10,pItem=0.8,pFeature=1,clusterAlg="hc",plot = FALSE)
    #add the labels for each observation to the train matrix
    train_matrix$cluster<-as.factor(rcc[[3]][["consensusClass"]])
    
    # Hierarchial clustering for copy 1 of test set
    matrix_T<-t(c1_test_matrix)
    dc = sweep(matrix_T,1, apply(matrix_T,1,median,na.rm=T))
    rcc = ConsensusClusterPlus(dc,maxK=3,reps=10,pItem=0.8,pFeature=1,clusterAlg="hc",plot = FALSE)
    #add the labels for each observation to the train matrix
    c1_test_matrix$cluster<-as.factor(rcc[[3]][["consensusClass"]])
    
    # Apply Logistic Regression Classifier
    # first the function needs manually defined a refference. I choose cluster 1
    train_matrix$cluster<-relevel(train_matrix$cluster, ref = 1)
    # fit the logistic regression
    multlogreg_fit <- multinom(cluster ~ MMP_2 + I_normalized + III_normalized + IV_normalized +
                                 Profilin + SOD2 + Calgranulin , data = train_matrix,trace = 0)
    
    # classify the copy 2 with the logistic regresiion model
    c2_test_matrix$cluster_predicted<-predict(multlogreg_fit,newdata = c2_test_matrix,"class")
    
    # check prediction strength and save the score
    ps_score_7<-rbind(ps_score_7,ps.cluster(c1_test_matrix$cluster,c2_test_matrix$cluster_predicted)[[1]])
    # Random Permutation option 1: Create random permutation of the classified test set lables, and check PS.
    ps_Score_random_7<-rbind(ps_Score_random_7,ps.cluster(c1_test_matrix$cluster,sample(c2_test_matrix$cluster_predicted))[[1]])
  }
}

ps_score<-as.data.frame(ps_score_2[-1])
ps_Score_random<-as.data.frame(ps_Score_random_2[-1])
#compare the true value and the random permutation (I drop the first score from every vector
# because it was the initiation with score 0)
ps_score$type<-as.factor("True")
colnames(ps_Score_random)[1] <- "V1"
colnames(ps_score)[1] <- "V1"
ps_Score_random$type<-as.factor("Random")

ps_score_combined<-rbind(ps_score,ps_Score_random)
ggplot(ps_score_combined, aes(x=type, y=V1,fill=type)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4,color = "blue")+
  labs(title = "Prediction Strength scores")+
  theme(plot.title = element_text(hjust = 0.5))

sum(ps_score[,1])/(length(ps_score[,1])-1)
sum(ps_Score_random[,1])/(length(ps_Score_random[,1])-1)
sum(ps_score[,1])/(length(ps_score[,1])-1) - sum(ps_Score_random[,1])/(length(ps_Score_random[,1])-1)

results1<-data.frame(ps_score_11,ps_score_10,ps_score_9,ps_score_8,ps_score_7,ps_score_6[,1],ps_score_5[,1],ps_score_4,ps_score_3[25:125],ps_score_2)
#results1<-results1[-1,]
means_True<-data.frame(colMeans(results1))

results<-data.frame(ps_Score_random_11,ps_Score_random_10,ps_Score_random_9,ps_Score_random_8,ps_Score_random_7,ps_Score_random_6[,1],ps_Score_random_5[,1],ps_Score_random_4,ps_Score_random_3[25:125],ps_Score_random_2)
#results<-results[-1,]
means_Random<-data.frame(colMeans(results))

colMeans(results1)-colMeans(results)

p<-data.frame("True_value" = colMeans(results1),"Random_value" = colMeans(results), "Num_Biomarkers" = c(11:2))
p_long <- melt(p, id="Num_Biomarkers")  # convert to long format
ggplot(data=p_long, aes(x=-Num_Biomarkers, y=value, colour=variable)) + geom_line()

Num_Biomarkers <- as.character(c(11:2))
p_distance<-data.frame("interval" = colMeans(results1)-colMeans(results), "Num_Biomarkers" = c(11:2))
p_long <- melt(p, id="Num_Biomarkers")  # convert to long format
ggplot(data=p_distance, aes(x=-Num_Biomarkers,y = interval)) + geom_line()

# save df with best 21 survivors
library("writexl")
temp <- data[data$Survival > 24,]
temp$patient_num <- rownames(temp)
temp <- temp[, c(20, 1:19)]

write_xlsx(temp,"best_survivors_group.xlsx",)
