# The current script assimilates multiple organizational and analytical steps from Hovhannisyan et al., 2020, comprising an investigation of the visual and semantic properties that predict object memory. 
# The database is based on the DinoLab objects (https://mariamh.shinyapps.io/dinolabobjects/)

# load in packages and libraries 
library("ggplot2")
library("readr")
library("readxl")
install.packages("base",dependencies = TRUE)
library("base")
install.packages("olsrr",dependencies = TRUE)
library("olsrr")

library("ggimage")
library(boot)
library(car)
library("GGally")
library("psych")
library(pastecs)
library(outliers)

#set the directory
setwd("~/Object_Memorability")


#dataset with raw predictor values (without transformations)
d_r<-  read.csv("~/predictors.csv")

#data with appropriate predictor values trasnformed
allRegValues<-read.csv("~/predictors_transformed.csv")


# LOG TRANSFORMATIONS OF THE DATA THAT ARE SKEWED AND REQUIRE IT (PNWS, MD, and slope do not require a transformation)
#semantic features
freqq<-log10(d_r$COCA_freq)
is.na(freqq) <- sapply(freqq, is.infinite)
num_features<- log10(d_r$NoF_noTax)
cs_noTax<-log10(d_r$CSC_noTax)


#visual features
energy<-log10(d_r$Energy)
jpeg_size<-log10(d_r$JPEG_size)
layer3<-log10(d_r$layer3)
layer6<-log10(d_r$layer6)
layer8<-log10(d_r$layer8)

#PLOTTING ORIGINAL AND TRANSFORMED DISTRIBUTIONS USING THE ABOVE VARIABLES FOR THE TRANSFORMED PREDICTORS.
#original
ooo<- ggplot(data=d_r, (aes(x=name_adj2, size=2)))
ooo + geom_line(color="pink", stat="density")+ theme(legend.position = "none", axis.text.x=element_text(face="bold", size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "grey"),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

#transformed
nnn<- ggplot(data=subset(allRegValues), (aes(x=nameAgree_adj, size=2)))
nnn + geom_line(color="red3", stat="density")+ theme(legend.position = "none", axis.text.x=element_text(face="bold", size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "grey"),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())


#regression models, final models were calculated in matlab
reg_pp_hit<- lm(allRegValues$hit_pp~ energy_adj + JPEG_size_adj + PNWS + H_adj + S_adj + V_adj + Layer3_adj + Layer6_adj + Layer8_adj + frequency_adj +  name_agreement_adj + NoF_adj + CS_adj + MD + slope_new, data=allRegValues)
print(summary(reg_pp_hit), digits=4)

reg_pp_far<- lm(allRegValues$far_pp~ energy_adj + JPEG_size_adj + PNWS + H_adj + S_adj + V_adj + Layer3_adj + Layer6_adj + Layer8_adj + frequency_adj +  name_agreement_adj + NoF_adj + CS_adj + MD + slope_new, data=allRegValues)
print(summary(reg_pp_far), digits=4)

reg_pw_hit<- lm(allRegValues$hit_pw~ energy_adj + JPEG_size_adj + PNWS + H_adj + S_adj + V_adj + Layer3_adj + Layer6_adj + Layer8_adj + frequency_adj +  name_agreement_adj + NoF_adj + CS_adj + MD + slope_new, data=allRegValues)
print(summary(reg_pw_hit), digits=4)

reg_pw_far<- lm(allRegValues$far_pw~ energy_adj + JPEG_size_adj + PNWS + H_adj + S_adj + V_adj + Layer3_adj + Layer6_adj + Layer8_adj + frequency_adj +  name_agreement_adj + NoF_adj + CS_adj + MD + slope_new, data=allRegValues)
print(summary(reg_pw_far), digits=4)


#correlation for the hit rates for both tasks 
hits<- ggplot(data=allRegValues, aes(x=hit_pp, hit_pw))
hits + geom_point() + geom_smooth(method="lm")


#correlation between hit rates for both tasks 
cor.test(allRegValues$hit_pp, allRegValues$hit_pw, method = c("pearson"))
#correlations between false alarm rates for both tasks 
cor.test(allRegValues$far_pp, allRegValues$far_pw, method = c("pearson"))


#Check for multicolinearity - if largest VIF is greater than 10 then there is cause for concern 
vif(reg_pp_hit)
vif(reg_pp_far)
vif(reg_pw_hit)
vif(reg_pw_far)
#check tolerance 1/vif - tolerance below 0.1 indicates a serious problem and below 0.2 indicates a  potential problem 
1/vif(reg_pp_hit)
1/vif(reg_pp_far)
1/vif(reg_pw_hit)
1/vif(reg_pw_far)
#check mean - if average is substantially greater than 1 then the regression may be biased
mean(vif(reg_pp_hit))
mean(vif(reg_pp_far))
mean(vif(reg_pw_hit))
mean(vif(reg_pw_far))

#get descriptive statistics
#in normal distribution kurtosis and skew should be around 0 
##in large samples (200+) like ours its more important to look at the shape of the distribution visually and to look at the value of the skew and kurtosis rather than calculate their significance. 
##shapiro.wilk test not great for large samples where it's easy to get significant resullts from small deviations from normality 

#raw data without transformations 
descrip_stats_raw<-round(stat.desc(d_r[,c(9:23)], basic = FALSE, norm = TRUE), digits = 4)

descrip_stats_transformed<-round(stat.desc(allRegValues[,c(9:23)], basic = FALSE, norm = TRUE), digits = 4)

#mean for far and hit rates 
mean(na.omit(allRegValues$far_pp))
mean(na.omit(allRegValues$far_pw))
mean(na.omit(allRegValues$hit_pp))
mean(na.omit(allRegValues$hit_pw))



###co-variance matrix ###
#first put all of these adjusted values and values from d_r that are in the regression into a data frame 
cormat <- ggcorr(data=allRegValues[,c(9:23)], label = TRUE, label_size = 5, label_round = 2)
cormat 


#LOOKING ONLY AT DNNs and memory now 
pp_dnn<- lm(d_r$hit_pp ~ d_r$layer1 + d_r$layer4 + d_r$layer7 + d_r$layer9 + d_r$layer11 + d_r$layer15+ d_r$layer18 + d_r$layer20,na.action=na.exclude) 
summary(pp_dnn)

pw_dnn<- lm(d_r$hit_pw ~ d_r$layer1 + d_r$layer4 + d_r$layer7 + d_r$layer9 + d_r$layer11 + d_r$layer15+ d_r$layer18 + d_r$layer20,na.action=na.exclude) 
summary(pw_dnn)


#CORR BETWEEN HIT RATES AND AND DEMOGRAPHIC VARIABLES 
dem_d<- read.csv("~/demographics.csv")

#f test to check variances 
var.test(dem_d$sex_num,dem_d$sex_num_pp, alternative = "two.sided") 
var.test(dem_d$edu_years,dem_d$edu_years_pp, alternative = "two.sided") 
var.test(dem_d$age,dem_d$age_pp, alternative = "two.sided") 
var.test(dem_d$lag,dem_d$lag_pp, alternative = "two.sided") 
var.test(dem_d$race_num,dem_d$race_num_pp, alternative = "two.sided")

#t test to check for group differences in demographics for PP and PW 
t.test(dem_d$age,dem_d$age_pp, var.equal= TRUE)
t.test(dem_d$edu_years,dem_d$edu_years_pp, var.equal = TRUE)
t.test(dem_d$sex_num,dem_d$sex_num_pp,var.equal= TRUE)
t.test(dem_d$lag,dem_d$lag_pp)
t.test(dem_d$race_num,dem_d$race_num_pp)


#pw hr and age
cor.test(dem_d$HR, dem_d$age, method = c("pearson"))
#pp hr and age
cor.test(dem_d$HR_pp, dem_d$age_pp, method = c("pearson"))


#PLOT THE CATEGORIES ORGANIZED FROM HIGHEST TO LOWEST # OF OBJECTS IN EACH

#read in appropriate dataset for this calculation
cats<-read.csv("~/Box Sync/ElectricDino/Projects/Object_Memorability/AMT_Manuscript/categories_hitrates.csv")
rr <- ggplot(data=subset(cats, categories != "NA"), aes(x=reorder(categories, -cat_num), y=cat_num))
rr + geom_bar(fill="lightpink",stat="identity")+ scale_fill_brewer(palette="RdPu") + theme(axis.title=element_text(size=14),axis.text=element_text(size=10),axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="Categories", y="Number of Items")

#choosing appropriate columns for box plot below
cats_cat<-na.omit(data.frame(cats[,16:21]))

g<-ggplot(data=subset(cats_cat, category_1 != "NA"), aes(x=factor(category_1, level= c('birds','fruit','insect','mammal','plant','sea','vegetable','appliances','building','clothing','decorative','electronics','food','furniture','holiday','household items','kitchen items','musical instruments','office tool','sports','street items','tool','toys','vehicle')), y=hit_1, fill=task_1))
g + geom_boxplot() + scale_fill_brewer(palette="RdPu") + theme(axis.title=element_text(size=14),axis.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=0.6,vjust=0.6, size =14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) + labs(x="Categories", y="Hit Rate")


