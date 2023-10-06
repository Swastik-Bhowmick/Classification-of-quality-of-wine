library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggcorrplot)

d=read.csv("E:/logistic/WineQuality.csv");View(d)
head(d,20)
d[sample(1:dim(d)[1],20),]
#working with white wine
d1= d %>% filter(type=="white") %>% select(-type)
head(d1)
summary(d1)
colSums(is.na(d1))
all_column_mean <- apply(d1, 2, mean, na.rm=TRUE)

# imputing NA with the mean calculated
for(i in colnames(d1))
  d1[,i][is.na(d1[,i])] <- all_column_mean[i]
d1

d1=d1%>%mutate(qual= ifelse(quality>5,1,0))%>%select(-quality)
head(d1,20)
colnames(d1)
ggplot(data=d1,aes(fill=as.factor(qual),y=fixed.acidity))+geom_boxplot()+ggtitle("boxplot of volatile acidity")+theme(plot.title=element_text(hjust=0.5))
ggplot(data=d1,aes(fill=as.factor(qual),y=volatile.acidity))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=citric.acid))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=residual.sugar))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=chlorides))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=free.sulfur.dioxide))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=total.sulfur.dioxide))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=density))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=pH))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=sulphates))+geom_boxplot()
ggplot(data=d1,aes(fill=as.factor(qual),y=alcohol))+geom_boxplot()

ggplot(data=d1,aes(fill=as.factor(qual),x=fixed.acidity))+geom_density(alpha=0.5)+ggtitle("boxplot of volatile acidity")+theme(plot.title=element_text(hjust=0.5))+scale_fill_manual(values=c("red","blue"))
d1$id=1:nrow(d1);d1
set.seed(42)
train=d1%>%sample_frac(0.75)
test=anti_join(d1,train,by='id')
View(train)
View(test)
nrow(test)
data=cor(d1%>%select(-c(qual,id)));data;View(data)
is.data.frame(data)
data1=melt(data);data1;View(data1)
ggplot(data1,aes(x=Var1,y=Var2,fill=value))+geom_tile()+theme(axis.text.x=element_text(angle=90),plot.title=element_text(hjust=0.5))+scale_fill_viridis_c()+labs(title = "Correlation Heatmap",x="",
                                                                                                                                             y = "")
#the heatmap shows 
d1%>%select(-qual)%>%cor()%>%ggcorrplot(lab=T,type="upper")
d2=d1%>%select(-qual)
ggpairs(d2)
z=0
d4=train%>%select(-id);d4
car::vif(glm(qual~.,data=d4,family=binomial(link="logit")))
d5=d4%>%select(-density)
car::vif(glm(qual~.,data=d5,family=binomial(link="logit")))
m=glm(qual~.,data=d5,family=binomial(link="logit"))
summary(m)
p=predict(m,type="response");p
m1=glm(qual~.,data=d5%>%select(-c(chlorides,pH,citric.acid)),family=binomial(link="logit"))
summary(m1)
p1=predict(m1,type="response")
y_train=train$qual
specificity_sensitivity=function(a){
    Y_i_hat=ifelse(p1>=a,1,0)
    sens=length(y_train[Y_i_hat==1&y_train==1])/length(y_train[y_train==1])
    spec=length(y_train[Y_i_hat==0&y_train==0])/length(y_train[y_train==0])
    acc=length(y_train[(Y_i_hat==1&y_train==1)|(Y_i_hat==0&y_train==0)])/length(y_train)
    return(c(sens,spec,acc))
}
M=matrix(0,ncol=3,nrow=length(p1))
for(i in 1:length(sort(p1)))
{
 
  M[i,]=specificity_sensitivity(sort(p1)[i])
  
}

prob=rep(sort(p1),3)
val=array(M)
Metrics=rep(c("Sensitivity","Specificity","Accuracy"),each=length(sort(p1)))
d7=data.frame(prob,val,Metrics);d7
plot=ggplot(d7,aes(x=prob,y=val,colour=Metrics))+geom_line()+labs(x="prob",y,title="Accuracy,Sensitivity and Specificity")
plot_data <- ggplot_build(plot)$data[[1]]

# Find the intersection point (assuming it exists)
intersection_x <- with(plot_data, prob[which.min(abs(val[Metrics == "Accuracy"] - val[Metrics == "Sensitivity"]) +
                                                   abs(val[Metrics == "Sensitivity"] - val[Metrics == "Specificity"]))])
intersection_y_accuracy <- with(plot_data, val[Metrics == "Accuracy"][which.min(abs(val[Metrics == "Accuracy"] - val[Metrics == "Sensitivity"]) +
                                                                                  abs(val[Metrics == "Sensitivity"] - val[Metrics == "Specificity"]))])
intersection_y_sensitivity <- with(plot_data, val[Metrics == "Sensitivity"][which.min(abs(val[Metrics == "Accuracy"] - val[Metrics == "Sensitivity"]) +
                                                                                        abs(val[Metrics == "Sensitivity"] - val[Metrics == "Specificity"]))])
intersection_y_specificity <- with(plot_data, val[Metrics == "Specificity"][which.min(abs(val[Metrics == "Accuracy"] - val[Metrics == "Sensitivity"]) +
                                                                                        abs(val[Metrics == "Sensitivity"] - val[Metrics == "Specificity"]))])

# Print the intersection point
cat("Intersection Point:\n")
cat("x:", intersection_x, "\n")
cat("Accuracy:", intersection_y_accuracy, "\n")
cat("Sensitivity:", intersection_y_sensitivity, "\n")
cat("Specificity:", intersection_y_specificity, "\n")

# Display the plot
print(plot)
p_threshold=intersection_x
y_pred=ifelse(p1>p_threshold,1,0);y_pred
table(y_pred,y_train)
accuracy=(951+1879)/length(y_pred);accuracy
precision=1879/(365+1879);precision
recall=1879/(723+1879);recall
f1_score=psych::harmonic.mean(c(precision,recall));f1_score
test_prob=predict.glm(m1,newdata=test%>%select(-c(id,chlorides,pH,citric.acid)),type="response")
y_t_p=ifelse(test_prob>p_threshold,1,0)
table(y_t_p,test$qual)
accuracy_test=(319+598)/length(test$qual);accuracy_test
precision_test=598/(598+123);precision_test
recall_test=598/(598+184);recall_test
f1_score_test=psych::harmonic.mean(c(precision_test,recall_test));f1_score_test
