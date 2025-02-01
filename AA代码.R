rm(list = ls()) #清空数据
summary(k1)
str(k1)
library(tableone) # 加载包
CreateTableOne(data = k1)  # 汇总整个数据集特征
dput(names(k1)) # 输出abcc数据集变量名称
k1$group<-as.factor(k1$group)
k1$sex<-as.factor(k1$sex)
k1$mallampati<-as.factor(k1$mallampati)
k1$hypertension<-as.factor(k1$hypertension)
k1$diabetes<-as.factor(k1$diabetes)
k1$coronary<-as.factor(k1$coronary)


library(tidyverse)
fct_relevel(k1$dlt1,c("37","35","32")) %>% summary(32,35,37)
k1fct_rev(k1$dlt1)%>% summary()
k1$cormack<-k1$`cormack-lehane`
#计算 smd 标准化均值差
# 安装和加载相关包
install.packages("MatchIt")
library(MatchIt)

formular1<-group ~age+sex+height+weight+bmi++mallampati+abdominal+questionnaire+
  hypertension+diabetes+coronary+induction+operation+awake
m_out<-matchit(formular1, data = k1,
               method = "nearest", distance = "glm",m.order = "random",caliper = 0.1)
psm_out<-match.data(m_out)
summary(m_out)
plot(summary(m_out))

confounders <- c( "age", "sex", "height",  "weight", "bmi","mallampati","abdominal",
                  "questionnaire",  "hypertension", "diabetes", "coronary",   
                 "induction", "operation", "awake") 

# var=confounders 说明着我们将总结混杂因素的数据。
# strata='treatment' 表示变量将按治疗类型分组。
# test=TRUE 表示将报告显着性检验的 p 值。
# smd=TRUE 表示将报告标准化平均差。
table1 <- CreateTableOne(var=confounders, strata='group', data=k1, test=TRUE)
print(table1, smd=TRUE)
s1<-print(table1, smd=TRUE)
write.csv(s1,file = "smd.csv")

#中位数差置信区间
install.packages("pairwiseCI")
library(pairwiseCI)
time$group<-as.factor(time$group)
time$group1<-ifelse(time$group=="Lateral",1,0)
difmedian <-pairwiseCItimedifmedian <-pairwiseCI(intubation~group1, data=time,
                       alternative = "two.sided",
                       conf.level = 0.95, method ="Median.diff")
difmedian



# 计算中位数差的置信区间(实验组减去对照组，group里面实验组是0，对照组是1)
wilcox_result <- wilcox.test(intubation~group, data=time, mu=0,exact = T,
                             conf.int = T, conf.level = 0.95)

# 打印结果
wilcox_result

round(2.2e-16,3)#保留三位小数

#分组计算中位数(分组统计分析)
aggregate(time$intubation, list(time$group), summary)


#累计曲线
library(ggplot2)
library(survival)
library(survminer)

time$success<-as.numeric(time$success)
ggsurvplot(survfit(Surv(intubation,success)~group,data=time),fun='cumhaz',pval=TRUE,risk.table = TRUE)
ggsurvplot(survfit(Surv(intubation,success)~group,data=time),
           fun = "event", #l绘制1-S(t)曲线
           #add.all = TRUE,#总体曲线
           #conf.int = TRUE,  #输出曲线置信区间
           pval = TRUE, # 设置添加P值
           pval.method = TRUE, #设置添加P值计算方法
           conf.int.style="ribbon"  ,#可定义的类别包括("ribbon", "step")
           conf.int.alpha=0.25,  #设置置信区间透明区
           surv.plot.height= 0.7, #生存图高度
           main = "Proportion of Successfully Intubated Patients",
           legend.title = "Group",
           legend.labs = c("Supine", "Lateral"),
           legend=c(0.8,0.8),
           xlab="iutubation",
           ylab="Proportion of Successfully Intubated Patients (%)", #或者直接写“1-S(t)”
           xlim = c(0,800),
           ylim = c(0,1),
           risk.table = TRUE,
           tables.height = 0.15, # 生存曲线图下所有生存表的高度，数值0-1之间
           tables.theme = theme_cleantable(),
           font.main = c(12, "bold", "darkblue"),
           font.x = c(12, "plain", "black"),
           break.x.by = 100,
           break.y.by = 0.2,
           font.y = c(12, "plain", "black"),
           font.tickslab = c(12, "plain", "black"),
           risk.table.col="strata",
           risk.table.height=0.2,
           palette = "jco",#可选调色板有 "grey","npg","aaas","lancet","jco","ucscgb","uchicago","simpsons"和"rickandmorty".
           #ggtheme = theme_bw() # Change ggplot2 theme
)

#Proportion of First Successfully Intubated Patients (%)
#Duration of Intubation Process, second



