
library(tidyverse)
library(readxl)
library(writexl)

setwd("d:/covid")
answer = read.table(file = "ÒßÇéÎÊ¾í9001-9300.csv", header = T , 
                     sep = ",",fill = TRUE,skip = 2, encoding = "UTF-8")
info = read.table(file = "¸´µ©SZ_list_85Àý.csv", header = T,
                  sep = ",",fill = TRUE, encoding = "UTF-8")

#####±í¸ñºÏ²¢
r1 = nrow(answer)
r2 = nrow(info)
c1 = ncol(answer)
c2 = ncol(info)
answer = answer[,2:140]
colnames(info)[1] = "ÐòºÅ"
colnames(answer)[1] = "¸´µ©ID"

result = info %>%
  full_join(answer, by = "¸´µ©ID")

write.csv(result,"result.csv")

##### Data Clean
# manually examine each column and make sure they reflect their true meaning and comply with the R data structure
# 

data2 <- read.csv('crisis.csv')

data2$quan <- (data2$BG14 > 1)  # quanrantine
data2$isol <- (data2$CD46 == 1) # without any outdoor during the last weeks of the pandamic 
data2$dep.score <- rowSums(data2[,c(121:129)])   # MDD symptom --- BDI
data2$scz.score <- 15 - rowSums(data2[,c(130:134)]) # SCZ symptom --- CAPE
data2$followupinterval <- year(data2$dateQuestion) - year(data2$dateEnroll) 
data2$age <- data2$ageEnroll + year(data2$dateQuestion) - year(data2$dateEnroll)  # age at questionnaire
data2$diag <- data2$CD69

write_excel_csv(data2, 'crisis.csv')
###### Data Analyses

# Analysis 1: effects on current symptoms during the pandamic

YoI <- c("dep.score", "scz.score")
model1 <- 'diag*quan+ age + sex'

for (i in YoI){
  formula <- paste(i, model1, sep = '~')
  fit1 <- lm(formula, data=data2)
  print(summary(fit1)$coefficients[c('diagSCZ', 'quanTRUE', 'diagSCZ:quanTRUE'), 'Pr(>|t|)'])
}

p1 <- ggboxplot(data2, x = "diag", y = "scz.score",
          color = "diag", palette = "jco",
          add = "jitter") +
          stat_compare_means(method = "t.test") +
          theme(legend.position="none")


p2 <- ggboxplot(data2, x = "diag", y = "dep.score",
          color = "diag", palette = "jco",
          add = "jitter") +
          stat_compare_means(method = "t.test") +
          theme(legend.position="none")

p3 <- ggarrange(p1,p2, nrow=1, ncol=2)
ggsave('grpsczdep.pdf', p3, height = 3, width = 5)

# Analysis 2: effects on delta.score during the pandamic

VoB <- unlist(lapply(26:43, function(x){paste0("CD",x)}))
VoF <- unlist(lapply(44:61, function(x){paste0("CD",x)}))
numVoI <- length(VoB)
model1 <- 'diag*quan + age + sex'

data2$diag <- relevel(data2$diag, ref = 'HC')

results <- matrix(1, nrow=numVoI, ncol=5)
colnames(results) <- c('diagSCZ',
      'diagMDD',
      'quanTRUE', 
      'diagMDD:quanTRUE',
      'diagSCZ:quanTRUE')
rownames(results) <- descrip[unlist(lapply(VoB, function(x, y){ grep(x, colnames(y))}, data2))]

for (i in c(1:numVoI)){
    print(i)
    delta <- data2[,VoB[i]] - data2[,VoF[i]]
    formula <- paste0('delta ~ ', model1, '+', VoB[i])
    fit1 <- lm(formula, data=data2)
    results[i,] <- summary(fit1)$coefficients[
    c('diagSCZ','diagMDD','quanTRUE', 
      'diagMDD:quanTRUE','diagSCZ:quanTRUE'),
      'Pr(>|t|)']
}
results <- as.data.frame(results)
results$rownames <- rownames(results)
write_excel_csv(results, 'result.csv')

# plot for interaction effect

n = length(data2$diag)
data3 <- data.frame(
  ID = as.factor(c(data2$fdID, data2$fdID)),
  vedio_game = c(data2$CD41, data2$CD59), 
  alcohol_use = c(data2$CD42, data2$CD60),
  diag = c(data2$diag, data2$diag),
  visit = as.factor(c(rep("before", n), rep("during", n))),
  quarantine = c(data2$quan, data2$quan)
)

library(wesanderson)

p1 <- ggplot(data=data3, aes_string(x="visit", y="vedio_game", group="ID"))+
      geom_jitter(aes(colour=ID), show.legend = FALSE, size = 0.5) +
      geom_line(aes(linetype=quarantine)) + 
      geom_point(aes(shape=quarantine))  +
      facet_wrap( ~ diag) 

p2 <- ggplot(data=data3, aes_string(x="visit", y="alcohol_use", group="ID"))+
      geom_jitter(aes(colour=ID), show.legend = FALSE, size = 0.5) +
      geom_line(aes(linetype=quarantine)) + 
      geom_point(aes(shape=quarantine))  +
      facet_wrap( ~ diag) 

p12 <- ggarrange(p1,p2, nrow=2, ncol=1)
ggsave('lockdowneffect.pdf', p12)


p1 <- ggboxplot(data=data3, x="diag", y="vedio_game", color="visit", 
      add = "jitter") +
      facet_wrap( ~ quarantine) 

p2 <- ggboxplot(data=data3, x="diag", y="alcohol_use", color="visit",
      add = "jitter") +
      facet_wrap( ~ quarantine) 

p12 <- ggarrange(p1,p2, nrow=2, ncol=1)
ggsave('lockdowncompare.pdf', p12)

