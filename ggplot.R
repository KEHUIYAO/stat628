
#箱线图
dat0['num'] = 1:252


p1 = ggplot(dat0, aes(x = factor(1),y = BODYFAT)) + geom_boxplot(outlier.size = 1.5, outlier.shape = 21, fill = 'grey') + stat_summary(fun.y = 'mean', geom = 'point', shape = 22, size = 10, fill = 'lightblue') + 
xlab("") + ylab("BODYFAT")+
  annotate("text",x=factor(1),y=dat$BODYFAT,label=ifelse(dat$BODYFAT>40, dat$num,""),vjust=1,color="red")
p1

p2 = ggplot(dat0, aes(x = factor(2),y = HEIGHT)) + geom_boxplot(outlier.size = 1.5, outlier.shape = 21, fill = 'grey') + stat_summary(fun.y = 'mean', geom = 'point', shape = 22, size = 10, fill = 'lightblue') + 
xlab('') + ylab("HEIGHT") 
p2

p3 = ggplot(dat0, aes(x = factor(3),y = BODYFAT)) + geom_boxplot(outlier.size = 1.5, outlier.shape = 21, fill = 'grey') + stat_summary(fun.y = 'mean', geom = 'point', shape = 22, size = 10, fill = 'lightblue') + 
  xlab("") + ylab("BODYFAT")+
  annotate("text",x=factor(3),y=dat$BODYFAT,label=ifelse(dat$BODYFAT>260, dat$num,""),vjust=1,color="red")
p3


grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

#箱线图

#残差图
res0 <-  lm(BODYFAT~DENSITY, data=dat0)$residuals;index <- which( abs(res0) > 1 );indexes=index
df = data.frame(num = 1:252, res = res0)
p4 = ggplot(df, aes(x = num, y = res)) + geom_point( alpha=0.9,size=1)+
  geom_hline(aes(yintercept = 0), color = 'blue', linetype = 'dashed') + labs(title="The difference of real and estimate Bodyfat", x="ID", y="Difference of Bodyfat")+theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text",x=df$num,y=df$res,label=ifelse(df$res>1|df$res< -1, dat0$num,""),vjust=1,color="red") 
p4
res1=lm(BODYFAT~DENSITY, data=dat0[-96,])$residuals;index <- which( abs(res0) > 1 );indexes=index

df = data.frame(num = 1:252, res = res0)
p5 = ggplot(df, aes(x = num, y = res)) + geom_point( alpha=0.9,size=1)+
  geom_hline(aes(yintercept = 0), color = 'blue', linetype = 'dashed') + labs(title="The difference of real and estimate Bodyfat", x="ID", y="Difference of Bodyfat")+theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text",x=df$num,y=df$res,label=ifelse(df$res>1|df$res< -1, df$num,""),vjust=1,color="red") 
p5

outlier_bodyfat <- c(96,48,76,182,216,33);train <- dat[-outlier_bodyfat,];test <- dat[outlier_bodyfat,]
pre <- predict(lm(BODYFAT~I(1/AGE), data=train),test)
round(rbind(Record=test$BODYFAT, Prediction=pre, Bias=test$BODYFAT-pre, Bias_Over_Record=(test$BODYFAT-pre)/test$BODYFAT ),2)

#固定点的箱线图
index = c()
value = c()
for (i in 2:dim(dat)[2]){
  index = c(index, rep(names(dat)[i], 247))
  value = c(value, dat[, i])}
df = data.frame(index = index, value = value)
bplot <- function(n){
  temp = data.frame(name = names(dat),val = unlist(dat[182,]))
  p = ggplot(df, aes(x = index, y = value)) + geom_boxplot(outlier.size = 1.5, outlier.shape = 21, fill = 'grey') + stat_summary(fun.y = 'mean', geom = 'point', shape = 22, size = 2, fill = 'lightblue') + 
xlab("")+ylab(n)+ geom_point(data = temp, aes(x = name, y = val), color = 'red', fill = 'red')
p

}

bplot(182)
bplot(216)
bplot(96)


#最后两个图 
par(mfrow=c(1,2))
res0 <-  lm(ADIPOSITY~I(WEIGHT/(HEIGHT)^2), data=dat0[,-17])$residuals;index <- which( abs(res0) > 10 )
df = data.frame(num = 1:252, res = res0)
p6 = ggplot(df, aes(x = num, y = res)) + geom_point( alpha=0.9,size=1)+ labs(title="The difference of real and estimate ADIPOSITY", x="ID", y="Difference of ADIPOSITY")+theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(aes(yintercept = 0), color = 'blue', linetype = 'dashed') + 
   annotate("text",x=dat0$IDNO,y=df$res,label=ifelse(abs(df$res)>10 , dat0$IDNO,""),vjust=1,color="red")
p6

outlier_bmi <- c(39,48,76,182, 216)
res1 <-  lm(ADIPOSITY~I(WEIGHT/(HEIGHT)^2), data=dat[-outlier_bmi,])$residuals;index <- which( abs(res1) > .2 )
num = 1:248
num = num[-c(39,48,76,182, 216)]
df = data.frame(num = num, res = res1)
p7 = ggplot(df, aes(x = num, y = res)) + geom_point(shape = 21, size = 2, fill = 'lightblue', color = 'black')+
  geom_hline(aes(yintercept = 0), color = 'red', linetype = 'dashed') + 
   annotate("text",x=df$num,y=df$res,label=ifelse(abs(df$res)>15, df$num,""),vjust=1,color="red")
p7

#