library("glmnet")
dat0=read.csv("BodyFat.csv")

dat=dat0
BodyFat <- dat0
fat1 <- BodyFat
fat1[42, 6] <- 69.50
fat=fat1[-c(39,48,76,182, 216),-3 ]
dat=fat

##Lasso regression
x=as.matrix(dat[,-c(1,2)])
fit = glmnet(x, dat[,2], family="gaussian", nlambda=800, alpha=1)
print(fit)
coef(fit, fit$lambda[149])#1 variable
coef(fit, fit$lambda[185])#2 variable
coef(fit, fit$lambda[25])#3 variable
coef(fit, fit$lambda[278])#4 variable


### Mallow's Cp

model=lm(BODYFAT ~. ,data=dat[,-1])
X = model.matrix(model)[,-1]
Y = dat[,2]
library("leaps")
library("faraway")
g=leaps(X,Y)
Cpplot(g)
g=leaps(X,Y,nbest = 1)
Cpplot(g)
#1,3,6,7,12,14
cp.choice=c(1,3,6,7,12,14)+2
dat.cp=dat[,c(2,cp.choice)]

model.cp <- lm(BODYFAT~.,data = dat[,c(2,cp.choice)])
summary(model.cp )
model.cp.right=lm(dat$BODYFAT~dat$ABDOMEN+dat$WRIST)
summary(model.cp.right)


#Adj R^2
g=leaps(X,Y,nbest=1,method="adjr2")
plot(g$adjr2)
(g$which)[which(g$adjr2==max(g$adjr2)),]
r2.choice=c(1,3,5:9,13,14)+2
model.r2=lm(BODYFAT~., data=dat[,c(2,r2.choice)])
summary(model.r2)


##BIC Forward
BodyFat <- dat0
fat1 <- BodyFat
fat1[42, 6] <- 69.50
fat=fat1[-c(39,48,76,182, 216),-c(3,1) ]
dat=fat
n <- dim(fat)[1]

model.forward<-lm(BODYFAT~1,data=fat)
model.forward.BIC<-step(model.forward,direction="forward", k=log(n), scope=list(lower = ~1,                                         upper = ~AGE+WEIGHT+HEIGHT+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST), data=fat)

mb <- model.forward.BIC#3 variable
summary(mb)
mb.most=lm(fat$BODYFAT~fat$WEIGHT+fat$ABDOMEN)#2 variable
summary(mb.most)
#
model.BIC=step(model,k=log(246),direction ="backward",trace=F )
summary(model.BIC)
dat$WEIGHT=dat$WEIGHT*0.453592
final=lm(dat$BODYFAT~dat$ABDOMEN+dat$WEIGHT)
summary(final)
