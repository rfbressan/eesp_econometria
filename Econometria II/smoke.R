library(nleqslv)
library(dplyr)
library(lmtest)
library(zoo)
library(sandwich)
library(stargazer)

smoke<-read.csv('smoke.csv')
x<-select(smoke, lcigpric, lincome, white,
          educ, age, agesq, restaurn)
y<-select(smoke, cigs)
    
# Question a)

ols<-lm(cigs~lcigpric+lincome+white+educ+age+agesq+restaurn,
        data=smoke)
coeftest(ols, vcov=vcovHC)
stargazer(coeftest(ols, vcov=vcovHC))

# Question b)

equation<-function(beta, print.score=T){
  # Setting initial variables for logical
  score<-matrix(nrow=dim(x)[1], ncol=dim(x)[2])
  colnames(score)<-colnames(x)
  i=1
  while(i<=dim(x)[1]){
    # Just renaming for simplicity
    a<-as.matrix(x[i,])
    b<-as.matrix(y[i,])
    # Score computation
    score[i,]<-t(a)%*%exp(a%*%beta)-t(a)%*%b
    i<-i+1
  }
  FOC<-colSums(score)/dim(x)[1]
  if(print.score==F){
    print(FOC)}
  else{
    print(list(FOC=FOC, score=score))}
}

# Initial guess
start<-matrix(rep(0,dim(x)[2]))
equation(start)

# Solving the Non-Linear equation
solver<-nleqslv(start, equation, jacobian = T,
                method = 'Newton', print.score=F)

# Storing Coefficients
coefs<-as.matrix(solver$x)
rownames(coefs)<-colnames(x)
colnames(coefs)<-'Coefficients'

# Covariance Matrix Estimation
sigma<-equation(coefs,print.score = T)$score
sigma<-t(sigma)%*%sigma/dim(x)[1]
hessian<-function(beta){
  i=1
  H=matrix(0,dim(x)[2],dim(x)[2])
  colnames(H)=rownames(H)
  # Just renaming for simplicity
  a<-t(as.matrix(x[i,]))
  b<-as.numeric(exp(a%*%t(beta)))
  while(i<dim(x)[1]+1){
    H=H-b*a%*%t(a)
    i=i+1}
  print(H)
  }
H<-solver$jac

table<-matrix(NA, nrow=dim(coefs)[1], ncol=2)
rownames(table)<-rownames(coefs)
colnames(table)<-c('Coef','S.E')
table[,1]<-coefs
for(i in 1:dim(coefs)[1]){
  table[i,2]<-solve(sigma)[i,i]}

# Creating an objective function for evaluation
objective<-function(beta){
  # Setting initial variables for logical
  q<-0
  i=1
  while(i<=dim(x)[1]){
    # Just renaming for simplicity
    a<-as.matrix(x[i,])
    b<-as.matrix(y[i,])
    # Likelihood
    q<-q-exp(a%*%beta)+b%*%exp(a%*%beta)
    i<-i+1
  }
  print(q/dim(x)[1])
}

# Question c)
sigma_robust<-solve(H)%*%sigma%*%solve(H)
colnames(sigma_robust)<-colnames(sigma)
rownames(sigma_robust)<-rownames(sigma)

# Question d)
fitted<-vector('numeric',length=dim(y)[1])
for(i in 1:dim(y)[1]){
  fitted[i]<-exp(as.matrix(x[i,])%*%coefs)
}
resid<-y-fitted
lambda<-exp(as.matrix(x)%*%coefs)
var<-as.numeric(var(resid)/mean(lambda))
sigma_hat<-var*solve(sigma)
