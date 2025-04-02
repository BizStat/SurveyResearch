
# 데이터 생성
stddev = c(0.898,5.560,6.394,10.628,9.006,8.439)
corr = c(1.000, -0.127,  0.103,  0.068,  0.136,  0.134,
        -0.127,  1.000, -0.411, -0.225, -0.342, -0.297,
         0.103, -0.411,  1.000,  0.085,  0.285,  0.275,
         0.068, -0.225,  0.085,  1.000,  0.252,  0.143,
         0.136, -0.342,  0.285,  0.252,  1.000,  0.433,
         0.134, -0.297,  0.275,  0.143,  0.433,  1.000 )

corr_mat = matrix(corr,nrow=6)
stddev = diag(stddev)

cov_mat = stddev %*% corr_mat %*% stddev

cov_val = array(dim=21)

k = 1
for(i in 1:6) {
  for(j in 1:i) {
    cov_val[k] = cov_mat[i,j]
    k = k+1
  }
}

library(lavaan)
library(semPlot)

cov.mat = getCov(cov_val,names = c('후기물질주의','현실주의','규범','지각','의도','행위'))
ex1.mod1 = '
  # regression model
  행위 ~ 후기물질주의 + 현실주의 + 의도
  의도 ~ 후기물질주의 + 현실주의 + 규범 + 지각
  규범 ~ 후기물질주의 + 현실주의
  지각 ~ 후기물질주의 + 현실주의
  # correlation
  
'

fit <- sem(ex1.mod1, sample.cov= cov.mat, sample.nobs= 5037)
fitMeasures(fit)
fitMeasures(fit,fit.measures=c("chisq","gfi","rmr","rmsea","nnfi","nfi","cfi","agfi","pnfi","pgfi"))
summary(fit)
summary(fit,standardized=TRUE)

semPaths(fit, style="lisrel", rotation=2, sizeMan=7, whatLabels="par")
