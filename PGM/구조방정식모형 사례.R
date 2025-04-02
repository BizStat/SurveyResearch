library(lavaan)
library(semPlot)

#install.packages("lavaanPlot",dependencies = TRUE)
#install.packages("devtools")
#devtools::install_github("alishinski/lavaanPlot")

library(lavaanPlot)


# 데이터 생성
Corr <- ' 1.000
          0.418 1.000
          0.394 0.627 1.000
          0.129 0.202 0.266 1.000
          0.189 0.284 0.208 0.365 1.000
          0.544 0.281 0.324 0.201 0.161 1.000
          0.507 0.225 0.314 0.172 0.174 0.546 1.000
          -.357 -.156 -.038 -.199 -.277 -.294 -.174 1.000 '
StdDev =  c(2.09,3.43, 2.81, 1.95, 2.06, 2.16, 2.06, 3.65)
# VarNM = c('JS1','JS2','PF','MV1','MV2','WS1','WS2','DA')
VarNM = c('PF','JS1','JS2','MV1','MV2','WS1','WS2','DA')         
Cov <- getCov(Corr, names = VarNM, sds = StdDev)
# Cov <- cor2cov(Corr,StdDev,names = VarNM)

# 모형식 입력
model.1 <- '
    # 잠재변수 정의
      xi1 =~ MV1 + MV2
      xi2 =~ WS1 + WS2
      xi3 =~ DA
      eta2 =~ JS1 + JS2
      eta1 =~ PF
    # 회귀식
      eta1 ~ xi2
      eta2 ~ xi1 + xi3 + eta1
    # 분산-공분산 설정
      xi1 ~~ xi2
      xi2 ~~ xi3
      xi3 ~~ xi1
'

# 모형 추정
mod.est <- sem(model.1, sample.cov = Cov, sample.nobs = 122)

fitMeasures(mod.est,fit.measures=c("chisq","rmsea","gfi","agfi"))
semPaths(mod.est, layout="tree2", sizeMan=7, residuals=TRUE, whatLabels = "est", nCharNodes = 12, normalize=TRUE, width=12, height=6)
lavaanPlot(model = mod.est)
lavaanPlot(model = mod.est, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)

modindices(mod.est)

summary(mod.est)

parameterEstimates(mod.est, standardized=TRUE)


# test
model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = FALSE)
pl <- lavaanPlot(model = fit)
embed_plot_pdf(pl, "plot2.pdf")
