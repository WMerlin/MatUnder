


setwd("D:\\dydaktyka\\Underpinnings\\bnlearn_lab")

library(bnlearn)

# an example downloaded from https://www.bnlearn.com/bnrepository/
alarm_bn = readRDS("alarm.rds")
# the structure of the network
graphviz.plot(alarm_bn)
# get Markov Blanket for the node 'CATECHOL'
mb_catechol = mb(alarm_bn, 'CATECHOL')
## -> "INSUFFANESTH" "TPR"          "SAO2"         "ARTCO2"       "HR"     
# draw a sample from a joint distribution following dependencies shown in DAG you've drawn
set.seed(1234)
df = rbn(alarm_bn, n=1000)

# let's estimate MB of 'CATECHOL'
res_iamb_spmi = learn.mb(df, node='CATECHOL', method='iamb', test='sp-mi')
# "HR"         "TPR"        "ARTCO2"     "INTUBATION"
learn.mb(df, node='CATECHOL', method='gs', test='sp-mi', B=100)
learn.mb(df, node='CATECHOL', method='gs', test='mc-mi', B=100)

tpr = sum(res_iamb_spmi %in% mb_catechol)/length(mb_catechol)
fdr = 1 - sum(res_iamb_spmi %in% mb_catechol)/length(res_iamb_spmi)
tpr
fdr


methods_list = c('gs', 'iamb')
tests_list = c('mi', 'mc-mi', 'sp-mi')

results_tpr = matrix(0, 2, 3, dimnames=list(methods_list, tests_list))
results_fdr = matrix(0, 2, 3, dimnames=list(methods_list, tests_list))
results_f1 = matrix(0, 2, 3, dimnames=list(methods_list, tests_list))


tpr <- function(pp, p){
  return(sum(pp %in% p)/length(p))
}
fdr <- function(pp, p){
  return(1 - sum(pp %in% p)/length(pp))
}
f1 <- function(tpr, fdr){
  return(2*(1-fdr)*tpr/(1-fdr+tpr))
}

mb_hr = mb(alarm_bn, 'HR')

for(i in 1:2){
  for(j in 1:3){
    if(tests_list[j] == 'mi'){
      res_tmp = learn.mb(df, node='HR', method=methods_list[i], test=tests_list[j])
    }else{
      res_tmp = learn.mb(df, node='HR', method=methods_list[i], test=tests_list[j], B=100)
    }
    
    results_tpr[i,j] = tpr(res_tmp, mb_hr)
    results_fdr[i,j] = fdr(res_tmp, mb_hr)
    results_f1[i,j] = f1(results_tpr[i,j], results_fdr[i,j])
  }
}


results_tpr
results_fdr
results_f1
