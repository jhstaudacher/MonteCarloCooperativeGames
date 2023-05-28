# weightedVotingGame for IMF2015
total <- read.csv(file = 'imf_2015.txt', sep=";")$Total
print(total)
v <- weightedVotingGameForSampling(total, 0.5)

#P=list(c(1:24), c(25:53), c(54:84), c(85:103), c(104:117), c(118:133), c(134:151), c(152:169), c(170:188))

# Results for IMF2015 for Shapley and Banzhaf
shap <- read.csv(file = 'IMF2015Res.csv', sep=" ")$simp2015Shap
shap <- as.numeric(sub("%", "", shap))
shap <- shap/100
print(shap)

banz <- read.csv(file = 'IMF2015Res.csv', sep=" ")$simp2015Banz
banz <- as.numeric(sub("%", "", banz))
banz <- banz/100
print(banz)


parContext <- list(24, 29, 31, 19, 14, 16, 18, 18, 19)
P <- list()
cnt <- 1
acc <- 1
for(i in parContext){
  print(acc:(acc+i-1))
  P[[cnt]] <- acc:(acc+i-1)
  acc <- acc + i
  cnt <- cnt + 1
}
P

# Testing
R <- list()

for(i in 1:188){
  if(i%%10 == 0)print(i)
  c <- confidenceBanzhaf(i, 188, v, 0.90, 0.05)
  R[i] <- (c[1]+c[2])/2
}
R <- unlist(R)/sum(unlist(R))

# Error
print(banz-R)

owen <- approOwen(188, 100000, v, P)
print(owen)
print(shap)

shapley <- approShapley(188, 100000, v)
print(shapley)
print(shap)


# Todo BigZ * float = 0; potentiell problem bei ApproOwen und stApproWenAndBanzafOwen

R <- list()

for(i in 1:188){
  if(i%%10 == 0)print(i)
  c <- confidenceShapleyShubik(i, 188, v, 0.90, 0.05)
  R[i] <- (c[1]+c[2])/2
}
R <- unlist(R)/sum(unlist(R))

# Error
print(banz-R)


as.numeric(stApproOwenAndBanzhafOwen(180, as.bigz(100000), v, P)$'Banzhaf-Owen')

confidenceBanzhaf(180, 188, v, 0.95, 0.005)

print(twoStageStApproShapleyOptCor(188, v, 1000000))


twoStageApproBanzhafOwen(180, 256, 2000, v, P)
