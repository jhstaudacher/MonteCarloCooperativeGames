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

owen <- approOwen(188, 100000, v, list(c(1:188)))
print(owen)
print(shap)

shapley <- approShapley(188, 100000, v)
print(shapley)
print(shap)


R <- list()

for(i in 1:188){
  if(i%%10 == 0)print(i)
  c <- confidenceShapleyShubik(i, 188, v, 0.90, 0.05)
  R[i] <- (c[1]+c[2])/2
}
R <- unlist(R)/sum(unlist(R))

# Error
print(banz-R)


stApproOwenAndBanzhafOwen(180, 1000000, v, list(c(1:188)))

confidenceBanzhaf(180, 188, v, 0.95, 0.005)

print(twoStageStApproShapleyOptCor(188, v, 1000000))


twoStageApproBanzhafOwen(180, 1, 100, v, list(c(1:188)))
