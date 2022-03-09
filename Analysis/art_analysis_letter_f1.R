if(!require(ARTool)){install.packages("ARTool")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(rcompanion)){install.packages("rcompanion ")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(psych)){install.packages("psych")}

fileName = "f1.csv"
dataPath = paste(getwd(), "/2022_kasowski_immersive-master/art_anova/", sep="")
filePath = paste(dataPath, fileName, sep="/")

library(ARTool)

csv = read.csv(filePath)

csv$Subject = factor(csv$Subject, levels=unique(csv$Subject))
csv$Rho = factor(csv$Rho, levels=unique(csv$Rho))
csv$Lambda = factor(csv$Lambda, levels=unique(csv$Lambda))
csv$Device = factor(csv$Device, levels=unique(csv$Device))
csv$Display = factor(csv$Display, levels=unique(csv$Display))

transformed_csv = art(F1 ~ Device*Rho*Lambda*Display + (1|Subject), data=csv)

artAnova = anova(transformed_csv)
artAnova$eta.sq.part = with(artAnova, `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
artAnova

rho = art.con(transformed_csv, "Rho:Display")
lam = art.con(transformed_csv, "Lambda:Display")
dev = art.con(transformed_csv, "Device:Display")

write.csv(artAnova,  paste(paste(dataPath, "art", sep="/"), fileName, sep=""))
write.csv(rho,  paste(paste(dataPath, "rho_con_", sep="/"), fileName, sep=""))
write.csv(lam,  paste(paste(dataPath, "lam_con_", sep="/"), fileName, sep=""))
write.csv(dev,  paste(paste(dataPath, "dev_con_", sep="/"), fileName, sep=""))
