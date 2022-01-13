#install.packages("ARTool")

fileName = "time.csv"
dataPath = "C:\\Users\\sbjsm\\Documents\\r_analysis\\"
filePath = paste(dataPath, fileName, sep="")

library(ARTool)

csv = read.csv(filePath)

csv$Subject = factor(csv$Subject, levels=unique(csv$Subject))

csv$Rho = factor(csv$Rho, levels=unique(csv$Rho))

csv$Lambda = factor(csv$Lambda, levels=unique(csv$Lambda))

csv$Device = factor(csv$Device, levels=unique(csv$Device))

csv$Display = factor(csv$Display, levels=unique(csv$Display))

transformed_csv = art(Time ~ Device*Rho*Lambda*Display + (1|Subject), data=csv)

anova(transformed_csv)

art.con(transformed_csv, "Rho")