library(xlsx)
data=read.xlsx(file.choose(),sheetName = "Sheet1")
data=data[,-1]
library(MASS)
library(quantreg)
library(lmtest)
library(robustbase)
install.packages("robustbase")
attach(data)
str(data)
#LMS
fitLMS=lqs(Pertumbuhan.Ekonomi~.,data=data,method = "lms")
#LTS
#fitLTS=lqs(Pertumbuhan.Ekonomi~.,data=data,method = "lts")
fitLTS2=ltsReg(Pertumbuhan.Ekonomi~.,data=data)
#S-Estimation
fitS=lqs(Pertumbuhan.Ekonomi~.,data=data,method = "S")
fitS2=lmrob.S(x=c(PDB,Impor,Pengangguran,Kepadatan.Penduduk),y=Pertumbuhan.Ekonomi)
#MM-Estimation
fitMM=rlm(Pertumbuhan.Ekonomi~.,data=data,method = "MM")
fitMM2=lmrob(Pertumbuhan.Ekonomi~.,data=data,method="MM")
fitM=rlm(Pertumbuhan.Ekonomi~.,data=data)
