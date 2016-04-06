library(RCAMx)

FILE = "/t3/camx/12US2_12_25/output/2011/2011EH_ANN/2011125.12US2.12.25.2011EH_ANN.ld.camx.avrg"

DAT = READ_CAMx_2DAVG(FILE)

#print(DAT[1:20,])
write.csv(DAT, file="TEST.csv")
