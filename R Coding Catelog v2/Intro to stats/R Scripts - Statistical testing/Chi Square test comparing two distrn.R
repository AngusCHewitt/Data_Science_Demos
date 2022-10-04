# import 2 docs using the read table funnction

Ortho <- read.table("Ortho.csv",sep=",",header=TRUE)

Non_Ortho <- read.table("Non Ortho.csv",sep=",",header=TRUE)

# conduct 2 distributions of ortho and non ortho
## may need to adjust cate with expected counts <5 

chisq.test(Ortho[2:5],Non_Ortho[2:5])