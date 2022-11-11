library(readr)
library(dplyr)
library("tidyr")
'%!in%' <- function(x,y)!('%in%'(x,y))
store1=NULL
folder='C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Machine Learning Project\\Melting Point Point Prediction\\Datasets\\SE_descriptor_Nitrile_dataset\\training'
files=list.files(folder,'.log')
for (k in 1:length(files)) {
  tryCatch({
    X76 <- read_csv(paste0("C:/Users/skpandey1/OneDrive - The University of Alabama/Machine Learning Project/Melting Point Point Prediction/Datasets/SE_descriptor_Nitrile_dataset/training/",k,".log"))
    
    #Dipole moment extraction
    dp=grep('Dipole moment' ,X76$`Entering Gaussian System`)
    dp_value=as.numeric(strsplit(as.character(X76[dp+1,1]),'Tot=')[[1]][2])
    
    mulli=grep('Mulliken' ,X76$`Entering Gaussian System`)
    
    
    dd=as.data.frame(X76[(mulli[1]+1):(mulli[2]-1),1])
    colnames(dd)='col1'
    dd1=dd %>%
      separate(col1, c("col1", "col2"), "   ")
    max1=max(as.numeric(na.omit(dd1$col2)))
    min1=min(as.numeric(na.omit(dd1$col2)))
    
    homo=grep('Alpha  occ. eigenvalues --' ,X76$`Entering Gaussian System`)
    dd3=X76[homo,1]
    colnames(dd3)='col1'
    dd3_1=dd3 %>%
      separate(col1, c("col1", "col2"), "--   ")
    
    dd3_1=dd3_1[,2]
    ss=NULL
    for (i in 1:nrow(dd3_1)) {
      ss=append(ss,as.numeric(strsplit(as.character(dd3_1[i,1]),'  ')[[1]]))
    }
    tail(ss,5)
    
    homo=grep('Alpha virt. eigenvalues --' ,X76$`Entering Gaussian System`)
    dd3=X76[homo,1]
    colnames(dd3)='col1'
    dd3_1=dd3 %>%
      separate(col1, c("col1", "col2"), "--   ")
    
    dd3_1=dd3_1[,2]
    ss5=NULL
    for (i in 1:nrow(dd3_1)) {
      ss5=append(ss5,as.numeric(strsplit(as.character(dd3_1[i,1]),'  ')[[1]]))
    }
    head(ss5,5)
    
    scf=grep('SCF Done' ,X76$`Entering Gaussian System`)
    dd4=X76[scf,1]
    colnames(dd4)='col1'
    dd4_1=dd4 %>%
      separate(col1, c("col1", "col2"), "=")
    scdf=strsplit(dd4_1$col2,'A.U.')[[1]][1]
    
    
    store1=rbind(store1,c(k,dp_value,tail(ss,5),head(ss5,5),scdf,min1,max1))
  }, error=function(e){})
  
    

}
store1=as.data.frame(store1)
colnames(store1)=c('index','DipoleMoment','HOMO-1','HOMO-2','HOMO-3','HOMO-4','HOMO-5','LUMO-1','LUMO-2','LUMO-3','LUMO-4','LUMO-5','SCF','Mulliken_min','Mulliken_max')
ddddd=which(seq(1,length(files),1) %!in% store1$index)
if(length(ddddd)!=0){

jhgj=matrix(NA,nrow = 1,ncol = ncol(store1))
store2=store1
hh=0
for (l in 1:length(ddddd)) {
  
store2 <- InsertRow(store2, NewRow = jhgj, RowNum = ddddd[l]+hh)
hh=hh+1
}
write.csv(store2, "Nitrile_SE.csv")
  
}else{write.csv(store1, "Nitrile_SE.csv")}
