#version_3
#fixed the sample switch issue with this code: edit GUID filename: identifier(KNNresult_fcs)<-paste("KNN_",keyword(HIV_fcs)$"$FIL",sep="")

#version2
#1.This code can handle a T,P sets with slight difference in the junk/remov parameters. It allows you MIX files with different Junk parameters (useful parameters must have the same names!).
#2.I imporved the stainName setting, the previous version require T,P to be 100% match, now P can be different than T.
#2.line 95, fixed a small bug to improve speed, the bug dosent affect the final result.
#3.removed un-nesessary switch colnames at line 165-168
#4.Fixed a bug: Should be able to run KNN before tSNE, please double check the result in flowjo when you use it for the first time though.


#Run me and follow what cow says (Just a guide for ppl to start)
{library(cowsay)
  WD <- getwd()
  whatcowsay<-paste("\nmoooooooooo.......\n1.Are your files in:\n(",
                    WD,
                    ")?\n******************************\nIf not,change the path to your folder ( ctrl+shift+H )!
                    \n2.Type in all file names and k=? between the ### lines.\n
                    3.When you done, run all codes.(ctrl+A,RUN) \nMoooooooooo........")
  say(whatcowsay,by="cow")}

#not important...just incase ppl write yes/no insdead of T and F
yes<-T
no<-F
YES<-T
NO<-F



#Settings:  (fill up the file names and required info below)
                  
Training_FCS_filename<-"T_D0_Ungated_viSNE"     # <-----DONT include ".fcs" !!!!!
Predictory_FCS_filename<-"P_D2_Ungated_viSNE" 
Result_FCS_filename<-"JN9_KNN"
k=1    #(k should be >=1) 

#**********************************************************************************************
#additional settings:(you may want to ask xiaoyu before you make changes in the lower sections)*
#**********************************************************************************************
Junk_parameters<-c("190BCKG","138Ba","140Ce","142Ce","Event_length",
                   "191Ir_DNA1","193Ir_DNA2","176Lu","208Pb","194Pt_cisplatin_1",
                   "195Pt_cisplatin_2","198Pt_cisplatin_3","131Xe","tSNE1","tSNE2","Time")

HIVrelated_parameters<-c("209Bi_KC57","150Nd_gag_mix")

Remov_parameters<-c("174Yb_CD4 (v)" ,"157Gd_CD8_14_19","146Nd_CD95")
#Remov_parameters <- Unbiased_Removal("day2_activated_memory.fcs","T_D0_Ungated_viSNE.fcs","means",5)

keep_old_HIV_parameters=yes #if yes, will keep the predictor's"HIV related parameters"unchanged.Otherwise, change to no
Need_parental=yes #do you need your training and predictory parental files copied to the result folder?





########################    DON'T CHANGE ANYTHING BELOW !!!!!!    #####################

#assign Objects
Switch_KC57_GAG_Parameters<-keep_old_HIV_parameters
library(flowCore)
library(expss)
library(class)
library(openxlsx)
#add.filetype to the file name
T_FCS<- paste(Training_FCS_filename,".fcs",sep="")     
P_FCS<-paste(Predictory_FCS_filename,".fcs",sep="") 
Final_result_filename<-paste(Result_FCS_filename,"_k",k,sep="") 
FINAKNN_FILENAME<-paste(Final_result_filename,".fcs",sep="") 

#upload file T
Original_Training_FCS<-read.FCS(T_FCS,
                                transformation="linearize", which.lines=NULL, 
                                alter.names=FALSE, column.pattern=NULL,truncate_max_range = FALSE)
Training_FCSinuse<-Original_Training_FCS

#T:add staining name and convert to df
T_pdata_df<-pData(parameters(Training_FCSinuse))
stainName<-as.character(T_pdata_df$"desc")
colnames(Training_FCSinuse)
colnames(Training_FCSinuse)<-stainName
colnames(Training_FCSinuse)
T_exprsDM<-exprs(Training_FCSinuse)
T_FCS_df<-as.data.frame(T_exprsDM)

#Asinh transformation
Training_Asinh<-asinh(T_FCS_df)
parameters_all<-T_FCS_df[0,]
parameters_all

#remove selected parameters
remo_all<-c(Junk_parameters,HIVrelated_parameters,Remov_parameters)
Training_Asinh_remo<-Training_Asinh[, !(colnames(Training_Asinh) %in% remo_all)]

##Review final parameters in console now, if they look ok
##UPLOAD FILE P
Original_Predictory_FCS<-read.FCS(P_FCS,
                                  transformation="linearize", which.lines=NULL, 
                                  alter.names=FALSE, column.pattern=NULL)
Predictory_FCSinuse<-Original_Predictory_FCS
#P:add staining name and convert to df
P_pdata_df<-pData(parameters(Predictory_FCSinuse))
stainName2<-as.character(P_pdata_df$"desc")
colnames(Predictory_FCSinuse)
colnames(Predictory_FCSinuse)<-stainName2
colnames(Predictory_FCSinuse)
P_exprsDM<-exprs(Predictory_FCSinuse)
P_FCS_df<-as.data.frame(P_exprsDM)
head(P_FCS_df)

Predictory_Asinh<-asinh(P_FCS_df)
parameters_all_pre<-P_FCS_df[0,]
Predictory_Asinh_remo<-Predictory_Asinh[, !(colnames(Predictory_Asinh) %in% remo_all)]
Training_Asinh_remo_CellID <- data.frame(names = row.names(Training_Asinh_remo), Training_Asinh_remo)
cellID_colname<-c("CellID", colnames(Training_Asinh_remo))
colnames(Training_Asinh_remo_CellID)<-cellID_colname

train<-Training_Asinh_remo_CellID[,2:ncol(Training_Asinh_remo_CellID)]
test<-Predictory_Asinh_remo
cl<-Training_Asinh_remo_CellID[,1]
dim(train)
dim(test)
length(cl)
lookupcode<-knn1(train, test, cl)
lookupcode_vn<-as.numeric(paste(lookupcode))
print(lookupcode_vn)
###########LOOP################
x<-1
reportdf<-data.frame(t(lookupcode_vn))

while(k-x>0){
  if(x==1){lookupcode_rep<-lookupcode_vn
  loop_train<-Training_Asinh_remo_CellID}
  
  loop_train<-loop_train[!loop_train$CellID %in% lookupcode_rep, ] #remove previous knn result
  
  train<-loop_train[,2:ncol(loop_train)]
  test<-Predictory_Asinh_remo
  cl<-loop_train[,1]
  dim(train)
  dim(test)
  length(cl)
  lookupcode_rep<-knn1(train, test, cl)
  lookupcode_repvn<-as.numeric(paste(lookupcode_rep))
  print(paste("KNN",x+1,"repeat result is:"))
  print(lookupcode_repvn)
  reportdf<-rbind(reportdf,lookupcode_repvn)
  lookupcode_vn<-c(lookupcode_vn,lookupcode_repvn)
  print("result add to the lookupcode:")
  print(lookupcode_vn)
  x<-x+1}
cat("KNN repeats:",x,"times\nEach run:",length(lookupcode),"\nTotal num of KNN found:",length(lookupcode_vn))
rowname_rdf<-paste0("Repeats",c(1:k)) 
row.names(reportdf)<-rowname_rdf

############LOOP###################

#inputKNN trainingset.fcs and extract the exprs to df
KNNresult_fcs<-Original_Training_FCS
HIV_fcs<-Original_Predictory_FCS

#creat a new folder
dir.create(Result_FCS_filename)
#change wd
setwd(paste(WD,"/",Result_FCS_filename,sep=""))

#write parental fcs

if(Need_parental){
  write.FCS(KNNresult_fcs,paste("T_",keyword(KNNresult_fcs)$"$FIL",sep=""), what="numeric")
  write.FCS(HIV_fcs,paste("P_",keyword(HIV_fcs)$"$FIL",sep=""), what="numeric")}

#prepare for lookup
original_DM<-exprs(KNNresult_fcs)
row.names(original_DM)<-c(1:nrow(original_DM))
original_df<- as.data.frame(t(original_DM))
original_df<-as.data.frame(original_DM, row.names =row.names(original_DM))

#lookup
KNNlookup_result_df<-vlookup_df(lookupcode_vn, original_DM,result_column =NULL, lookup_column = "names")
KNNlookup_result_df2<-KNNlookup_result_df[, !(colnames(KNNlookup_result_df) %in% "row_names")]
KNNlookup_result_DM<-as.matrix(sapply(KNNlookup_result_df2, as.numeric))

# #switch back to col&row name
# unchanged_exprsDM<-exprs(HIV_fcs)
# colnames(KNNlookup_result_DM)<-colnames(unchanged_exprsDM)
# row.names(KNNlookup_result_DM)<-row.names(unchanged_exprsDM)

#PUT BACK TO exprs
exprs(KNNresult_fcs) <- KNNlookup_result_DM

#switch columns(HIV related parameters)
Nknn<-as.numeric(nrow(HIV_fcs))

if(Switch_KC57_GAG_Parameters){
  dftemp<-pData(parameters(KNNresult_fcs))
  switch_keyname<-vlookup_df(HIVrelated_parameters, dftemp,result_column ="name", lookup_column = "desc")
  switch_keyname<-as.character(switch_keyname[,1])
  
  exprs(KNNresult_fcs)[1:Nknn, switch_keyname] <- exprs(HIV_fcs)[1:Nknn, switch_keyname]
  
  ############loop2###########
  y=1
  while(k>y){
    exprs(KNNresult_fcs)[((Nknn*y)+1):(Nknn*(y+1)), switch_keyname] <- exprs(HIV_fcs)[1:Nknn,switch_keyname]
    
    y=y+1}
  ###########loop2############
}
#edit keywords
keyword(KNNresult_fcs)$"$FIL"<-FINAKNN_FILENAME
#edit cell#
cellnumber<-as.numeric(length(lookupcode_vn))
keyword(KNNresult_fcs)$"$TOT"<-cellnumber
#edit GUID file name
identifier(KNNresult_fcs)<-paste("KNN_",keyword(HIV_fcs)$"$FIL",sep="")
#export file
write.FCS(KNNresult_fcs,FINAKNN_FILENAME, what="numeric")

#generate a report:
INCLUDED_parameters<-colnames(Training_Asinh_remo)
parameters<-list(Junk_parameters,HIVrelated_parameters,Remov_parameters,INCLUDED_parameters)

parameters_name<-c("Junk parameters(removed)","HIV related parameters(removed)","Other removed parameters","INCLUDED_parameters")

text1<-paste("k=",k,".so","KNN repeats:",x,"times")
text2<-paste("Each run:",length(lookupcode))
text3<-paste("Total number of KNN found:",length(lookupcode_vn))
if(Switch_KC57_GAG_Parameters==TRUE){
  text4<-paste0("Your",switch_keyname,HIVrelated_parameters,"has been replaced with original predictory data, please check the name and staining in this report is correct.")
}else {text4<-paste("You have chose not to change your KNN precursor's HIV related staining with predictory set")}

INFO_pagelist<-list(text1,text2,text3,text4)

report_wb<- createWorkbook()
Settings<-"Settings"
IDRESULT<-"knn cell ID result"
addWorksheet(report_wb, Settings)
addWorksheet(report_wb, IDRESULT)
writeData(report_wb, Settings,INFO_pagelist[1], startCol = 1, startRow = 1, xy = NULL)
writeData(report_wb, Settings,INFO_pagelist[2], startCol = 1, startRow = 2, xy = NULL)
writeData(report_wb, Settings,INFO_pagelist[3], startCol = 1, startRow = 3, xy = NULL)
writeData(report_wb, Settings,INFO_pagelist[4], startCol = 1, startRow = 4, xy = NULL)
writeData(report_wb, Settings, parameters_name[1], startCol = 2, startRow = 7, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters_name[2], startCol = 3, startRow = 7, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters_name[3], startCol = 4, startRow = 7, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters_name[4], startCol = 5, startRow = 7, xy = NULL,colNames = TRUE, rowNames = TRUE)

writeData(report_wb,Settings, parameters[1], startCol = 2, startRow = 8, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters[2], startCol =3, startRow = 8, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters[3], startCol = 4, startRow = 8, xy = NULL,colNames = TRUE, rowNames = TRUE)
writeData(report_wb, Settings, parameters[4], startCol = 5, startRow = 8, xy = NULL,colNames = TRUE, rowNames = TRUE)

writeData(report_wb, IDRESULT, reportdf, startCol = 1, startRow = 1, xy = NULL,colNames = TRUE, rowNames = TRUE)

filenamexlsx<-paste(Final_result_filename,"report.xlsx",sep="")
saveWorkbook(report_wb,filenamexlsx, overwrite = TRUE)

# hello from animals
someone_say_hello <- function() {
  remoanimals<-c("anxiouscat","fish", "grumpycat","longcat","longtailcat",
                 "mushroom","shortcat","signbunny","stretchycat")
  used_animals<-names(animals)
  animal_names <-used_animals [! used_animals %in% remoanimals]
  animal <- sample(animal_names, 1)
  class(animals)
  say(paste("Hello, I'm a ", animal, "\nEnjoy you KNN result in folder[",Final_result_filename,"]:\n",WD,"!!!"), by = animal)}
someone_say_hello()
setwd(WD)


