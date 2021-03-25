# Behavioural Data Analsysis Functions for ICx3

library(dplyr)
library(reshape2)
#DONT USE reshape: cast() has issues with missing values... dont know why

se <- function(x) sqrt(var(x)/length(x))


# Map instance types to their corresponding names
typeNameF= function(types){
  typeNames=rep("",length(types))
  i=1
  for (type in types){
    if(type==1){
      typeName= 'In PT - No - Easy'
    }else if(type==2){
      typeName= 'In PT - Yes - Easy'
    }else if(type==3){
      typeName= 'In PT - No - Hard'
    }else if(type==4){
      typeName= 'In PT - Yes - Hard'
    }else if(type==5){
      typeName= 'Out PT - No'
    }else if(type==6){
      typeName= 'Out PT - Yes'
    }else{
      typeName= 'None Defined'
    }
    typeNames[i]=typeName
    i=i+1
  }
  return(typeNames)
}




#' Create the formatted regression table for statistical model export
#' 
#' @param model model to be exported
#' @param title Caption shown aththe top of the table
#' @param outputType "html" or "latex"
#' @param outputName The name of the output file
#' @param out_folder Output Folder Path
#'
#' @return The text of the table
#' @export table Exports tables if outputType==Latex
modelTableStyle = function(model,title,outputType,outputName,out_folder){
  print(outputName)
  # Stores the model in the list with all the models
  allModels[[outputName]]<<-model
  if (outputType=="html"){
    table = stargazer(model, type=outputType, report=('vc*sp'),#(('vc*p'))
                      column.labels=c("Random Intercept"), dep.var.caption = title,
                      dep.var.labels.include = TRUE,model.names=FALSE,
                      align=TRUE, column.sep.width = "5000pt")
  } else if (outputType=="latex"){
    table = stargazer(model, type=outputType, report=('vc*sp'),
                      column.labels=c("Random Intercept"), dep.var.caption = title, 
                      dep.var.labels.include = TRUE,model.names=FALSE, align=TRUE, 
                      column.sep.width = "0pt", 
                      out = paste0(out_folder,"/",outputName,".tex"),table.placement="H")  
  }
  #return(table)
}

#' Exports the formatted plot as pdf for report
#' @param plot The ggplot to be exported
#' @param outputName The name of the output file
#' @param out_folder Output Folder Path
#'
#' @return 
#' @export plot formatted plot as pdf 
plotExport = function(plot,outputName,out_folder){
  print(outputName)
  print(plot)
  #folder="~/Google Drive/Melbourne/UNIMELB/Research/Complexity Project/KS-IC/Code/Behavioural-Analysis"
  pdf(file=paste0(out_folder,"/",outputName,".pdf"))
  print(plot)
  dev.off()
}

#' Export non-regression-based table to html or latex using Pander
#'
#' @param table The table to be formated
#' @param title Caption shown ath the top of the table
#' @param outputType html" or "latex"
#' @param outputName The name of the output file
#' @param out_folder Output Folder Path
#'
#' @return The formatted text of the table
#' @export table Exports tables if outputType==Latex
exportTable = function(table,title,outputType,outputName,out_folder){
  print(outputName)
  if (outputType=="html"){
    table = stargazer(table, type=outputType, dep.var.caption = title, align=TRUE,
                      column.sep.width = "5000pt",summary=FALSE,rownames = FALSE)  
  } else if (outputType=="latex"){
    table = stargazer(table, type=outputType, dep.var.caption = title, align=TRUE,
                      column.sep.width = "0pt",
                      out = paste0(out_folder,"/",outputName,".tex"), 
                      table.placement="H",
                      summary=FALSE,rownames = FALSE)  
  }
}



# Import and merge DATA accross participants Decision + Optimisation (Trial Info + Instance Info)
##Input:
# folderData=folderDataDec
# pFix=c("p1","p2","op1","op2")
# #"dec" or "opt"
# task="dec"
##Output
# dataAll := data with trialInfo and instanceInfo for all aprticipants in the folder
importTrialInfo = function(folderData){
  #Obtain all participant ID's in the folder 
  trialInfoFiles = list.files(folderData,pattern="*TrialInfo.txt$")
  pIDs=sapply(strsplit(trialInfoFiles,"_"),function(x) x[1])
  
  dataAll=data_frame()
  for (pID in pIDs){
    #Import Trialinfo and Instance Info
    trialInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*TrialInfo.txt$"))
    instanceInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*InstancesInfo.txt$"))
    #print(pID)
    trialInfo=read.csv(paste0(folderData,trialInfoFile),skip=1, sep=";", stringsAsFactors = FALSE)
    instanceInfo=read.csv(paste0(folderData,instanceInfoFile),skip=1, sep=";", header=TRUE, stringsAsFactors = FALSE)
    
    #Merge Both instanceInfo and trialInfo
    trialInfoM=merge(trialInfo,instanceInfo, by="instanceNumber")
    
    #Add Column with participant ID
    trialInfoM=cbind(pID=pID,trialInfoM, stringsAsFactors =FALSE)
    
    #Bind new participant to dataAll
    dataAll=rbind(dataAll,trialInfoM)
  }
  return(dataAll)
}


# Main data Description
# #Input
# dataAll := Data for all participants with trialInfo and instanceInfo (output of importTrialInfo)
# #Output
# A list with table with data summarized in different ways
# summaryData= function(dataAll){
#   #basic Info on correct answers (Opt and Dec)
#   mean(dataAll$correct)
#   sum(dataAll$correct)
#   #Correct answers by Instance Type
#   #tapply(dataAll$correct, dataAll$type, mean)
#   
#   #Correct answers by Instance Type and Participant
#   groups1=dataAll %>% group_by(pID,type)
#   g1=summarise(groups1, correctP=mean(correct))
#   g1=cast(g1,formula=pID~type,value="correctP")
#   
#   #Correct answers by Participant
#   groups2=dataAll %>% group_by(pID)
#   g2=summarise(groups2, mean(correct),payoff1AUD=sum(correct))
#   
#   #Correct answers by Instance Type
#   groups3=dataAll %>% group_by(type)
#   g3=summarise(groups3, accuracy=mean(correct), se=se(correct))
#   
#   #Correct answers by Instance
#   #groupsI=dataAll %>% group_by(instanceNumber)
#   groupsI=dataAll %>% group_by(id)
#   gI=summarise(groupsI, correctP=mean(correct))
#   
#   #For Optimization:
#   #Mean Time Spent per instance
#   mean(dataAll$timeSpent)
#   t1=summarise(groups1, timeSpent=mean(timeSpent))
#   t1=cast(t1,formula=pID~type,value="timeSpent")
#   
#   #Output Info Storage
#   summary=list(g1,g2,g3,gI,t1)
#   return(summary)
# }


#' For KS Analysis: Add a colum to the KS data representing the sum of weights
#'
#' @param dataInput Knapsack Data, either decision or optimisation with a 'w' column
#'
#' @return dataInout with one additional column representing the sum of weights (totalWeights)
#' @export
#'
#' @examples
addSumOfWeights = function(dataInput){
  totalWeights=rep(0,length(dataInput$w))
  i=1
  for (weigths in dataInput$w) {
    totalWeights[i]=sum(as.integer(strsplit(weigths,',')[[1]]))
    i=i+1
  }
  dataInput$totalWeights=totalWeights
  return(dataInput)
}

#' For KS Analysis: Add a colum to the KS data representing the sum of weights
#'
#' @param dataInput Knapsack Data, either decision or optimisation with a 'w' column
#'
#' @return dataInout with one additional column representing the sum of weights (totalWeights)
#' @export
#'
#' @examples
addSumOfValues = function(dataInput){
  totalValues=rep(0,length(dataInput$v))
  i=1
  for (values in dataInput$v) {
    totalValues[i]=sum(as.integer(strsplit(values,',')[[1]]))
    i=i+1
  }
  dataInput$totalValues=totalValues
  return(dataInput)
}



#' Generates a list with all possible subsets of a vector of size n
#'
#' @param n
#'
#' @return A list of vectors of possible subsets of a vector of size n. The output vectors are boolean vecotrs of size n (e.g. [False,True,False] for n=3)
#'
#' @examples
powerSet <- function(n) { 
  #n <- length(set)
  masks <- 2^(0:(n-1))
  #lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
  listOfSets=lapply( (1:2^n)-1, function(u) { bitwAnd(u, masks) != 0 } )
  return(listOfSets)
}


#' Knapssack Decision: Adds a column to dataInput (Decision knapsack) with the number of solutions to each instance
#'
#' @param dataInput (Decision instances)
#'
#' @return dataInput with a new column (nSolutions) representting the number of item combinations that satisfy the constraints
#' @export
#'
#' @examples
nSolutions_ks <- function(dataInput){
  #Copy Data Input for later use
  dataInput2=dataInput
  #Extracts a Unique list of instance with weights, value, capacity and profit
  dataInput=dataInput[c('w','v','c','p','id')]
  dataInput=unique(dataInput)
  
  #Generate all the possible subsets of a set of size 6
  p_set=powerSet(6)
  
  #Generate number of solutions for each instance
  dataInput$nSolutions=-1
  nInstances = dim(dataInput)[1]
  for(i in c(1:nInstances)){
    weights=as.integer(strsplit(dataInput$w[i],',')[[1]])
    values=as.integer(strsplit(dataInput$v[i],',')[[1]])
    wCap=dataInput$c[i]
    vProf=dataInput$p[i]
    
    nSolutions=0
    for (subset in p_set){
      if(sum(weights[subset])<=wCap & sum(values[subset])>=vProf){
        nSolutions=nSolutions+1
      }
    }
    dataInput$nSolutions[i]=nSolutions
  }
  
  #Merges the unique-instance-only information dataframe (dataInput) with the Big decision data Frame with all the information
  dataInput2=merge(dataInput2,dataInput,by=c("id","v","w","p","c"))
  
  return(dataInput2)
}

#' 3SAT Transforms the game description of a 3SAT instance into a string respresenting a boolean formula
#'
#' @param vars_values_game The instance "literals" as used in the unity game (string)
#' @param lit_values_game The instance "variables" as used in the unity game (string)
#' @param VariableNames the names you want to use for the variables in the output formula e.g. c('A','B',...)
#'
#' @return boolean_s : string respresenting a boolean formula (R-format)
#'
#' @examples
instanceBooleanFormula = function(vars_values_game,lit_values_game,varsNames){
  varVal= as.integer(strsplit(vars_values_game,',')[[1]])
  litVal= as.integer(strsplit(lit_values_game,',')[[1]])
  
  nlits=length(litVal)
  
  boolean_s="("
  for(i in 1:nlits){
    if(litVal[i]==0){
      boolean_s=paste0(boolean_s,"!")
    }
    boolean_s=paste0(boolean_s,varsNames[varVal[i]])
    if( i %% 3 ==0 ){
      if(i < nlits){
        boolean_s=paste0(boolean_s,") & (")
      } else {
        boolean_s=paste0(boolean_s,")")
      }
    } else {
      boolean_s=paste0(boolean_s,"|")
    }
    
  }
  return(boolean_s)
}


#' 3SAT: Adds a column to dataInput (3SAT) with the number of solutions to each instance
#'
#' @param dataInput (3SAT)
#'
#' @return dataInput with a new column (nSolutions) representting the number of item combinations that satisfy the constraints
#' @export
#'
#' @examples
nSolutions_sat <- function(dataInput){
  
  dataOutput=dataInput
  dataOutput$nSolutions=-1
  tf=c(TRUE,FALSE)
  
  
  for (s in 1:dim(dataInput)[1]){
    #Getting all possible combinations of the variables 
    vars=1:dataInput$nVariables[s]
    varsNames = LETTERS[vars]
    variablesGrid_commandvars = paste0("'",varsNames,"'=tf",collapse=",")
    variablesGrid_command = paste0("expand.grid(",variablesGrid_commandvars ,")")              
    combinations = eval(parse(text=variablesGrid_command))
    
    varVal_s = dataInput$v[s]
    litVal_s = dataInput$l[s]
    
    #Transform the Input to a boolean R-readable function
    boolean_s = instanceBooleanFormula(varVal_s, litVal_s,varsNames)
    
    #Testing all possible combinations of the variables on the boolean formula
    command = paste0("combinations$formula= with(combinations,", boolean_s, ")" )
    eval(parse(text = command))
    
    nSolutions= sum(combinations$formula)
    dataOutput$nSolutions[s]=nSolutions
  }
  
  return(dataOutput)
  
}

#nSolutions TSP




#' Analysis of a variable (that has one value per participant) and mean KS performance per participant
#'
#' @param summaryVariable is a DataFrame with 2 columns: 1 is the pID, 2 is the performance of the variable of interest 
#' @param KSData: Knapsack data, either optimisation or decision data. Mean Performance is calculated using function summaryData()
#'
#' @return Prints and returns the correlation and the plot.
#' @export
#'
#' @examples
indDiffAnalysis= function(summaryVariable,dataDec){
  summaryListTemp=summaryData(dataDec)
  names(summaryListTemp[[2]])['mean(correct)'==names(summaryListTemp[[2]])]='AccuracyKnapsack'
  varMerge = merge(summaryListTemp[[2]], summaryVariable, by="pID")
  varName=names(varMerge)[4]
  
  print(varName)
  
  cortest = cor.test(varMerge$AccuracyKnapsack,varMerge[[varName]],method="pearson")#spearman is non-parametric
  print(cortest)
  
  ggp= ggplot(varMerge,aes_string(x="AccuracyKnapsack",y=varName))+
    geom_point() +
    geom_smooth(method="lm")+
    labs(x="Knapsack Accuracy")
  
  print(ggp)
  
  return(list("plot"=ggp,"corrTest"=cortest))
  
}


#' Recode the order of the tasks presented so every column specifies if the corresponding task
#' was 1st,2nd, or 3rd
#'
#' @param ordertasks 
#'
#' @return ordertasks with 3 joined columns with the corresponding recoding
#' @export
#'
#' @examples
recodeOrder = function(ordertasks){
  orderSAT=vector(length = dim(ordertasks)[1])
  orderTSP=vector(length = dim(ordertasks)[1])
  orderKS=vector(length = dim(ordertasks)[1])
  
  
  for(p in 1:length(ordertasks$pID)){
    print(p)
    orderSAT[p]=which(ordertasks[p,2:4]=="SAT")
    orderKS[p]=which(ordertasks[p,2:4]=="KS")
    orderTSP[p]=which(ordertasks[p,2:4]=="TSP")
  }
  output = cbind(ordertasks,orderSAT,orderKS,orderTSP)
  return(output)
  
}



#' Import clicks data into one single data frame
#'
#' @param folderData Folder where the data is located
#'
#' @return data frame with the clicks for all participants
#' @export
#'
#' @examples
importTrialInfo_clicks = function(folderData){
  #Obtain all participant ID's in the folder 
  trialInfoFiles = list.files(folderData,pattern="*Clicks.txt$")
  pIDs=sapply(strsplit(trialInfoFiles,"_"),function(x) x[1])
  
  dataAll=data_frame()
  for (pID in pIDs){
    #Import Trialinfo and Instance Info
    trialInfoFile = list.files(folderData,pattern=paste0("^",paste0(pID,"_"),".*Clicks.txt$"))
    #print(pID)
    trialInfo=read.csv(paste0(folderData,trialInfoFile),skip=2, sep=";", stringsAsFactors = FALSE)
    
    
    #Add Column with participant ID
    trialInfoM=cbind(pID=pID,trialInfo, stringsAsFactors = FALSE)
    
    #Bind new participant to dataAll
    dataAll=rbind(dataAll,trialInfoM)
  }
  return(dataAll)
}


#' 3SAT: Extract final answer. What were the final variables clicked before submited or time was up
#'
#' @param dataclicks imported clicks data (doesn't need to be mereged with dataTrial_SAT)
#' @param nVars Number of variables Used for all the 3sat instances
#'
#' @return a data frame with pID,block, trial and one column for each variable
#' (-1 means the negative variable was selected, 1 that the positive var was selected and 0 that the variable wasn't selected)
#'
#' @examples
extract_final_answer = function(dataclicks,nVars){
  dataInput = dataclicks 
  dataInput$Literal[dataInput$Literal==0] = -1
  
  dataInput2 = dataInput %>% group_by(pID,block,trial,Variable) %>% 
    summarise(on_off=(n()%%2),literal = last(Literal)) %>% 
    mutate(var_status=on_off*literal)
  
  if(length(unique(dataInput2$Variable))!=nVars){
    stop("Not all variables were clicked. Output only shows
            the ones that were clicked at least once by someone.")
  }
  
  dataInput3 = dcast(dataInput2,pID+block+trial~Variable, value.var ="var_status")
  dataInput3[base::is.na(dataInput3)]=0
  
  return(dataInput3)
}



#' Percentage of lightbulbs (clauses) that were on (TRUE) for one instance.
#'
#' @param vars_s An array defining the variables in the instance (in string format) e.g: vars="[1,2,5,2,3,4,5,...]"
#' @param lits_s An array defining the cprresponding literals in the instance (in string format) e.g: lits="[1,0,1,0,0...]"
#' @param varstatus The players final answer in vector form in order of variable name e.g varstatus=[0,1,-1,1-1]
#' for varstats:(-1 means the negative variable was selected, 1 that the positive var was selected and 0 that the variable wasn't selected)
#'
#' @return Percentage of lightbulbs (clauses) that were on (TRUE) for one instance.
#' @export
#'
#' @examples
correct_clicks = function(vars_s,lits_s,varstatus){
  
  vars=as.numeric(unlist(strsplit(vars_s,",")))
  lits=as.numeric(unlist(strsplit(lits_s,",")))

  #renaming lits to +1,-1
  lits[lits==0]=-1
  
  status = varstatus[vars]
  
  status_true = status*lits
  
  status_m <- matrix(status_true, ncol = 3, byrow = TRUE)
  
  clauses_status=apply(status_m, 1, max)
  nclauses_true = length(clauses_status[clauses_status==1])
  nclauses_total = length(clauses_status)
  
  percentage_clauses_true = nclauses_true/nclauses_total
  
  return(percentage_clauses_true)
}


#' Percentage of lightbulbs (clauses) that were on (TRUE) for all the dataset
#'
#' @param dataTrial_SAT 
#' @param data_finalclicks Data as calculated by extract_final_answer()
#' @param nVars Number of variables Used for all the 3sat instances
#'
#' @return A data frame with all the clicks data merged with dataTrial_SAT and with a new column
#' with the percentage of lightbulbs (clauses) that were on (TRUE) for each instance.
#' @export
#'
#' @examples
correct_clicks_df = function(dataTrial_SAT,data_finalclicks,nVars){

  data_finalclicks_join=merge(dataTrial_SAT, data_finalclicks,
                              by=c("pID","block","trial"))
  data_finalclicks_join$p_clauses_true=-1
  
  for(n in 1:dim(data_finalclicks_join)[1]){
    vars = data_finalclicks_join$v[n]
    lits = data_finalclicks_join$l[n]
    var_status=data_finalclicks_join[n,as.character(c(1:nVars))]
    var_status = as.numeric(var_status)
    
    data_finalclicks_join$p_clauses_true[n]=correct_clicks(vars,lits,var_status)
  }
  return(data_finalclicks_join)
}


#' Create a boxplot of a value for each category by grouping beforehand by a group
#' Eg. Boxplot accuracy by instance complexity where each dot analysed is the average accuracy per participant.
#'
#' @param data 
#' @param group Name of the variable to group for mean calcualtion
#' @param category Each of the categories to be plotted
#' @param value Variable of interest
#' @param title Title for the plot
#' @param xlab x label
#' @param ylab y-label
#'
#' @return A ggplot object
#'
#' @examples ggplot_by_group_box(mtcars,cyl,gear,mpg)
#' ggplot_by_group_box(mtcars,cyl,gear,mpg)
#' ggplot_by_group_box(dataTrial_SAT,pID,region,correct)
ggplot_by_group_box = function(data,group,category,value,
                               title=NA,xlab=NA,ylab=NA){
  require(rlang)
  require(dplyr)
  require(ggplot2)
  category_q = rlang::enquo(category)
  group_q = rlang::enquo(group)
  value_q = rlang::enquo(value)
  
  if(is.na(xlab)){
    xlab=rlang::ensym(category_q)
  }
  if(is.na(ylab)){
    ylab=rlang::ensym(value_q)
  } 
  if(is.na(title)){
    title = paste(rlang::ensym(value_q),
                  "by",
                  rlang::ensym(category_q),
                  "grouped by",
                  rlang::ensym(group_q))
  }
  data2 = data %>% group_by(!!group_q,!!category_q) %>% summarise(means = mean(!!value_q))
  plo = ggplot(data2, aes(x=as.factor(!!category_q),y=means,fill=as.factor(!!category_q)))+
    geom_boxplot()+
    geom_jitter()+
    stat_summary(fun.y = mean, geom = "point", shape = 5, size = 3)+
    labs(title=title,x=xlab,y=ylab)+
    theme_light()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  plo
  return(plo)
}




