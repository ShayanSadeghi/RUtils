ColMerger = function(data,type,step){
  dscData=c()
  
  i = 1
  while(i <= length(data)){
    dat = data[(i):(i+step-1)]
    if(i == length(data)){
      dat=data[(i)]
    }
    
    if(type == "average"){
      dscData = append(dscData,mean(dat))
    }else if(type == "equal"){
      dscData = append(dscData,unique(dat))
    }else if(type == "sum"){
      dscData = append(dscData,sum(dat))
    }else if(type == "period"){
      dat = paste(c(min(dat), max(dat)),collapse =" , ")
      dscData = append(dscData,dat)
    }
    i = i + step
  }
  return(dscData)
}