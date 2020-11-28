## Merge Cols in different types and steps
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



## calculate statistical mode
GetMode = function(x){
  u = unique(x)
  u[which((tabulate(match(x,u))) == max(tabulate((match(x,u)))))]
}



## select a random value and use three different ways to estimate that
SelectBestEstimator = function(data){
  output = c()
  #select the first
  rec = min(which(data == sample(data,1)))
  newData  = data[-rec]
  print(paste("Real Record is: ", data[rec]))  
  #mean:
  m1 = mean(newData)
  d1 = abs(m1 - data[rec]) 
  #median:
  m2 = median(newData) 
  d2 = abs(m2 - data[rec])
  #mode:
  m3 = GetMode(newData) 
  d3 = min(unique(abs(m3 - data[rec])))
  
  #choose Minimum distance
  minDis = min(d1, d2, d3)
  
  
  if(minDis == d1){
    output = append(output,paste("Mean: ", m1))
  }
  if(minDis == d2){
    output = append(output, paste("Median: ", m2))
  }
  if(minDis == d3){
    output = append(output,paste("Mode: ", m3))
  }
  print(paste(output,collapse = " & "))
}



