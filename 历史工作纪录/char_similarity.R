#找出两个向量最大共有字符串
get_max_commen_char = function(A,B,ifvectorized=FALSE){
  if(!ifvectorized){
    A=strsplit(A,"")[[1]]
    B=strsplit(B,"")[[1]]
  }
  I=length(A)
  J=length(B)
  LEO=matrix(nrow=I+1,ncol=J+1)
  LEO[1,]=0
  LEO[,1]=0
  for(i in 2:(I+1)){
    for(j in 2:(J+1)){
      if(A[i-1]==B[j-1]){
        LEO[i,j]=LEO[i-1,j-1]+1
      }
      else if(LEO[i-1,j]>=LEO[i,j-1]){
        LEO[i,j]=LEO[i-1,j]
      }
      else{
        LEO[i,j]=LEO[i,j-1]
      }
    }
  }
  return(LEO)
}

correlation = function(A,B){
  if(is.na(A)||is.na(B)){
    if(is.na(A)&&is.na(B)){
      return(0.66) #当两者都是NA时候，给予一定相似度
    }
    else{
      return(0)
    }
  }
  if(nchar(A)==0||nchar(B)==0){
    if(nchar(A)==nchar(B)){
      return(0.66)
    }
    else{
      return(0)
    }
  }
  devider = (nchar(A)+nchar(B))/2
  nominator = get_max_commen_char(A,B)[nchar(A)+1,nchar(B)+1]
  return(nominator/devider)
}

 vector_similarity = function(v1,v2){
  s = mapply(FUN="correlation",v1,v2)
  return(s)
 }
 
 whole_similarity = function(v1,v2){
   s1 = paste(v1,collapse = ' ')
   s2 = paste(v2,collapse = ' ')
   return(correlation(s1,s2))
 }
 
 #输入一个模板集合和一个待测试集合
 #输出以待测试集合为基准的集合，包含匹配信息
 
 matchresult = function(standardset,testset,weight,t_colname,a_colname,whole = F){
   rs = data.frame(matrix(numeric(),ncol=ncol(testset)+2))
   for(i in 1:nrow(testset)){
     df = data.frame(matrix(numeric(0),ncol = length(weight)+1))
     for(j in 1:nrow(standardset)){
       if(!whole){
         result = vector_similarity(testset[i,t_colname],standardset[j,a_colname])
         score = sum(result*weight)}
       else{
         result = whole_similarity(testset[i,t_colname],standardset[j,a_colname])
         score = result
         weight = 1
       }
       fresult = c(result,"score"=score)
       df = rbind(df,fresult)
     }
     max = max(df[,length(weight)+1])
     max_index = which.max(df[,length(weight)+1])
     added = data.frame(t(c(testset[i,],max,standardset[max_index,"id"]))) #,standardset[max_index,"最终编码"]
     colnames(added) = colnames(rs)
     rs = rbind(rs,added)
   }
   colnames(rs)[(ncol(rs)-1):ncol(rs)] = c('max_value','match_id')
   return(rs)
 }
 
 
 preparebackgrounddata = function(con,type = 1) {
   if (type==1) {
     query = "select * from gt.gt_material where id in
     (select material_id from gt.gt_material_class_relation);"
   }
   else if(type==2){
     query = 'select * from gt.gt_material where id in (select distinct material_id from gt.gt_goods where company_id is not null)'
   }
   else if(type==3){
     query = 'select g.id,g.name,g.material_id,g.model from gt.gt_goods g,gt.gt_goods_info i where g.id = i.goods_id'
   }
   material_list = dbGetQuery(con,query)
   return(material_list)
   }
 
 main_match_mh = function(testset, mainset, sepnum = 16,t_colname=c('name','model'),a_colname=c('name','model')) {
   if (nrow(mainset) <= 500) {
     final_result = matchresult(mainset,testset,c(0.45,0.55),t_colname,a_colname,T)
   }
   else{
     final_results = list()
     cl = makeCluster(sepnum)
     clusterEvalQ(cl, parse('char_similarity.R'))
     # clusterEvalQ(cl,source('char_similarity.R'))
     clusterExport(
       cl,
       list(
         "whole_similarity",
         "vector_similarity",
         "matchresult",
         "correlation",
         "get_max_commen_char"
       )
     )
     for(i in 1:nrow(testset)){
       print(paste(testset$num,'寮€濮嬪惊鐜?',' ',Sys.time()))
       ll = lapply(strsplit(mainset[,a_colname[1]],split=''),intersect,unlist(strsplit(testset[i,t_colname[1]],split='')))
       filteredset = mainset[lapply(ll,length)>0,]
       # filteredset = mainset
       print(nrow(filteredset))
       mainsets = seperate(filteredset, sepnum)
       print(paste(Sys.time(),' start multi-core calculation'))
       results = parLapply(
         cl,
         mainsets,
         matchresult,
         testset = testset[i,],
         weight = c(0.45,0.55),
         t_colname = t_colname,
         a_colname = a_colname,
         whole = F    
       )
       temp_results = do.call("rbind", results)
       final_results[[i]] = temp_results
       # write.xlsx(temp_results,paste('material',i,'.xlsx',sep=''))
       # print(paste('save file',i,'successful'))
     }
     stopCluster(cl)
     final_result = do.call("rbind",final_results)
     print(paste(Sys.time(),'end multi-core calculation'))
   }
   return(final_result)
 }
 
 seperate = function(dataset,num){
   rownum = nrow(dataset)
   linenum = floor(rownum/num)
   datasets = list()
   tempsample = sample(1:nrow(dataset),nrow(dataset))
   tempset = dataset[tempsample,]
   for(i in 1:(num-1)){
     datasets[[i]] = tempset[((i-1)*linenum+1):(i*linenum),]
   }
   datasets[[num]] = tempset[((num-1)*linenum+1):rownum,]
   return(datasets)
 }
 
 formatdataframe = function(data,colnum){
   for(i in colnum){
     data[,i]=iconv(data[,i],from="utf-8",to="gbk")
     # within(data,{
     #   
     # })
   }
   colnames(data) = iconv(colnames(data),from="utf-8",to="gbk")
   return(data)
 }
 
 match_material = function(test,
                           material_list,
                           sepnum = 16,
                           threshold = 0.5,
                           type,
                           con) {
   library('parallel')
   n = data.frame(
     material_id = numeric(0),
     goods_id = numeric(0),
     seq = numeric(0),
     match_value = numeric(0)
   )
   if (type == 'm') {
     for (i in 1:nrow(test)) {
       query = paste(
         'select id from gt.gt_material where name ="',
         test[i, 'name'],
         '" and model="',
         test[i, 'model'],
         '" limit 1',
         sep = ''
       )
       temp = dbGetQuery(con, query)
       if (nrow(temp) > 0) {
         temp = cbind(
           material_id = temp,
           goods_id = NA,
           seq = row.names(test[i,]),
           match_value = 1
         )
         n = rbind(n, temp)
       }
     }
   }
   else if (type == 'g') {
     for (i in 1:nrow(test)) {
       query = paste(
         'select id from gt.gt_goods where name ="',
         test[i, 'name'],
         '" and model="',
         test[i, 'model'],
         '" limit 1',
         sep = ''
       )
       temp = dbGetQuery(con, query)
       if (nrow(temp) > 0) {
         temp = cbind(
           material_id = temp,
           goods_id = NA,
           seq = row.names(test[i,]),
           match_value = 1
         )
         n = rbind(n, temp)
       }
     }
   }
   test = cbind(test, seq = row.names(test), stringsAsFactors = FALSE)
   temptest = test[!(row.names(test) %in% n$seq), ]
   m = data.frame(
     material_id = NA,
     goods_id = NA,
     match_value = NA,
     seq = NA
   )
   m = m[-1, ]
   if (nrow(temptest) > 0) {
     xx = main_match_mh(temptest, material_list)
     if (nrow(material_list) > 500) {
       if (type == 'm') {
         for (i in 1:nrow(temptest)) {
           m[i, 'material_id'] = xx[(i - 1) * sepnum + which.max(unlist(xx[((i - 1) * sepnum + 1):(i * sepnum), 'max_value'])), 'match_id']
           m[i, 'match_value'] = max(unlist(xx[((i - 1) * sepnum + 1):(i * sepnum), 'max_value']))
           m[i, 'seq'] = temptest[i, 'seq']
         }
       }
       else{
         for (i in 1:nrow(temptest)) {
           m[i, 'goods_id'] = xx[(i - 1) * sepnum + which.max(unlist(xx[((i - 1) * sepnum + 1):(i * sepnum), 'max_value'])), 'match_id']
           m[i, 'match_value'] = max(unlist(xx[((i - 1) * sepnum + 1):(i * sepnum), 'max_value']))
           m[i, 'seq'] = temptest[i, 'seq']
         }
       }
     }
     else{
       if (type == 'm') {
         for (i in 1:nrow(temptest)) {
           m[i, 'material_id'] = xx[i, 'match_id']
           m[i, 'match_value'] = xx[i, 'max_value']
           m[i, 'seq'] = temptest[i, 'seq']
         }
       }
       else{
         for (i in 1:nrow(temptest)) {
           m[i, 'match_value'] = xx[i, 'max_value']
           m[i, 'goods_id'] = xx[i, 'match_id']
           m[i, 'seq'] = temptest[i, 'seq']
         }
       }
     }
   }
   # m = cbind(m,id = row.names(test))
   m = rbind(m, n)
   m = m[m$match_value > threshold, ]
   return(m)
 }