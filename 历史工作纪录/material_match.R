product_para = data.frame(host="192.168.1.211", port= 3310, user="product", 
                          password = "p123789", dbname="testsite",stringsAsFactors = FALSE)
predeploy_para = data.frame(host="192.168.1.212", port= 43666, user="yufabu", 
                            password = "2015SJGTWYUFABUgxm1234560", dbname="gt_statistics",stringsAsFactors = FALSE)
mysqlconn <- function(parameter,code='gbk') {
  library(RMySQL)
  library(plyr)
  library(geosphere)
  con <- dbConnect(MySQL(), host=parameter$host, port= parameter$port, 
                   user=parameter$user, password = parameter$password, dbname=parameter$dbname)
  if(code=='gbk')
  dbSendQuery(con, 'SET NAMES gbk')
  else
  dbSendQuery(con, 'SET NAMES utf8')
  return(con)
}

preparetestdata = function(filename = '~/R/menghua_material.xlsx',colformat=c(2,4,6,8,12),colchangename = c(6,8),sheetname ='Sheet1'){
  library('xlsx')
  sample_list = read.xlsx(filename,sheetname)
  sample_list = formatdataframe(sample_list,colformat)
  colnames(sample_list)[colchangename] = c('name','model')
  return(sample_list)
}


initialization = function(con){
  library(devtools)
  library(RMySQL)
  library(jiebaR)
  library(tidyr)
  # con = mysqlconn(predeploy_para)
  material_name = dbGetQuery(con,"select DISTINCT name from gt.gt_material")
  keys = worker("keywords",topn=10)
  material_key=lapply(material_name$name,keywords,keys)
  # material_name2 <- as.data.frame(material_key)
  material_name$key_word = sapply(material_key,paste,collapse = ',')
  material_row <- separate_rows(material_name,key_word,sep=',')
  material <- data.frame(keyword=unique(material_row$key_word),stringsAsFactors=F)
  # material=data.frame(clear_data(remove_empty_data,material,c('keyword')))
  material=data.frame(keyword=material[-which(material$keyword==''),],stringsAsFactors=F)
  # material_copy=material
  Encoding(material$keyword)='utf8'
  Encoding(material_name$name)='utf8'
  Encoding(material_name$key_word)='utf8'
  new_con = mysqlconn(predeploy_para,'utf8')
  new_material_name = dbGetQuery(new_con,"select DISTINCT name from gt.gt_material")
  combined_material_name = data.frame(new_material_name$name,material_name$key_word)
  dbWriteTable(con,'gt_material_keyword',material,overwrite=T,row.names=F)
  dbWriteTable(con,'gt_material_with_index',combined_material_name,overwrite=T,row.names=F)
  dbDisconnect(con)
  dbDisconnect(new_con)
}

clear_data = function(f,origin_data,col_names){
  result_data = f(origin_data,col_names)
  return(result_data)
}

is.empty.origin = function(key){
  if(is.na(key)){
    return(TRUE)
  }
  else if(gsub("([ ])","",key)==''){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
#去除空行
remove_empty_data = function(origin_data,col_names){
  is.empty = Vectorize(is.empty.origin) #函数向量化
  empty_logic_list = lapply(origin_data[,col_names],is.empty)
  result = T
  for(l in empty_logic_list){
    result = result&l
  }
  result_data = origin_data[!result,]
  return(result_data)
}
#对错误exeption进行处理
test=function(){
  # try(stop(simpleMessage('comeon')))
  n = try(parse(text='a=1'))
  m=tryCatch(parse(test='a=1'))
  print(n)
  print(2)
}

main = function(){
setwd("~/R")
source('~/R/char_similarity_modify.R', encoding = 'UTF-8')
source('~/R/material_match.R', encoding = 'UTF-8')  
preconn = mysqlconn(predeploy_para)
# cond = tryCatch(dbReadTable(con,'gt_material_keyword'),error = function(e){return(T)})
cond1 = !dbExistsTable(preconn,'gt_material_keyword')
cond2 = !dbExistsTable(preconn,'gt_material_with_index')
#tryCatch(expr,error,next,finally)
if(cond1|cond2){
  initialization(preconn)
}
material = dbGetQuery(preconn,'select * from gt_material_keyword')
material_name = dbGetQuery(preconn,'select * from gt_material_with_index')
material_list = preparebackgrounddata(preconn)
sample_list = preparetestdata('~/R/物资匹配实验数据.xlsx', 1:6, 2:3,'Sheet1')
sample_list = clear_data(remove_empty_data,sample_list,c('name','model'))
ms = list()
i=1
cl = getClusters(16) #建立多个线程
for(name in sample_list$name){
# i=3
# name = sample_list$name[i]
gresult = lapply(material$keyword,grep,name) #看关键词是否包含在待匹配词内
gresult2 = lapply(gresult, length) > 0
words = material[gresult2,] #this place need to be checked
index_results = lapply(words,grep,material_name$key_word)
index_result = do.call('c',index_results)
result2 = material_name[index_result,]
material_list_to_match = material_list[material_list$name%in%result2$name,]
# colnames(sample_list)[which(colnames(sample_list)=='material_name')]='name'
m = match_material_with_classify(sample_list[i,],material_list_to_match,16,cl)
ms[[i]]=m
i=i+1
}
final_result2 = do.call('rbind',ms)
stopCluster(cl)
dbDisconnect(preconn)
return(final_result2)
}
final_result2 = main()