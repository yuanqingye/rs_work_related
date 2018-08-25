predeploy_para = data.frame(host="192.168.1.212", port= 43666, user="yufabu", 
                            password = "2015SJGTWYUFABUgxm1234560", dbname="gt_statistics",stringsAsFactors = FALSE)
mysqlconn <- function(parameter) {
  library(RMySQL)
  library(plyr)
  library(geosphere)
  con <- dbConnect(MySQL(), host=parameter$host, port= parameter$port, 
                   user=parameter$user, password = parameter$password, dbname=parameter$dbname)
  dbSendQuery(con, 'SET NAMES gbk')
  return(con)
}

con=mysqlconn(predeploy_para)
data = dbGetQuery(con,"select company_id,material_id  from gt.gt_quotation_goods limit 10")


buyer_rec = function(data){
  #data = fetch(res)
  user = unique(data$company_id) 
  material = unique(data$material_id)
  uidx = match(data$company_id, user)
  iidx = match(data$material_id, material) 
  M = matrix(0, length(user), length(material))
  i = cbind(uidx, iidx)
  M[i]=1#
  mod = colSums(M^2)^0.5   
  MM = M %*% diag(1/mod)
  S = crossprod(MM)
  R = M %*% S  #两个矩阵的乘积(内积)
  R = apply(R, 1, FUN=sort, decreasing=TRUE, index.return=TRUE)
  k=5
  res = lapply(R, FUN=function(r)return(material[r$ix[1:k]]))
}

ff <- buyer_rec(data)

