library('rvest')
library('xml2')
library('sqldf')
#library('psych')
library('XML')

options(sqldf.driver="SQLite")

crawler_hx <- function(url)
{
web    <- read_html(url)
rootnode  <- web%>%html_nodes ("li.tpl-huxingtu.cell.fl")
rootsize <- xmlSize(rootnode)
#print(rootsize)
chushi <- data.frame()
for (i in 1:rootsize)
{
  t     <- rootnode[i]
  title <- t %>%html_nodes("div.info>h4>a") %>% html_attr("title")
  des   <- t %>%html_nodes("div.info>p>span")
  if (length(des)==2){des1  <- des[1]%>%html_text()
                      des2  <- des[2]%>%html_text()
                      des2  <- sub("建筑面积 ","",des2)
                      #des2  <- substring(des2, 1,length(des2)-2)
                      }
  else{des1 <-''
       des2 <- des%>%html_text()
       des2  <- sub("建筑面积 ","",des2)
       #des2  <- substring(des2, 1,length(des2)-2)
       }
  href  <- t%>%html_nodes("div.info>h4>a")%>% html_attr("href")
  href  <- paste("https://www.kujiale.com",href,sep="")
  pic_link  <- t%>% html_attr("data-pic")
  chushi<-rbind(chushi,data.frame(title,des1,des2,href,pic_link))
}
return(data.frame(chushi))
}


myfunction <- function(city,add,shi,ting,wei)
{
  zuhe      <- paste(shi,'室',ting,'厅',wei,'卫',sep="")
  sql       <- paste("select * from final  where huxing like ","'",zuhe,"%'",sep="")
  city_list <- read.table('C:/Users/01444878/Desktop/脚本/kjl_r/city.csv',sep=',',header=TRUE)
  for (i in 1:length(city_list[,1]))
  { if (city==city_list[i,3]) 
  {
    gurl <- paste("https://www.kujiale.com/huxing/",city_list[i,4],"-",add,sep="")
    break
  }
  }
  url    <- gurl
  final  <- data.frame()
  web    <- read_html(url)
  num1   <- web%>%html_nodes ("div.page-r>a")%>% html_text()
  data   <- strsplit(num1,'\"')
  h      <- length(data)
  if (h > 0){
  num    <- data[h-1]
  i      <- as.numeric(num)
  for (t in 1:i)
  {gurl  <- paste(url,'/',t,sep="")
   chushi <- crawler_hx(gurl)
   final <- rbind(final,chushi)
  }
  names(final)[1:5] <- c('name','huxing','mianji','link','pic_link')
  final <- sqldf(sql)
  return(final)}
  else{
    final <- crawler_hx(gurl)
    names(final)[1:5] <- c('name','huxing','mianji','link','pic_link')
    final <- sqldf(sql)
    return(final)
    }
}

myfunction('北京市','朗琴园','2','0','1')

