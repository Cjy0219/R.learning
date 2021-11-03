library(dplyr)
library(hflights)
#预览数据集
class(hflights);dim(hflights)
head(hflights)
#在利用dplyr包处理数据之前，需要将数据装载成dplyr包的一个特定对象类型（data frame tbl / tbl_df），
#也称作 tibble 类型，可以用 tbl_df函数将数据框类型的数据装载成 tibble 类型的数据对象
packageVersion("dplyr")
attach(hflights)
tbl_hflights<-tbl_df(hflights)
class(tbl_hflights)
tbl_hflights

#数据记录筛选
# 选择2011年1月而且起飞时间为1400的所有数据记录
filter(tbl_hflights,Year == 2011, Month == 1, DepTime == 1400)
# '且'的关系除了"，"也可以用&符号表示，也就是列出的所有条件同时满足
filter(tbl_hflights,Year == 2011 & Month == 1 & DepTime == 1400)
# 选择起飞时间在1400之前的航班
filter(tbl_hflights,Year == 2011 & Month == 1 & DepTime <= 1400)
# '或'的关系用|符号表示。选择起飞时间为1400或者1430的航班,且UniqueCarrier为'AA'
filter(tbl_hflights,Year == 2011 & Month == 1 & (DepTime == 1400 | DepTime == 1430) & UniqueCarrier == 'AA')



#变量筛选
# 选择指定的变量
select(tbl_hflights,Year,Month,DayofMonth,FlightNum,Distance)
# 用类似于生成数字序列的方式选择变量
# 选择Year开始一直到ArrTime结束的所有变量
select(tbl_hflights,Year:ArrTime)
# 也可以按照倒过来的顺序选择
select(tbl_hflights,ArrTime:Year)
# 除了选择变量，也可以删除指定的变量
select(tbl_hflights,-Year,-Month,-DayofMonth,-FlightNum,-Distance)
select(tbl_hflights,-(Year:ArrTime))



#数据排序
#抽取数据
tbl_hflights1<-select(filter(tbl_hflights,Year == 2011 & Month == 1 & DepTime == 1400),Year:ArrTime,AirTime)
tbl_hflights1
# 将数据按照ArrTime升序排序
arrange(tbl_hflights1,ArrTime)
# 将数据先按照AirTime降序，再按照ArrTime升序排列
arrange(tbl_hflights1,desc(AirTime),ArrTime)


#变量变换/重构
tbl_hflights2<-mutate(tbl_hflights1,
                      DurTime = (as.numeric(substr(ArrTime,1,2)) - as.numeric(substr(DepTime,1,2)))*60 + as.numeric(substr(ArrTime,3,4)) ,
                      Dur_Time1 = DurTime * 60)
tbl_hflights2



#数据汇总
# 计算航班平均飞行时长
summarize(tbl_hflights2,avg_dur = mean(DurTime),sum_air = sum(AirTime))


#数据分组
# 按照航空公司分组进行汇总
summarise(group_by(tbl_hflights, UniqueCarrier), 
          m = mean(AirTime,na.rm = TRUE), 
          sd = sd(AirTime,na.rm = TRUE), 
          cnt = n(), 
          me = median(AirTime,na.rm = TRUE))


#多步操作连接符/管道函数
# 对数据进行分布处理：分组-汇总-排序-打印
tbl_hflights %>%
  group_by(UniqueCarrier) %>%
  summarize(m = mean(AirTime,na.rm = TRUE), sd = sd(AirTime,na.rm = TRUE)) %>%
  arrange(desc(m),sd) %>%
  head(10)


#挑选随机样本
# 随机抽取10个样本
sample_n(tbl_hflights,10)
# 随机抽取10%的样本
tbl_hflights %>% 
  sample_frac(0.1) %>%
  select(Year:UniqueCarrier) %>%
  group_by(UniqueCarrier) %>%
  summarize(m = mean(ArrTime,na.rm = TRUE), cnt = n()) %>%
  arrange(desc(m))



#查看包中自带参考资料
# 查看自带的参考资料
vignette(package = "dplyr")
vignette("introduction", package = "dplyr")


#连接操作
df <- data.frame(x = c('a','b','c','a','b','e','d','f'), y = c(1,2,3,4,5,6,7,8))
df
df2 <- data.frame(x = c('a','b','c'), z = c('A','B','C'))
df2
#转换为tbl对象
df2tbl <- tbl_df(df)
df2tbl
df2tbl2 <- tbl_df(df2)
df2tbl2

inner_join(x = df2tbl, y = df2tbl2, by = 'x')
semi_join(x = df2tbl, y = df2tbl2, by = 'x')
anti_join(x = df2tbl, y = df2tbl2, by = 'x')


#数据合并
mydf1 <- data.frame(x = c(1,2,3,4), y = c(10,20,30,40))
mydf1
mydf2 <- data.frame(x = c(5,6), y = c(50,60))
mydf2
mydf3 <- data.frame(z = c(100,200,300,400))
mydf3
#bind_rows()函数需要两个数据框或tbl对象有相同的列数
bind_rows(mydf1, mydf2)
#bind_cols()函数则需要两个数据框或tbl对象有相同的行数
bind_cols(mydf1, mydf3)



#集合操作
#取两个集合的交集
intersect(x,y, ...)
#取两个集合的并集，并进行去重
union(x,y, ...)
#取两个集合的并集，不去重
union_all(x,y, ...)
#取两个集合的差集，在x中不在y中
setdiff(x,y, ...)
#判断两个集合是否相等
setequal(x, y, ...)



#连接数据库数据
src_mysql(dbname, host = NULL, port = 0L, user = "root", password = "",...)
tbl(src,from = '')


