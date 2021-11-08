library(tidyverse)
#eg1:计算iris前四列，每列的均值
df = iris[, 1:4]
map(df, mean)
#返回结果为相同长度数值向量
map_dbl(df, mean)
#可以设置函数的参数，例如上述计算忽略缺失值
map_dbl(df, mean, na.rm = TRUE)
#风格公式map_dbl(df, ~mean(.x, na.rm = TRUE))结果同上

#eg2批量读取数据文件并合并（列名相同）
files = list.files("datas/", pattern = "xlsx", full.names = TRUE)
df = map_dfr(files, read.xlsx, 1)

#批量建模
df1 = mtcars %>% select(mpg,cyl,wt)
df1
df1 = df1 %>%  group_nest(cyl)
df1
df1$data[[1]]
#分组建模
df1 = df1 %>% mutate(model = map(data, ~lm(mpg ~ wt, data = .x)), pred = map(model, predict))
df1
#用列表的元素名做map相当于提取该元素
df1$model %>% map(summary) %>% map_dbl("r.squared")

#模型参数/评估信息
df1$model %>% map(broom::tidy)
df1$model %>% map(broom::glance)
#解除嵌套
df1 %>% unnest(c(data, pred))

#eg4根据身高、体重数据计算BMI指数
height = c(1.58, 1.76, 1.64)
weight = c(52, 73, 68)
cal_BMI = function(h, w) w/h^2
map2_dbl(height, weight, cal_BMI)


#数据在数据框中，也同样可用
df2 = tibble(height = height, weight = weight)
df2 %>% mutate(bmi = map2_dbl(height, weight, cal_BMI))

#分别生成不同数量不同均值、标准差的正态分布随机数
df3 <- tibble(n = c(1,3,5), mean = c(5,10,-3), sd = c(1,5,10))
df3
set.seed(123)
pmap(df3, rnorm)


#walk系列，将mpg按manufacturer分组，每个manufacturer的数据分别保存为单独数据文件
df4 = mpg %>% group_nest(manufacturer)
df4 %>% pwalk(~write.csv(..2, paste0("datas/", ..1, ".csv")))


#eg选择前十个国家的数据，绘制预期寿命随年份变化的图，并分别保存为图形文件
install.packages("repurrrsive")
library("repurrrsive")
df5 = repurrrsive::gap_split[1:10]
class(df5)
df5
walk(df5, 
     ~ ggsave(paste0("datas/", .x$country[1], ".png"),
              ggplot(.x, aes(year, lifeExp)) +       
                geom_line() +
                ggtitle(paste("Life Expectancy of", .x$country[1]))
     )
)


#reduce函数
reduce(1:100,sum)

#批量数据连接
files = list.files("datas/", pattern = "xlsx", full.names = TRUE)
df6 = map(files, readxl::read_xlsx) %>% reduce(full_join, by="姓名")
df6


#accumulate应用场景：生成一系列的回归公式
acc <- str_c("x", 2:5)
accumulate(acc, ~str_c(.x, .y, sep = "+"), .init = "y ~ x1")

#every和some
mpg %>%
  group_by(manufacturer) %>%
  filter(every(cty, ~ .x > 15) & some(cty, ~ .x > 25)) %>% 
  ungroup()
