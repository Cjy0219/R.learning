#gather
library(tidyr)
#将mtcars的所有列聚合成两列
gather(mtcars, attr, value)
#聚合mpg和gear之间的列
gather(mtcars, attr, value, mpg:gear)
#仅聚合mpg和wt变量，其余变量保持不变
gather(mtcars, attr, value, mpg, wt)
#添加car列到mtcars中
mtcars$car <- rownames(mtcars)
#除了car列，将mtcars的所有列聚合成两列
gather(mtcars, attr, value, -car)
mtcars %>% gather(attr, value, -car)
#聚合mpg和gear之前的列
mtcars %>% gather(attr, value, mpg:gear)
mtcars %>% gather(mpg:gear, key = "attr", value = "value")
#仅聚合gear和carb变量，其余变量保持不变
mtcars %>% gather(attr ,value ,gear ,carb)
mtcars %>% gather(gear ,carb ,key = "attr", value = "value")

#spread
mtcars$car <- rownames(mtcars)
longformat <- gather(mtcars, attr, value, -car)
#还原长格式为原宽格式数据
spread(longformat, attr, value)
longformat %>% spread(attr, value)
#设置sep，新的列名为<key_name><sep><key_value>的形式
spread(longformat, attr, value, sep = '|')
#还原长格式为原宽格式数据，car列的值转换为每一个列
spread(longformat, car, value)
longformat %>% spread(car, value)
df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
#转换为宽格式再转换为长格式，实际还原为原df，只是变量顺序不同
df %>% spread(x, y) %>% gather(x, y, a:b, na.rm = TRUE)
df <- data.frame(row = rep(c(1, 51), each = 3),
                 var = c("Sepal.Length", "Species", "Species_num"),
                 value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))
#对于混合类型的value列，默认convert = FALSE，转换的新列为factor类型
df %>% spread(var, value)
#设置convert = TRUE，保留原类型
df %>% spread(var, value, convert = TRUE)

#unite
library(dplyr)
#使用默认的连接符“_”合并vs和am列为新列vs_am，并删除vs和am列
unite(mtcars, vs_am, vs, am) 
mtcars %>% unite(vs_am, vs, am)
#使用默认的连接符“_”合并vs和am列为新列vs_am，保留vs和am列
unite(mtcars, vs_am, vs, am, remove = FALSE)
#使用连接符“|”合并vs和am列为新列vs_am，并删除vs和am列
unite(mtcars, vs_am, vs, am, sep = '|')

#separate
library(dplyr)
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
#分割为两列，保留NA值
df %>% separate(x, c("A", "B"))
df <- data.frame(x = c("a", "a b", "a b c", NA))
#分割为两列，发出warning并删除多余的列，缺失的列从右以NA填充
df %>% separate(x, c("a", "b"))
#分割为两列，直接删除多余的列，缺失的列从右以NA填充
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
#分割两次(设置的列为两列)，缺失的列从左以NA填充
df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")
df <- data.frame(date = c("2017-03-08 01:20:20", "2017-03-09 02:30:30", "2017-03-10 03:40:40"))
#分割为year,month,day,hour,minute,second六列
df %>% 
  separate(date, c("day", "time"), sep = " ") %>%
  separate(day, c("year", "month", "day"), sep = "-") %>% 
  separate(time, c("hour", "minute", "second"), sep = ":") 


#extract
library(dplyr)
df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6"),
  stringsAsFactors = FALSE
)
#分割y和z列，转换为行
separate_rows(df, y, z, convert = TRUE)


#嵌套和嵌套还原
# 将除Species的列进行nest操作，生成的新列的元素都为list对象
# 其中nest()合并为一列，chop()分别对各列进行合并
iris %>% nest(data = -Species)
iris %>% chop(cols = -Species)

nest_vars <- names(iris)[1:4]
iris %>% nest(data = one_of(nest_vars))
iris %>% chop(cols = one_of(nest_vars))

# 分别组合多列进行合并，chop()不支持此操作
iris %>% nest(petal = starts_with("Petal"), sepal = starts_with("Sepal"))

# 先进行分组操作，分组变量之外的变量会进行nest合并
iris %>% group_by(Species) %>% nest()

# 对不同分组建立线性模型
mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(models = lapply(data, function(df) lm(mpg ~ wt, data = df)))



df <- tibble(
  x = 1:4,
  y = list(NULL, 1:2, 3, NULL), 
  z = list('d', c('a', "b"), "c", NULL)
)
# 展开y和z列，并且删除值都为NULL的行，不全为NULL的行保留
df %>% unnest(c(y, z))
df %>% unchop(c(y, z))
# 展开y和z列，设置keep_empty = TRUE保留所有行
df %>% unnest(c(y, z), keep_empty = TRUE)
df %>% unchop(c(y, z), keep_empty = TRUE)
# 展开y和z列，设置展开后列的类型
df %>% unchop(c(y, z), ptype = tibble(y = character(), z = character()))
# 先使用y列展开，在使用z展开，和直接使用y,z展开不同，y和z会进行笛卡尔积
df %>% unchop(y) %>% unchop(z)


#缺失值处理
#使用给定值替换每列的缺失值
library(dplyr)
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
#以0替换x中的NA，以unknown替换y中的NA
df %>% replace_na(list(x = 0, y = "unknown"))

#以前一个值填充缺失值，默认自上向下填充
df <- data.frame(x = 1:5, y = c(10, NA, 15, NA, 20))
#自上向下替换NA值
df %>% fill(y)
df %>% fill(y, .direction = "down")
#自下向上替换NA值
df %>% fill(y, .direction = "up")

#填充以创建完整的序列值向量
#返回序列1：6
full_seq(c(1, 2, 4, 6), 1)
#period值与原数据间隔不匹配，报错Error: `x` is not a regular sequence.
full_seq(c(1, 2, 4, 6), 2)
#返回序列1：13，间隔为2
full_seq(c(1, 5, 9, 13), 2)


#删除包含缺失值的行
df <- data_frame(x = c(1, 2, NA), y = c("a", NA, "b"))
#删除变量x中NA对应的行
df %>% drop_na(x)
#删除变量y中NA对应的行
df %>% drop_na(y)
#未设置列，删除变量x和y中NA对应的行
df %>% drop_na()


#转换隐式的缺失值为显式的
df <- data_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
#以item_id和item_name中的每个元素扩展原数据框, 组合后的缺失值以NA代替
df %>% complete(item_id, item_name)
df %>% complete(crossing(item_id, item_name))
#以item_id和item_name中的每个元素扩展原数据框, 并以给定值替换缺失值
df %>% complete(item_id, item_name, fill = list(group = 0, value1 = 0, value2 = 0))
#以item_id和item_name中的每个元素扩展原数据框，只返回原数据框中存在的组合
df %>% complete(nesting(item_id, item_name))

#保留group，以item_id和item_name中的每个元素扩展原数据框，只返回原数据框
#中item_id和item_name存在的组合
df %>% complete(group, nesting(item_id, item_name))
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0, value2 = 0))

#保留group，以item_id和item_name中的每个元素扩展原数据框，返回所有item_id
#和item_name存在的组合
df %>% complete(group, crossing(item_id, item_name))
df %>% complete(group, crossing(item_id, item_name), fill = list(value1 = 0, value2 = 0))
