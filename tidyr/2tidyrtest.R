#gather
library(tidyr)
#��mtcars�������оۺϳ�����
gather(mtcars, attr, value)
#�ۺ�mpg��gear֮�����
gather(mtcars, attr, value, mpg:gear)
#���ۺ�mpg��wt����������������ֲ���
gather(mtcars, attr, value, mpg, wt)
#����car�е�mtcars��
mtcars$car <- rownames(mtcars)
#����car�У���mtcars�������оۺϳ�����
gather(mtcars, attr, value, -car)
mtcars %>% gather(attr, value, -car)
#�ۺ�mpg��gear֮ǰ����
mtcars %>% gather(attr, value, mpg:gear)
mtcars %>% gather(mpg:gear, key = "attr", value = "value")
#���ۺ�gear��carb����������������ֲ���
mtcars %>% gather(attr ,value ,gear ,carb)
mtcars %>% gather(gear ,carb ,key = "attr", value = "value")

#spread
mtcars$car <- rownames(mtcars)
longformat <- gather(mtcars, attr, value, -car)
#��ԭ����ʽΪԭ����ʽ����
spread(longformat, attr, value)
longformat %>% spread(attr, value)
#����sep���µ�����Ϊ<key_name><sep><key_value>����ʽ
spread(longformat, attr, value, sep = '|')
#��ԭ����ʽΪԭ����ʽ���ݣ�car�е�ֵת��Ϊÿһ����
spread(longformat, car, value)
longformat %>% spread(car, value)
df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
#ת��Ϊ����ʽ��ת��Ϊ����ʽ��ʵ�ʻ�ԭΪԭdf��ֻ�Ǳ���˳��ͬ
df %>% spread(x, y) %>% gather(x, y, a:b, na.rm = TRUE)
df <- data.frame(row = rep(c(1, 51), each = 3),
                 var = c("Sepal.Length", "Species", "Species_num"),
                 value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))
#���ڻ�����͵�value�У�Ĭ��convert = FALSE��ת��������Ϊfactor����
df %>% spread(var, value)
#����convert = TRUE������ԭ����
df %>% spread(var, value, convert = TRUE)

#unite
library(dplyr)
#ʹ��Ĭ�ϵ����ӷ���_���ϲ�vs��am��Ϊ����vs_am����ɾ��vs��am��
unite(mtcars, vs_am, vs, am) 
mtcars %>% unite(vs_am, vs, am)
#ʹ��Ĭ�ϵ����ӷ���_���ϲ�vs��am��Ϊ����vs_am������vs��am��
unite(mtcars, vs_am, vs, am, remove = FALSE)
#ʹ�����ӷ���|���ϲ�vs��am��Ϊ����vs_am����ɾ��vs��am��
unite(mtcars, vs_am, vs, am, sep = '|')

#separate
library(dplyr)
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
#�ָ�Ϊ���У�����NAֵ
df %>% separate(x, c("A", "B"))
df <- data.frame(x = c("a", "a b", "a b c", NA))
#�ָ�Ϊ���У�����warning��ɾ��������У�ȱʧ���д�����NA���
df %>% separate(x, c("a", "b"))
#�ָ�Ϊ���У�ֱ��ɾ��������У�ȱʧ���д�����NA���
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
#�ָ�����(���õ���Ϊ����)��ȱʧ���д�����NA���
df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")
df <- data.frame(date = c("2017-03-08 01:20:20", "2017-03-09 02:30:30", "2017-03-10 03:40:40"))
#�ָ�Ϊyear,month,day,hour,minute,second����
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
#�ָ�y��z�У�ת��Ϊ��
separate_rows(df, y, z, convert = TRUE)


#Ƕ�׺�Ƕ�׻�ԭ
# ����Species���н���nest���������ɵ����е�Ԫ�ض�Ϊlist����
# ����nest()�ϲ�Ϊһ�У�chop()�ֱ�Ը��н��кϲ�
iris %>% nest(data = -Species)
iris %>% chop(cols = -Species)

nest_vars <- names(iris)[1:4]
iris %>% nest(data = one_of(nest_vars))
iris %>% chop(cols = one_of(nest_vars))

# �ֱ���϶��н��кϲ���chop()��֧�ִ˲���
iris %>% nest(petal = starts_with("Petal"), sepal = starts_with("Sepal"))

# �Ƚ��з���������������֮��ı��������nest�ϲ�
iris %>% group_by(Species) %>% nest()

# �Բ�ͬ���齨������ģ��
mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(models = lapply(data, function(df) lm(mpg ~ wt, data = df)))



df <- tibble(
  x = 1:4,
  y = list(NULL, 1:2, 3, NULL), 
  z = list('d', c('a', "b"), "c", NULL)
)
# չ��y��z�У�����ɾ��ֵ��ΪNULL���У���ȫΪNULL���б���
df %>% unnest(c(y, z))
df %>% unchop(c(y, z))
# չ��y��z�У�����keep_empty = TRUE����������
df %>% unnest(c(y, z), keep_empty = TRUE)
df %>% unchop(c(y, z), keep_empty = TRUE)
# չ��y��z�У�����չ�����е�����
df %>% unchop(c(y, z), ptype = tibble(y = character(), z = character()))
# ��ʹ��y��չ������ʹ��zչ������ֱ��ʹ��y,zչ����ͬ��y��z����еѿ�����
df %>% unchop(y) %>% unchop(z)


#ȱʧֵ����
#ʹ�ø���ֵ�滻ÿ�е�ȱʧֵ
library(dplyr)
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
#��0�滻x�е�NA����unknown�滻y�е�NA
df %>% replace_na(list(x = 0, y = "unknown"))

#��ǰһ��ֵ���ȱʧֵ��Ĭ�������������
df <- data.frame(x = 1:5, y = c(10, NA, 15, NA, 20))
#���������滻NAֵ
df %>% fill(y)
df %>% fill(y, .direction = "down")
#���������滻NAֵ
df %>% fill(y, .direction = "up")

#����Դ�������������ֵ����
#��������1��6
full_seq(c(1, 2, 4, 6), 1)
#periodֵ��ԭ���ݼ����ƥ�䣬����Error: `x` is not a regular sequence.
full_seq(c(1, 2, 4, 6), 2)
#��������1��13�����Ϊ2
full_seq(c(1, 5, 9, 13), 2)


#ɾ������ȱʧֵ����
df <- data_frame(x = c(1, 2, NA), y = c("a", NA, "b"))
#ɾ������x��NA��Ӧ����
df %>% drop_na(x)
#ɾ������y��NA��Ӧ����
df %>% drop_na(y)
#δ�����У�ɾ������x��y��NA��Ӧ����
df %>% drop_na()


#ת����ʽ��ȱʧֵΪ��ʽ��
df <- data_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
#��item_id��item_name�е�ÿ��Ԫ����չԭ���ݿ�, ��Ϻ��ȱʧֵ��NA����
df %>% complete(item_id, item_name)
df %>% complete(crossing(item_id, item_name))
#��item_id��item_name�е�ÿ��Ԫ����չԭ���ݿ�, ���Ը���ֵ�滻ȱʧֵ
df %>% complete(item_id, item_name, fill = list(group = 0, value1 = 0, value2 = 0))
#��item_id��item_name�е�ÿ��Ԫ����չԭ���ݿ�ֻ����ԭ���ݿ��д��ڵ����
df %>% complete(nesting(item_id, item_name))

#����group����item_id��item_name�е�ÿ��Ԫ����չԭ���ݿ�ֻ����ԭ���ݿ�
#��item_id��item_name���ڵ����
df %>% complete(group, nesting(item_id, item_name))
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0, value2 = 0))

#����group����item_id��item_name�е�ÿ��Ԫ����չԭ���ݿ򣬷�������item_id
#��item_name���ڵ����
df %>% complete(group, crossing(item_id, item_name))
df %>% complete(group, crossing(item_id, item_name), fill = list(value1 = 0, value2 = 0))