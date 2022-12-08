# 更改文件路径的方法
setwd #("C:/Documents") #如要将工作目录改到C盘文档
# 选中要运行的script，ctrl+回车即可运行选中的这几行，如：可以生成3
# R语言中的赋值符号为<-  快捷键是alt -
A <- 1
B <- 2
C <- A+B
C
# ls 生成处于活动状态的对象列表
ls()
# 生成结果： "A" "B" "C"

# 从工作区中删除对象A
rm(A)
# 可以看到右边environment表格中A行已删除

# 绘图函数plot()用法
# 运行一组1-100和101-200的数据，1；100表示生成介于1和100之间的整数
Data1 <- c(1:100)
Data2 <- c(101:200)
plot(Data1,Data2,col="red")

# 输入两组100个vector的数据(normally distributed)，设置分别的平均值mean和标准差数值
Data3 <- rnorm(100, mean = 53,sd=34)
Data4 <- rnorm(100, mean = 64,sd=14)
# 以data3,data4分别为x,y轴作图，散点颜色设为蓝色
plot(Data3,Data4, col="blue")

# 如果要了解函数的作用，输入？后面加函数名称，如
?plot()
# 关于plot函数的解释即显示在右下角help窗口

#R最常见的数据结构由向量vector(1D)，矩阵matrix(2D)，数组Array(nD)，列表List(1D)和数据帧Data frame(2D)
# data frame是最灵活的一种形式，数据存在x和y中
df <- data.frame(Data1,Data2)
plot(df, col="green")
# 即生成一个绿色连续整数的散点图
# 如果data frame很大，查看部分行：
library(tidyverse)
# 看前十行
df %>%
  head()
# 看后十行
df %>%
  tail()
###我这里只输出了6行，怎么调整？【可能只有6行？

# 选择data frame中部分元素或行和列范围
# data.frame[row,column] #这一步不要输入

# 用df取数据框的子集，逗号前为行，逗号后为列
df[1:10,1] #data1即第一列，输出前10行
df[5:15,]  #没有规定第几列，即输出所有列的5-15行
df[c(2,3,6),2] #选择2 3 6行，data2的元素
df[,1] #没有规定第几行，即输出data1(第一列)所有行的元素

# 更改列标题
library(dplyr)  #用于将数据改为适合的格式
df <- df %>%
  dplyr::rename(column1 = Data1, column2 = Data2)

# 按名词选择或引用列
df %>%
  dplyr::select(column1)
# 为什么生成了两列1到100的整数

df$column1
#列出data1数据

df[['column1']]
# 结果同上

# 2.5将数据读入R
# LondonDataOSK<- read.csv("GISW2\\LondonData.csv", 
#                          header = TRUE, 
#                          sep = ",",  
#                          encoding = "latin1")
#2.5.1 上面这步报错了
# Note, I’ve made an R project for all these practicals, which is why my file path starts with prac2_data/. 

getwd() 
# 获取R当前的工作路径

# If you save the .csv in the same folder as the .Rproj then you can just use:
LondonDataOSK<- read.csv("LondonData.csv", 
                         sep=",")
# 从R project打开项目文档即可成功读取

# 下面是更简单的read csv方法
install.packages("here")
library(here)
here::here() # 指出文件路径

# read_csv(here("prac2",""))
install.packages("here")
library(here)
here()

# 有点类似新建文件夹
# 如 here("新建一级"，“新建二级”，“新建三级”)
# 输出结果："/E盘/CASA_practice/GIS/W2GIS/新建一级/新建二级/新建三级"


library(readr) # 注意从网站读取前
# 直接从网页读取csv
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
# read_csv和read.csv区别：都是用来读取CSV格式的文件，
# 但是 read_csv 读取文件后会保持原始数据的列名称，
# 并且会将数据转换成 tbl_df 格式

# 2.5.4 examining data
# check what data type my new data set is 方法一
class(LondonData) 
# 结果：spec_tbl_df    tbl_df   tbl  data.frame

# 方法2 old skool data
class(LondonDataOSK)
# 结果:只有data.frame


# dplyr包中的summarise_all() 和tidyr包中的pivot_longer()也可以检查数据是否read in correctly
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# 输出以下结果
## # A tibble: 67 × 2
##    All_variables                      Variable_class
##    <chr>                              <chr>         
##  1 Ward name                          character     
##  2 Old code                           character     
##  3 New code                           character     
##  4 Population - 2015                  numeric       
##  5 Children aged 0-15 - 2015          numeric       
##  6 Working-age (16-64) - 2015         numeric       
##  7 Older people aged 65+ - 2015       numeric       
##  8 % All Children aged 0-15 - 2015    numeric       
##  9 % All Working-age (16-64) - 2015   numeric       
## 10 % All Older people aged 65+ - 2015 numeric       
## # … with 57 more rows
## # ℹ Use `print(n = ...)` to see more rows


# 快速编辑数据
LondonData <- edit(LondonData)
# 在R中出现类似excel的弹窗，可以双击表格直接修改

# 一次性计算 平均数，中位数median等统计结果
summary(df)

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))

#向右操作符，forward-pipe operator
# 就是把左侧准备的数据或表达式，传递给右侧的函数调用或表达式进行运行
library(dplyr) #用管道传参需要这个包，!没有library会报错
LondonData %>% 
  colnames() %>% 
  # just look at the head, top5
  head()

#结果：输出了每一列的标题
# [1] "Ward name"                  "Old code"                   "New code"                  
# [4] "Population - 2015"          "Children aged 0-15 - 2015"  "Working-age (16-64) - 2015"



#  2.5.5 Data manipulation in R



# 2.5.5.1 select rows
library(dplyr) 
LondonBoroughs<-LondonData[626:658,]

LondonBoroughs<-LondonData%>%
  slice(626:658)  # slice选择626-658行的数据，即伦敦borough所有ward的数据

Femalelifeexp<- LondonData %>% 
filter(`Female life expectancy -2009-13`>90)

library(stringr)
library(dplyr)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09")) 

# 选取列名为New code的column,行名为E09(伦敦城的编码)开头的row
# str_detect 匹配字符串的字符..
# ..如
## val <- c("abca4", 123, "cba2")
## 检查字符串向量，是否包括a
## str_detect(val, "a")
##[1]  TRUE FALSE  TRUE

## 检查字符串向量，是否以a为开头
## str_detect(val, "^a")
##[1]  TRUE FALSE FALSE

## 检查字符串向量，是否以a为结尾
## str_detect(val, "a$")
##[1] FALSE FALSE FALSE


#查看结果
LondonBoroughs$`Ward name`

# 另一种查看结果：每个城市占了一行
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

# 输出了对应行的城市名称,可以看到因为原始数据中有两行都是city of london，在这里也出现了两次

## [1] "City of London"         "City of London"         "Barking and Dagenham"  
## [4] "Barnet"                 "Bexley"                 "Brent"                 
## [7] "Bromley"                "Camden"                 "Croydon"               
## [10] "Ealing"                 "Enfield"                "Greenwich"             
## [13] "Hackney"                "Hammersmith and Fulham" "Haringey"              
## [16] "Harrow"                 "Havering"               "Hillingdon"            
## [19] "Hounslow"               "Islington"              "Kensington and Chelsea"
## [22] "Kingston upon Thames"   "Lambeth"                "Lewisham"              
## [25] "Merton"                 "Newham"                 "Redbridge"             
## [28] "Richmond upon Thames"   "Southwark"              "Sutton"                
## [31] "Tower Hamlets"          "Waltham Forest"         "Wandsworth"    

# distinct() 去掉重复的行 remove duplicate rows 
library(dplyr)
LondonBoroughs<-LondonBoroughs %>%
  distinct()


# 2.5.5.2 Selecting columns 



# c() conbine合并
# select columns 1,19,20 and 21 方法1
library(dplyr)
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]

# select columns 1,19,20 and 21 方法2
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

# select columns that contain certain words …
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 


# 2.5.5.3 renaming columns
library(janitor) 
# janitor: remove大写字母capital,空格space变成下划线underscore
LondonBoroughs <- LondonData %>%
  dplyr::rename(Borough=`Ward name`)%>%  # 故意下划线改空格，便于演示错的改成对的
  clean_names()

LondonBoroughs <- LondonBoroughs %>%
  #here the ., means all data
  clean_names(., case="snake")
# case还可以是small_camel，upper_camel大写，lower_camel小写，title，snake 都是格式
#   snake格式的特殊说明：each space is replaced with an underscore (_) ..
#   ..and the first letter of each word is written in lowercase.


# 2.5.5.4 More dplyr verbs


# ① mutate() 在现有变量基础上（通过等式运算）建立新变量，主要是创建新列 

# mutate(要修改的数据框df,新变量名称= 现有的变量*2）类似这样的运算

library(dplyr)
LondonBoroughs<-LondonData %>% 
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy2009_13 + male_life_expectancy2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeexpectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeexpectancy)%>% arrange(desc(normalisedlifeexpectancy))

  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)


#top of data 展示前五行
Life_expectancy <- LondonBoroughs %>% 
library(dplyr)   #如果保存了的话要重新library
slice_head(Life_expectancy, n=5)

## # A tibble: 5 × 4
##   new_code  borough                averagelifeexpectancy normalisedlifeepectancy
##   <chr>     <chr>                                  <dbl>                   <dbl>
## 1 E09000001 City of London                          86.4                    1.05
## 2 E09000020 Kensington and Chelsea                  84.4                    1.03
## 3 E09000015 Harrow                                  84.2                    1.02
## 4 E09000027 Richmond upon Thames                    83.9                    1.02
## 5 E09000033 Westminster                             83.8                    1.02



# 2.5.5.5 Levelling up with dplyr



# 对比伦敦和UK的平均寿命 case_when()
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2  # 只有输入了这行这个2的表格才会在右边出现

# 分别计算above和below数据的range count 和average
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group


# 对比各Boroughs和全国平均寿命 

# dplyr:: across(） transformation across the columns selected

Life_expectancy3 <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%  
  mutate(across(where(is.numeric), round, 3))%>%  # 将是numeric类型的数据round，3，即保留三位小数
  mutate(across(UKdiff, round, 0))%>%  
  # round()根据十进制值对数字进行round off（四舍五入） the numbers based on decimal values
  #如果UKdiff差值小于0
  # 可是  is.numeric(-1.5)
  ## [1] TRUE
  
  
### 这里插入一个简化casewhen的办法
install.packages('tibbles')
install.packages('stringr')
library(tibble)
library(purrr)
library(stringr)
library(dplyr)
allCaseWhen <<- function(x,pattern,result){
  x1 <- str_flatten(map2_chr(result,pattern,~str_glue("str_detect(x,'{.y}')~'{.x}',")))
  x2 <- str_glue("function(x) case_when({x1})")
  fx <- eval(parse(text=x2))
  fx({{x}})}

# 1 表格形式列出条件
conditions <- tribble(
    ~pattern,~result,
    'averagelifeexpectancy >= 81','str_c("equal or above UK average by","years",sep=" ")',
    'TRUE','str_c("below UK average by","years",sep=" ")',
  )

conditions <- tribble(
  ~pattern,~result,
  '^a','str_c("equal or above UK average by","years",sep=" ")',
  '^b','str_c("below UK average by","years",sep=" ")',
)

# 1 也可以是新建Excel，建立以pattern和result为列名的表格
# ..然后复制单元格，用以下代码读进R
conditions <- clipr::read_clip_tbl()



# 因为此时表格中UKCompare一列都是below或者above，就可以用^a区分出above，^b区分出below


# 2 运行all Case When
tibble(UKcompare=stringr::UKcompare) %>% 
  mutate(
    category=
      allCaseWhen(UKcompare,
                  conditions$pattern, #读取条件
                  conditions$result  #返回结果
      )
  )
  group_by(UKcompare)%>%  #根据不同列中的值对行进行分组
  summarise(count=n())



## 下面的老师范例做法
  library(dplyr)
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3

# str_c() function from the stringr package 字符串拼接
# str_c(..., sep = "", collapse = NULL)
# ...指的多参数的输入，sep 把多个字符串拼接为一个大的字符串，用于字符串分割符
# collapse 把多个向量参数拼接为一个大的字符串，用于字符串的分割符

# “sep ”determines how these two vectors are separated

# 如：
## > str_c('a','b')
##  "ab"
## > str_c('a','b',sep='-')
##  "a-b"
## > str_c(c('a','a1'),c('b','b1'),sep='-')
##  "a-b"   "a1-b1"

# 输出结果：
## # A tibble: 7 × 2
##   UKcompare                            count
##   <chr>                                <int>
## 1 below UK average by -1 years             3
## 2 below UK average by 0 years              5
## 3 equal or above UK average by 0 years     4
## 4 equal or above UK average by 1 years    12
## 5 equal or above UK average by 2 years     4
## 6 equal or above UK average by 3 years     4
## 7 equal or above UK average by 5 years     1


Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))

## Warning: Predicate functions must be wrapped in `where()`.
## 
##   # Bad
##   data %>% select(is.numeric)
## 
##   # Good
##   data %>% select(where(is.numeric))
## 
## ℹ Please update your code.
## This message is displayed once per session.



# 2.5.6 Ploting
library(tidyverse)
dplyr::rename(LondonBoroughs, children_obese2011_12to2013_14 = percent_children_in_reception_year_who_are_obese2011_12to2013_14)
# 列名没有改成功

plot(LondonBoroughs$male_life_expectancy2009_13,LondonBoroughs$percent_children_obese_2011_12_to_2013_14)
# 横坐标的名字没有显示，只显示为Index



# 2.5.7 Pimp my graph加工图表
install.packages("plotly")
library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese2011_12to2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")


#  2.5.8 Spatial Data in R

install.packages("maptools")
install.packages(c("classInt", "tmap"))
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", 
                   "geojsonio"))
install.packages('rgdal', type = "source", configure.args=c('--with-proj-include=/usr/local/include','--with-proj-lib=/usr/local/lib'))


#Load Packages (ignore any error messages about different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(here)
# rgdal要在library sp后加载
# 如果install显示成功了但加载不出来可以restart R studio


# 2.5.8.2 Making some choropleth maps等值线地图
setwd("E:/CASA_practice/GIS/prac2_data")

# read geojson
EW <- st_read("E:/CASA_practice/GIS/prac2_data/Local_Authority_Districts_2016.geojson")

# read shp, EW is a new dataset to be created
EW <- st_read(here::here("Local_Authority_Districts_2016",
                         "Local_Authority_Districts_December_2016_FCB_in_the_UK.shp"))


# pull out london from UK, London (ID starts with E09) from the ‘lad15cd’ column 
library(stringr)
library(sf)
LondonMap<- EW %>%
  filter(str_detect(lad16cd, "^E09"))

#plot it using the qtm function
qtm(LondonMap)


# 2.5.8.3 Attribute data
# join attribute data to boundaries
library(janitor)
LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad16cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad16cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad16cd,
           .keep_all = TRUE)
# merge() as default is the same as an inner join(交集)…
# always use a join type (e.g. left_join()) where possible 
# ..as it is easier to control how the join works


# join geographical data with census data
library(janitor)
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad16cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad16cd" = "new_code"))

distinct(BoroughDataMap2,lad16cd,
         .keep_all = TRUE)
# 记得用distinct删除重复行
# 如需删除特定位置的distinct(dat, longitue, latitude, .keep_all = TRUE)
# 根据多列删除，distinct(dat,col1, col2, .keep_all = TRUE)


# 2.5.9 Simple mapping
## tm_fill() # fill of polygon
## tm_borders() # only display bordr of polygons
## tm_polygon() # =tm_fill() + tm_borders()

library(tmap)
library(tmaptools)

tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

# 导入OpenstreetMap 在线地图
#  create a bounding box around London
install.packages("osmdata")
library(osmdata)
library(sf)
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = NULL)


#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")


# style — how to divide the data into out colour breaks
# palette — the colour scheme to use


# 设置图像格式
library(tmap)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2016", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))


# 2.6 Tidying Data

# read in data, force columns to the appropriate data types (e.g. text, numeric)
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)

# 方法一 convert the tibble into a tidy tibble
# 用这个函数pivot_longer(），将每行名为area，列为各种flytipping类型
# ..改为列头只有tipping type和各种type的count
#...行头有多行同样area的行
library(tidyverse)
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )

view(flytipping_long)

# 方法二
flytipping2 <- flytipping1[,1:4]
# 只计算出每个地区的total_incidents
# 行中无重复的Area

# 另一种表格形式，列头为每年不同tipping type
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

# 方法四 列头只有年份 统计的是每年的total
# if you were just interested in a specific varaible 
# and wanted the coloums to be each year of the data…
widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)

# join flytipping to londonborough
library(janitor)
library(dplyr)
flytipping2 <- LondonMap %>% 
  clean_names() %>%
  left_join(LondonMap, 
            widefly,
            by = c("code" = "lad16cd"))

distinct(flytipping2,lad16cd,
         .keep_all = TRUE)
# 以上步骤报错