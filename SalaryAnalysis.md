108-2 大數據分析方法 作業一
================
Chun-Han Chen

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
（107年）<https://data.gov.tw/dataset/6647>
（104-105年）<http://ipgod.nchc.org.tw/dataset/a17000000j-020066>
，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 一、比較104年度和107年度大學畢業者的薪資資料

### 1-0.資料匯入與處理

``` r
#載入套件
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#分別讀入107與104年的資料
Data107 <- read_csv("~/Downloads/4133da254dbcdba28a2097de48d8d606_csv/107年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
Data104 <- read_csv("~/Downloads/a17000000j-020066-mah.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#另外存成變數以防原始資料被更動
DataWork107<-Data107
DataWork104<-Data104
#用迴圈將底線和...取代成空字串
for(n in 1:14)
  {DataWork107[[n]]<-gsub("—|…","",DataWork107[[n]])
  DataWork104[[n]]<-gsub("—|…","",DataWork104[[n]])}
#用迴圈將所有資料轉換成數值
for(n in 3:14)
  {DataWork107[[n]]<-as.numeric(DataWork107[[n]])
  DataWork104[[n]]<-as.numeric(DataWork104[[n]])}
```

### 1-1.107年度薪資較104年度薪資高的職業有哪些?

#### 按照提高比例由大到小排序(3分)，呈現前十名的資料(2分)，並用文字說明結果(10分)。

``` r
#統一兩個表當中的用詞
DataWork104[[2]]<-gsub("部門","",DataWork104[[2]])
DataWork104[[2]]<-gsub("、","_",DataWork104[[2]])
DataWork107[[2]]<-gsub("營建工程","營造業",DataWork107[[2]])
#因104年的職業別不一定出現在107年(如教育業)，因此使用left_join()
DataJoin<-left_join(DataWork104,DataWork107,by="大職業別")

#將研究所薪資除以大學薪資的比例結果儲存到新的欄位
DataJoin$PriceUpPercentage<-DataJoin$`大學-薪資.y`/DataJoin$`大學-薪資.x`
#計算薪資有調漲的個數
DataJoin$UP<-ifelse(DataJoin$PriceUpPercentage>0,"yes","no")
table(DataJoin$UP)
```

    ## 
    ## yes 
    ## 101

``` r
#使用order排序並做成子集，利用head()取出前十名
knitr::kable(head(DataJoin[order(DataJoin$PriceUpPercentage,decreasing = T),c(2,28)],10))
```

| 大職業別                       | PriceUpPercentage |
| :------------------------- | ----------------: |
| 專業\_科學及技術服務業-服務及銷售工作人員     |          1.158908 |
| 不動產業-技術員及助理專業人員            |          1.130807 |
| 住宿及餐飲業-服務及銷售工作人員           |          1.108842 |
| 藝術\_娛樂及休閒服務業-事務支援人員        |          1.097632 |
| 金融及保險業-技藝\_機械設備操作及組裝人員     |          1.097194 |
| 用水供應及污染整治業-專業人員            |          1.086330 |
| 住宿及餐飲業                     |          1.081297 |
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員 |          1.072507 |
| 營造業-服務及銷售工作人員              |          1.071877 |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員   |          1.071044 |

Ans:

  - 以上前十名薪資被提高的職業當中，機械設備操作及組裝人員比較高，可能是因為因應時代的演進，
    機器設備逐漸更新與進步，需要提高薪資來吸引更具技術性的人員來工作

### 1-2.提高超過5%的的職業有哪些?

``` r
#篩選漲薪超過5％(1.05)的職業
DataJoin$Over_1.05<-ifelse(DataJoin$PriceUpPercentage>1.05,"yes","no")
DataUP1.05<-DataJoin[grepl("yes",DataJoin$Over_1.05),"大職業別"]
knitr::kable(DataUP1.05)
```

| 大職業別                         |
| :--------------------------- |
| 工業及服務業-服務及銷售工作人員             |
| 工業-專業人員                      |
| 工業-技術員及助理專業人員                |
| 工業-技藝\_機械設備操作及組裝人員           |
| 製造業-專業人員                     |
| 製造業-技術員及助理專業人員               |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員     |
| 用水供應及污染整治業                   |
| 用水供應及污染整治業-專業人員              |
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員   |
| 營造業                          |
| 營造業-專業人員                     |
| 營造業-事務支援人員                   |
| 營造業-服務及銷售工作人員                |
| 服務業-服務及銷售工作人員                |
| 批發及零售業-服務及銷售工作人員             |
| 運輸及倉儲業                       |
| 運輸及倉儲業-技術員及助理專業人員            |
| 運輸及倉儲業-事務支援人員                |
| 運輸及倉儲業-服務及銷售工作人員             |
| 運輸及倉儲業-技藝\_機械設備操作及組裝人員       |
| 住宿及餐飲業                       |
| 住宿及餐飲業-事務支援人員                |
| 住宿及餐飲業-服務及銷售工作人員             |
| 住宿及餐飲業-技藝\_機械設備操作及組裝人員       |
| 金融及保險業                       |
| 金融及保險業-專業人員                  |
| 金融及保險業-技術員及助理專業人員            |
| 金融及保險業-服務及銷售工作人員             |
| 金融及保險業-技藝\_機械設備操作及組裝人員       |
| 不動產業                         |
| 不動產業-技術員及助理專業人員              |
| 不動產業-事務支援人員                  |
| 不動產業-服務及銷售工作人員               |
| 不動產業-技藝\_機械設備操作及組裝人員         |
| 專業\_科學及技術服務業-服務及銷售工作人員       |
| 支援服務業-服務及銷售工作人員              |
| 藝術\_娛樂及休閒服務業                 |
| 藝術\_娛樂及休閒服務業-專業人員            |
| 藝術\_娛樂及休閒服務業-技術員及助理專業人員      |
| 藝術\_娛樂及休閒服務業-事務支援人員          |
| 藝術\_娛樂及休閒服務業-技藝\_機械設備操作及組裝人員 |
| 其他服務業-服務及銷售工作人員              |

### 1-3.主要的職業種別是哪些種類呢?

``` r
#主要的職業類別，取出大職業別中"-" 前面的字串，並分析出現次數
upjobclass<-strsplit(DataUP1.05$大職業別,"-")
upjobclass2<-lapply(upjobclass, "[", 1)
knitr::kable(table(unlist(upjobclass2)))
```

| Var1         | Freq |
| :----------- | ---: |
| 不動產業         |    5 |
| 電力及燃氣供應業     |    1 |
| 服務業          |    1 |
| 工業           |    3 |
| 工業及服務業       |    1 |
| 金融及保險業       |    5 |
| 批發及零售業       |    1 |
| 其他服務業        |    1 |
| 藝術\_娛樂及休閒服務業 |    5 |
| 營造業          |    4 |
| 用水供應及污染整治業   |    3 |
| 運輸及倉儲業       |    5 |
| 支援服務業        |    1 |
| 製造業          |    2 |
| 住宿及餐飲業       |    4 |
| 專業\_科學及技術服務業 |    1 |

Ans:

  - 主要是不動產業(5個)，金融保險業(5個)，藝術\_娛樂及休閒服務業(5個)，運輸及倉儲業
    (5個)

  - 前兩者可能因為景氣的回升所以帶動薪資成長，而休閒產業可能是因為網路時代的興起，運輸業則可能是因為電商業者銷售額提高導致運輸量提高而增加薪資。

## 二、男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 2-1.104和107年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#104年大學畢業男女薪資排序(小到大)
knitr::kable(head(DataWork104[order(DataWork104$`大學-女/男`,decreasing = F),c(2,12)],10))
```

| 大職業別                       | 大學-女/男 |
| :------------------------- | :----: |
| 電力及燃氣供應業-技藝\_機械設備操作及組裝人員   | 91.69  |
| 教育服務業-服務及銷售工作人員            | 91.90  |
| 礦業及土石採取業-技術員及助理專業人員        | 92.42  |
| 礦業及土石採取業-技藝\_機械設備操作及組裝人員   | 93.10  |
| 礦業及土石採取業                   | 95.28  |
| 其他服務業-事務支援人員               | 95.47  |
| 營造業-技藝\_機械設備操作及組裝人員        | 95.64  |
| 用水供應及污染整治業-技藝\_機械設備操作及組裝人員 | 95.90  |
| 營造業                        | 96.35  |
| 教育服務業                      | 96.44  |

``` r
#107年大學畢業男女薪資排序(小到大)
knitr::kable(head(DataWork107[order(DataWork107$`大學-女/男`,decreasing = F),c(2,12)],10))
```

| 大職業別                 | 大學-女/男 |
| :------------------- | :----: |
| 礦業及土石採取業-專業人員        | 96.02  |
| 電力及燃氣供應業-事務支援人員      | 96.96  |
| 礦業及土石採取業             | 97.11  |
| 營造業                  | 97.52  |
| 教育業-事務支援人員           | 97.61  |
| 電力及燃氣供應業-服務及銷售工作人員   | 97.73  |
| 營造業-技術員及助理專業人員       | 97.78  |
| 用水供應及污染整治業-專業人員      | 97.81  |
| 用水供應及污染整治業-服務及銷售工作人員 | 97.94  |
| 電力及燃氣供應業-專業人員        | 98.23  |

### 2-2.哪些行業女生薪資比男生薪資多?

``` r
#女生薪資大於男性的行業(104年)
DataWork104$FemaleMore<-ifelse(DataWork104$`大學-女/男`>100,"yes","no")
FemaleMoretable104<-DataWork104[grepl("yes",DataWork104$FemaleMore),"大職業別"]
knitr::kable(FemaleMoretable104)
```

| 大職業別                         |
| :--------------------------- |
| 專業\_科學及技術服務業-技藝\_機械設備操作及組裝人員 |

``` r
#女生薪資大於男性的行業(107年)
DataWork107$FemaleMore<-ifelse(DataWork107$`大學-女/男`>100,"yes","no")
FemaleMoretable107<-DataWork107[grepl("yes",DataWork107$FemaleMore),"大職業別"]
knitr::kable(FemaleMoretable107)
```

| 大職業別 |
| :--- |

### 2-3.現象分析說明

Ans: - 從以上結果可以得知以下幾點：

  - 不管是在104年或107年，男生薪資大於女生的行業別通常落在水、電、礦業、營造等較粗活的工作

  - 而在女生薪資比男生薪資高的部分，很明顯104年僅有一個，107年甚至沒有

  - 以上很顯著的呈現男女同工不同酬的現象，而且經過3年之後，仍然無法獲得改善

  - 在男女失衡的行業上，多為粗活的工作，顯示對女生軟弱的既定刻板印象

## 三、研究所薪資差異

### 3-1.以107年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

#### 請按照薪資差異比例由大到小排序(3分)，呈現前十名的資料(2分)，並用文字說明結果(10分)

``` r
#將研究所薪資除以大學薪資的比例結果儲存到新的欄位
DataWork107$PriceUpPercentage<-DataWork107$`研究所-薪資`/DataWork107$`大學-薪資`
#將完整的欄位（即職業別）取出並另存到新dataframe
DataWork107COM<-DataWork107[complete.cases(DataWork107),]

#使用order排序並做成子集，利用head()取出前十名
knitr::kable(head(DataWork107COM[order(DataWork107COM$PriceUpPercentage,decreasing = T),c(2)],10))
```

| 大職業別              |
| :---------------- |
| 其他服務業             |
| 專業\_科學及技術服務業      |
| 出版、影音製作、傳播及資通訊服務業 |
| 製造業               |
| 工業                |
| 工業及服務業            |
| 用水供應及污染整治業        |
| 批發及零售業            |
| 服務業               |
| 醫療保健業             |

Ans:以下分別列出前十名職業:

  - 1.其他服務業
  - 2.專業\_科學及技術服務業
  - 3.出版、影音製作、傳播及資通訊服務業
  - 4.製造業
  - 5.工業
  - 6.工業及服務業
  - 7.用水供應及污染整治業
  - 8.批發及零售業
  - 9.服務業
  - 10.醫療保健業

從結果上面可以分析，「其他服務業」涵蓋了許多專業技術人員，因此可能在學歷的晉升上， 習得更多技能，使得研究所的薪資與大學薪資差異較大。
此外，「醫療保健業」可能排名較後面的原因，可能是因為職業本身較具有高度專業性，
大學薪資本來就高，而學歷晉升所提升的薪資比例就會看起來較低。

## 四、我有興趣的職業別薪資狀況分析

### 4-1.有興趣的職業別篩選，呈現薪資

``` r
#呈現有興趣的職業別，並顯示大學薪資與研究所薪資
LoveJob<-DataWork107COM[grepl(c("出版、影音製作、傳播及資通訊服務業|工業及服務業"),DataWork107COM[[2]]),c(2,11,13)]
```

Ans: 比較有興趣的職業別分別是「出版、影音製作、傳播及資通訊服務業」與「工業及服務業」

### 4-2.這些職業別研究所薪資與大學薪資差多少呢？

``` r
#計算新資差異
LoveJob$range<-LoveJob$`研究所-薪資`-LoveJob$`大學-薪資`
```

Ans:

  - 「出版、影音製作、傳播及資通訊服務業」相差5031元
  - 「工業及服務業」相差5360元

### 4-3.請問此薪資與妳想像中的一樣嗎?

Ans:

  - 對於「工業及服務業」的研究所薪資覺得應該要再高一點，
    畢竟此類的技術含量可能會比較高，不過對於「出版、影音製作、傳播及資通訊服務業」來說，
    則沒有太大的落差

### 4-4.會因為這樣改變心意，決定念/不念研究所嗎?

Ans:

  - 會，因為此兩種行業別在一題的薪資差異排行榜中名列前茅，代表晉升高學歷有非常有助於提高薪資水準。
