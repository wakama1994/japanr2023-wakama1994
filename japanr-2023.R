library('tidyverse')
library('ggplot2')
path <- "~/Desktop/japanr/サンプル - スーパーストア.xls - 注文.csv"
# データ読み込み
df <- read_csv(path)
# 時系列の前処理
df <- df %>% 
  mutate(オーダー日 = as.Date(オーダー日))
# ① 月別売上を前年との比較
df3<- df%>%filter(地域=='関西地方')%>%filter(オーダー日>= '2022-01-01',  オーダー日<= '2022-12-31')%>%group_by(month = lubridate::floor_date(オーダー日, 'month'))%>%summarize(sum = sum(売上))

df2<- df%>%filter(地域=='関西地方')%>%filter(オーダー日>= '2021-01-01',  オーダー日<= '2021-12-31')%>%group_by(month = lubridate::floor_date(オーダー日, 'month'))%>%summarize(sum = sum(売上))


p1<- ggplot() + geom_bar(data=df3, 
aes(x=month(month), y=sum), 
alpha=0.4, color='orange', 
fill='orange',stat = "identity")+geom_line(data=df2, aes(x=month(month), y=sum)
,color='blue') +labs(x='月'
,y="売上(円)") +theme_bw(base_family = "HiraKakuPro-W3") + ggtitle("①月別売上の前年比較")+ scale_fill_discrete(name="年代",
breaks=c(df2,df3),labels=c("2022年", "2021年"))
##scale_fill_discreteで凡例が出なかった.....
# ② 都道府県ごとのカテゴリー別売上
df4<- df%>%filter(地域=='関西地方')
p2  <- ggplot(df4, aes(x = reorder(x=都道府県,X=-売上,
FUN=sum), y = 売上, 
fill = カテゴリ))+ geom_bar(stat = "identity")+theme_bw(base_family = "HiraKakuPro-W3") + ggtitle('② 都道府県ごとのカテゴリー別売上')+theme_bw(base_family = "HiraKakuPro-W3") + labs(x='都道府県',
y='売上(円)')
# ③サブカテゴリーごとの売上
df5<- df%>%filter(地域=='関西地方')%>%group_by(サブカテゴリ)%>%summarize(sum = sum(売上))
p3<- ggplot(df5, aes(x ='' 
, y = sum, 
fill = サブカテゴリ)) + geom_bar(stat = "identity", 
position = "stack") +coord_polar(theta = 'y',start=0) +theme_bw(base_family = "HiraKakuPro-W3")+ ggtitle('③サブカテゴリーごとの売上')
# ３つ出す（重なってしまう）
library("cowplot")
plot_grid(p1, p2, p3, align = "h")
