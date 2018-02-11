# 스파크에서 불려오기
# 1. 접속
library(sparklyr)
library(dplyr)
# 2. 스파크 테이블에 접속; 캐시에 저장
tbl_cache(sc, 'sales2017')
# 3. 스파크 테이블 값 저장
sales2017_tbl <- tbl(sc, 'sales2017')
# 4. 스파크 테이블 값을 R 데이터프레임으로 변환
information <- data.frame(sales2017_tbl)

install.packages("psych")
library(dplyr)

information[1:31,1] <- "1월"
information[32:59,1] <- "2월"
information[60:90,1] <- "3월"
information[91:120,1] <- "4월"
information[121:151,1] <- "5월"
information[152:181,1] <- "6월"
information[182:212,1] <- "7월"
information[213:243,1] <- "8월"
information[244:273,1] <- "9월"
information[274:304,1] <- "10월"
information[305:334,1] <- "11월"
information[335:365,1] <- "12월"
information[366:396,1] <- "1월"
information[397:424,1] <- "2월"
information[425:455,1] <- "3월"
information[456:485,1] <- "4월"
information[486:516,1] <- "5월"
information[517:546,1] <- "6월"
information[547:577,1] <- "7월"
information[578:608,1] <- "8월"
information[609:638,1] <- "9월"
information[639:669,1] <- "10월"
information[670:699,1] <- "11월"
information[700:730,1] <- "12월"
information[731:761,1] <- "1월"
information[762:789,1] <- "2월"
information[790:820,1] <- "3월"
information[821:850,1] <- "4월"
information[851:881,1] <- "5월"
information[882:911,1] <- "6월"
information[912:942,1] <- "7월"
information[943:973,1] <- "8월"
information[974:1003,1] <- "9월"
information[1004:1034,1] <- "10월"
information[1035:1064,1] <- "11월"
information[1065:1095,1] <- "12월"
library(psych)
pairs.panels(information[-c(2:1)])
library(ggplot2)
library(dplyr)
# 가게별 매출비교
info.1 <- information %>% group_by(장소) %>% summarise(sales = mean(매출액))
ggplot(data = info.1, aes(x= 장소, y=sales, fill = 장소)) + geom_bar(stat = "identity")
info.1

# 가계별 매출 비교2
install.packages("plotrix") # pie3D를 사용하기 위해 설치한다.
library(plotrix)

info.5 <- c(info.1$sales)

gd_rate <- round(info.5/sum(res_total2)*100, 1) #남녀 성별 백분율 구하고
gd_labels <- paste(gd_rate, "%") # 그걸 퍼센트로 낸다
# 3D파이 그래교
pie3D(info.5,
      main= "가계별 매출비교",
      col=rainbow(length(info.5)), 
      labels=gd_labels,
      explode = 0.05
)
legend(0.5,1,c("서초","삼성","잠실"), cex=0.8, fill=rainbow(length(info.5)))

# 월별 가게 매출 비교1
info.2 <- ggplot(information, aes(x = 날짜, y = 매출액, fill = 장소)) + geom_boxplot()
info.2 <- info.2 + theme(axis.text.x = element_text(angle=10, hjust=1, vjust=1))
info.2
# 월별 가게 매출 비교2
info.3 <- ggplot(information, aes(x = 날짜, y = 매출액, fill = 장소)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월"))
info.3

# 월별 가게 매출 비교3
info.4 <- information %>%  group_by(날짜, 장소) %>%  summarise(sales = mean(매출액))
info.4 <- arrange(info.4, 장소)
ggplot(info.4, aes(x = 날짜, y = sales, group = 장소, color = 장소, linetype = 장소)) + geom_line(lwd=2) + geom_point(lwd=4)

