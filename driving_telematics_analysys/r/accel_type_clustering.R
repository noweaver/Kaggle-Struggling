setwd("/home/vcrm_6442267/workspace/r/kaggle")

# 캐글 데이터 가져오기
result = sqlQuery( conn, "select * from kaggle.driver_fixed_speed4
                   where user_id = 1
                   order by user_id, trip_id, seq_num" )


# 두 가감속 구간 시계열데이터 DTW 계산
align <- dtw(result[result$trip_id==1 & result$section_type_seq==1,"velocity"], 
             result[result$trip_id==1 & result$section_type_seq==22,"velocity"], step=asymmetricP1, keep=T)
dtwPlotTwoWay(align)


# DTW계산을 위해 테이블을 unpivot
library(reshape2)
head(result2)
tripData <- dcast(result, user_id + trip_id + section_type + section_type_seq ~ section_type_num, value.var = "velocity")
#for (i in 3:202) names(tripData)[i]= paste("t_",i-2, sep="")

# 한 사용자의 속도 구간별 DTW를 계산
#distMatrix <- dist(tripData[,-(1:3)], method="dtwOmitNA")

#레이블 지정
labs <- do.call("paste", c(tripData[c(1:4)], sep="_"))
rownames(tripData) <- labs
#tripData$trip_id==1 & 
testdf = tripData[tripData$section_type=="S03" & tripData$"7" > 0,]
distMatrix <- dist(testdf, method="dtwOmitNA")

# distance matrix를 결과 테이블로 변환
resultdf <- data.frame(t(combn(rownames(testdf),2)), as.numeric(distMatrix))
names(resultdf) <- c("section_1", "section_2", "distance")
# 거리로 정렬
resultdf <- resultdf[order(-resultdf$distance),]

# 클러스터링
hc <- hclust(distMatrix, method="average")

#시각화
plot(hc, main="")
rect.hclust(hc, k=2, border="red")
groups <- cutree(hc, k=c(2))
testdf <- cbind(testdf, groups)

graphics.off()    # close graphics windows

result2 <- result[user_id==1 & trip_id==1,]
attach(result2)
a <- ggplot(data=result2[section_type=="A03" & section_type_seq==15,], aes(section_type_num, velocity, fill=accel_type)) + 
  geom_bar(stat="identity") + ggtitle("1_1_A03_15")
b <- ggplot(data=result2[section_type=="A03" & section_type_seq==155,], aes(section_type_num, velocity, fill=accel_type)) + 
  geom_bar(stat="identity") + ggtitle("1_1_A03_155")
c <- ggplot(data=result2[section_type=="A03" & section_type_seq==20,], aes(section_type_num, velocity, fill=accel_type)) + 
  geom_bar(stat="identity") + ggtitle("1_1_A03_20")
d <- ggplot(data=result2[section_type=="A03" & section_type_seq==12,], aes(section_type_num, velocity, fill=accel_type)) + 
  geom_bar(stat="identity") + ggtitle("1_1_A03_12")
multiplot(a, b, c, d, cols=2)
dettach(result2)
#geom_line(aes(colour = accel_type))
#scale_color_manual(values = c("blue", "red", "yellow"))
#scale_color_hue()

