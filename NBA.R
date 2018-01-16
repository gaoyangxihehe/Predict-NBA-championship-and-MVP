#爬虫
#队伍数据
library(rvest)
library(stringr)
year<-c(2006:2017)
data<-data.frame()
team<-c("atl","bkn","bos","cha","chi","cle","dal","den","det",
        "gsw","hou","ind","lac","lal","mem","mia","mil","min",
        "no","nyk","okc","orl","phi","phx","por","sac","sas",
        "tor","utah","was")
tech<-c("OFFR","DEFR","SPG","BPG","PPG","APG","2P%","3P%","FT%","TPG","FPG")
for(i in year){
for(j in 1:30){
url1<-"http://www.espn.com/nba/team/stats/_/name/"
url2<-"/year/"
url3<-"/seasontype/2"
url<-paste(url1,team[j],url2,i,url3,sep="")
html<-read_html(url,encoding="UTF-8")
tables<-html %>% html_table(fill=TRUE)
tables<-as.data.frame(cbind(tables[[1]],tables[[2]]))
tables<-tables[-1,]
names(tables)<-tables[1,]
tables<-tables[-1,]
 tables<-subset(tables,tables$PLAYER=="Totals")
 tables<-tables[,names(tables) %in% tech]
# tables<-tables[length(tables),names(tables) %in% tech]
tables<-data.frame(Year=i ,TEAM =team[j] ,tables)
data<-rbind(data,tables)
}
}
#队伍加上胜率
tables<-data.frame()
for(i in year){
  url1<-"http://www.espn.com/nba/standings/_/season/"
  url2<-"/group/league"
  url<-paste(url1,i,url2,sep="")
  html<-read_html(url,encoding="UTF-8")
  tables1<-html %>% html_table(fill=TRUE)
  tables1<-as.data.frame(tables1)
  tables1<-tables1[,c(1,4)]
  names(tables1)<-c("TEAM","Y")
  tables1$TEAM<-tolower(str_extract(tables1$TEAM, "[A-Z]{1,4}$"))
  #数据有错，替换球队
  tables1$TEAM<-sub(pattern = c("gs"), replacement = c("gsw"), tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("sa"), replacement = c("sas"), tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("ny"), replacement = c("nyk"), tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("nj"), replacement = c("bkn"), tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("sasc"),replacement = c("sac"), tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("wsh"),replacement = c("was"),tables1$TEAM,fixed = TRUE)
  tables1$TEAM<-sub(pattern = c("sea"),replacement = c("okc"),tables1$TEAM,fixed = TRUE)
  tables1<-data.frame(Year=i ,tables1)
  tables<-rbind(tables,tables1)
}
data<-merge(tables,data,by=c("Year","TEAM"))
write.csv(data,"NBA_team_tech.csv")


##################################
#球员数据
library(rvest)
library(stringr)
year<-c(2007:2017)
data<-data.frame()
tech<-c("OFFR","DEFR","SPG","BPG","PPG","APG","2P%","3P%","FT%","TPG","FPG")
mvp<-read.csv("mvp.csv",header=TRUE)
year<-mvp$year
player<-as.character(mvp$player)
team<-mvp$short
n<-length(mvp$year)
for(i in 1:n){
    url1<-"http://www.espn.com/nba/team/stats/_/name/"
    url2<-"/year/"
    url3<-"/seasontype/2"
    url<-paste(url1,team[i],url2,year[i],url3,sep="")
    html<-read_html(url,encoding="UTF-8")
    tables<-html %>% html_table(fill=TRUE)
    tables<-as.data.frame(cbind(tables[[1]],tables[[2]]))
    tables<-tables[-1,]
    names(tables)<-tables[1,]
    tables<-tables[-1,]
    tables<-tables[grep(player[i],tables$PLAYER),]
    tables<-tables[,names(tables) %in% tech]
    if(identical(tables$PPG,character(0))){
      print(player[i])
      next
    }
    tables<-data.frame(Year=year[i] ,Player =player[i] ,tables)
    data<-rbind(data,tables)
}
write.csv(data,"NBA_player_tech.csv",row.names = F)

###球员数据加上得票率
data<-data.frame()
mvp$player<- str_replace_all(mvp$player,"\\?","")
for(j in 2007:2012){
  url1<-"http://www.basketball-reference.com/allstar/NBA_"
  url2<-"_voting.html"
  url<-paste(url1,j,url2,sep="")
  html<-read_html(url,encoding="UTF-8")
  tb<-html %>% html_table(fill=TRUE)
  tb<-as.data.frame(rbind(tb[[3]],tb[[4]],tb[[5]],tb[[6]],tb[[7]],tb[[8]]))
  tb<-na.omit(tb)
  tb<-tb[,2:3]
  names(tb)<-c("Player","Y")
  mvp1<-subset(mvp,mvp$year==j)
  Player<-mvp1$player
for(i in 1:length(mvp1$year)){
  tab<-tb[grep(Player[i],tb$Player),]
  if(identical(tab$Y,character(0))){
    print(Player[i])
    next
  }
  tab<-data.frame(Year=j ,tab)
  data<-rbind(data,tab)
}
}
for(j in 2013:2016){
  url1<-"http://www.basketball-reference.com/allstar/NBA_"
  url2<-"_voting.html"
  url<-paste(url1,j,url2,sep="")
  html<-read_html(url,encoding="UTF-8")
  tb<-html %>% html_table(fill=TRUE)
  tb<-as.data.frame(rbind(tb[[1]],tb[[2]],tb[[3]],tb[[4]],tb[[5]],tb[[6]]))
  tb<-na.omit(tb)
  tb<-tb[,2:3]
  names(tb)<-c("Player","Y")
  mvp1<-subset(mvp,mvp$year==j)
  Player<-mvp1$player
  for(i in 1:length(mvp1$year)){
    tab<-tb[grep(Player[i],tb$Player),]
    if(identical(tab$Y,character(0))){
      print(Player[i])
      next
    }
    tab<-data.frame(Year=j ,tab)
    data<-rbind(data,tab)
  }
}


year<-mvp$year
Player<-mvp$player
url1<-"http://www.basketball-reference.com/allstar/NBA_2017_voting-frontcourt-eastern-conference.html"
url2<-"http://www.basketball-reference.com/allstar/NBA_2017_voting-frontcourt-western-conference.html"
url3<-"http://www.basketball-reference.com/allstar/NBA_2017_voting-backcourt-eastern-conference.html"
url4<-"http://www.basketball-reference.com/allstar/NBA_2017_voting-backcourt-western-conference.html"
html1<-read_html(url1,encoding="UTF-8")
html2<-read_html(url2,encoding="UTF-8")
html3<-read_html(url3,encoding="UTF-8")
html4<-read_html(url4,encoding="UTF-8")
tb1<-html1 %>% html_table(fill=TRUE)
tb2<-html2 %>% html_table(fill=TRUE)
tb3<-html3 %>% html_table(fill=TRUE)
tb4<-html4 %>% html_table(fill=TRUE)
tb1<-as.data.frame(tb1)
tb2<-as.data.frame(tb2)
tb3<-as.data.frame(tb3)
tb4<-as.data.frame(tb4)
tb<-rbind(tb1,tb2,tb3,tb4)
tb<-tb[-1,2:3]
names(tb)<-c("Player","Y")
for(i in 259:n){
  tad<-tb[grep(Player[i],tb$Player),]
  if(identical(tad$Y,character(0))){
    print(Player[i])
    next
  }
  tad<-data.frame(Year=year[i] ,tad)
  data<-rbind(data,tad)
}
################################
play<-read.csv("NBA_player_tech.csv",header=TRUE)
day<-merge(data,play,by=c("Year","Player"))
write.csv(day,"NBA_player_tech.csv",row.names = F)#数据预处理
GT <- read.csv('TEAM.csv')
a <- cbind(GT[,5],GT[,6],GT[7],GT[,8],GT[,9],GT[,10],
           GT[,11],GT[,12],GT[,13],GT[,14],GT[,15])
a <- scale(a)
colnames(a) <-names(GT)[c(-1,-2,-3,-4)] 
team.cor <- cor(a)
GP <- read.csv('PLAYER.csv')
b <- cbind(GP[,5],GP[,6],GP[,7],GP[,8],GP[,9],GP[,10],
           GP[,11],GP[,12],GP[,13],GP[,14],GP[,15])
b <- scale(b)
colnames(b) <-names(GP)[c(-1,-2,-3,-4)] 
player.cor <- cor(b)


#判断需要提取的公因子数目
library(psych)
fa.parallel(team.cor, fa="both",main="Scree plots with parallel analysis of team")#Graph1
# Parallel analysis suggests that the number of factors =  3  and the number of components =  2 
fa.parallel(player.cor, fa="both", main="Scree plots with parallel analysis of player")#G2
#Parallel analysis suggests that the number of factors =  3  and the number of components =  2 

#提取公因子
fa_team <- fa(team.cor, nfactors=3, rotate="none", fm="pa")
fa_team
fa_player <- fa(player.cor, nfactors=3, rotate="none", fm="pa")
fa_player

#因子旋转-斜交旋转
#对于斜交旋转，因子分析会考虑三个矩阵，因子载荷矩阵、因子模式矩阵和因子关联矩阵。
library(GPArotation)
fa_t.promax <- fa(team.cor, nfactors=3, rotate="promax", fm="pa")
fa_t.promax
fa_p.promax <- fa(player.cor, nfactors=3, rotate="promax", fm="pa")
fa_p.promax
#图像显示
factor.plot(fa_t.promax, labels=rownames(fa_t.promax$loadings))#G3
factor.plot(fa_p.promax, labels=rownames(fa_p.promax$loadings))#G4
fa.diagram(fa_t.promax, simple=FALSE)#G5
fa.diagram(fa_p.promax, simple=FALSE)#G6
#因子得分


#二因子斜交旋转法计算得到因子得分权重
fa_t.promax$weights
fa_p.promax$weights
# team 
pc1 <- as.vector(nrow(a))
pc2 <- as.vector(nrow(a))
pc3 <- as.vector(nrow(a))
for(j in 1:nrow(a)){
        pc1[j] <- 0
        for(i in 1:11) pc1[j] <- pc1[j]+a[j,i]*(fa_t.promax$weights)[i,1]
} 
for(j in 1:nrow(a)){
        pc2[j] <- 0
        for(i in 1:11) pc2[j] <- pc2[j]+a[j,i]*(fa_t.promax$weights)[i,2]
} 
for(j in 1:nrow(a)){
        pc3[j] <- 0
        for(i in 1:11) pc3[j] <- pc3[j]+a[j,i]*(fa_t.promax$weights)[i,3]
}
a <- cbind(a,pc1,pc2,pc3)

pca1 <- as.vector(nrow(a))
pca2 <- as.vector(nrow(a))
pca3 <- as.vector(nrow(a))
for(j in 1:nrow(a)){
        pca1[j] <- 0
        for(i in 1:11) pca1[j] <- pca1[j]+a[j,i]*(fa_t.promax$weights)[i,1]
} 
for(j in 1:nrow(a)){
        pca2[j] <- 0
        for(i in 1:11) pca2[j] <- pca2[j]+a[j,i]*(fa_t.promax$weights)[i,2]
} 
for(j in 1:nrow(a)){
        pca3[j] <- 0
        for(i in 1:11) pca3[j] <- pca3[j]+a[j,i]*(fa_t.promax$weights)[i,3]
}
GT <- cbind(GT,pca1,pca2,pca3)
##player
pcb1 <- as.vector(nrow(b))
pcb2 <- as.vector(nrow(b))
pcb3 <- as.vector(nrow(b))
for(j in 1:nrow(b)){
        pcb1[j] <- 0
        for(i in 1:11) pcb1[j] <- pcb1[j]+b[j,i]*(fa_p.promax$weights)[i,1]
} 
for(j in 1:nrow(b)){
        pcb2[j] <- 0
        for(i in 1:11) pcb2[j] <- pcb2[j]+b[j,i]*(fa_p.promax$weights)[i,2]
} 
for(j in 1:nrow(b)){
        pcb3[j] <- 0
        for(i in 1:11) pcb3[j] <- pcb3[j]+b[j,i]*(fa_p.promax$weights)[i,3]
}
GP <- cbind(GP,pcb1,pcb2,pcb3)

###排序
order_pc1 <- rank((GT$pca1)[1:30])
for(i in 2:12) order_pc1 <- c(order_pc1,rank((GT$pca1)[((i-1)*30+1):(i*30)]))
order_pc1

order_pc2 <- rank((GT$pca2)[1:30])
for(i in 2:12) order_pc2 <- c(order_pc2,rank((GT$pca2)[((i-1)*30+1):(i*30)]))
order_pc2

order_pc3 <- rank((GT$pca3)[1:30])
for(i in 2:12) order_pc3 <- c(order_pc3,rank((GT$pca3)[((i-1)*30+1):(i*30)]))
order_pc3

GT_order <- cbind(GT,order_pc1,order_pc2,order_pc3)
#View(GT_order)

# 四只球队三大公共因子的2006-2017年走势
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
gsw <- subset(GT_order,GT_order$TEAM=='gsw',select=c(order_pc1,order_pc2,order_pc3))
plot(gsw$order_pc1,type='line',main='the pc1,2,3 of gsw',xlab='year',ylab='rank',xaxt='n')
axis(side=1,at=1:12,labels=as.character(2006:2017))
lines(gsw$order_pc2,col='red')
lines(gsw$order_pc3,col='blue')
legend('topleft',c('pc1','pc2','pc3'),col = c('black','red','blue'),lty=c(1,1,1))#G7


sas <- subset(GT_order,GT_order$TEAM=='sas',select=c(order_pc1,order_pc2,order_pc3))
plot(sas$order_pc1,type='line',main='the pc1,2,3 of sas',xlab='year',ylab='rank',xaxt='n')
axis(side=1,at=1:12,labels=as.character(2006:2017))
lines(sas$order_pc2,col='red')
lines(sas$order_pc3,col='blue')
legend('topleft',c('pc1','pc2','pc3'),col = c('black','red','blue'),lty=c(1,1,1))#G8

hou <- subset(GT_order,GT_order$TEAM=='hou',select=c(order_pc1,order_pc2,order_pc3))
plot(hou$order_pc1,type='line',main='the pc1,2,3 of hou',xlab='year',ylab='rank',xaxt='n')
axis(side=1,at=1:12,labels=as.character(2006:2017))
lines(hou$order_pc2,col='red')
lines(hou$order_pc3,col='blue')
legend('topleft',c('pc1','pc2','pc3'),col = c('black','red','blue'),lty=c(1,1,1))#G9

cle <- subset(GT_order,GT_order$TEAM=='cle',select=c(order_pc1,order_pc2,order_pc3))
plot(cle$order_pc1,type='line',main='the pc1,2,3 of cle',xlab='year',ylab='rank',xaxt='n')
axis(side=1,at=1:12,labels=as.character(2006:2017))
lines(cle$order_pc2,col='red')
lines(cle$order_pc3,col='blue')
legend('topleft',c('pc1','pc2','pc3'),col = c('black','red','blue'),lty=c(1,1,1))#Graph10

par(opar)

#2017年表现
#1.雷达图
library(fmsb)
#team
max(GT$pca1);min(GT$pca1)
max(GT$pca2);min(GT$pca2)
max(GT$pca3);min(GT$pca3)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
maxmin <- data.frame(PA1=c(5,-2),PA2=c(3,-3),PA3=c(4,-3))
dat.A <- data.frame(PA1=GT2$pca1[GT2$TEAM=='gsw'],PA2=GT2$pca2[GT2$TEAM=='gsw'],PA3=GT2$pca2[GT2$TEAM=='gsw'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of GSW')#Graph11

dat.A <- data.frame(PA1=GT2$pca1[GT2$TEAM=='sas'],PA2=GT2$pca2[GT2$TEAM=='sas'],PA3=GT2$pca2[GT2$TEAM=='sas'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of SAS')#Graph12

dat.A <- data.frame(PA1=GT2$pca1[GT2$TEAM=='hou'],PA2=GT2$pca2[GT2$TEAM=='hou'],PA3=GT2$pca2[GT2$TEAM=='hou'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of HOU')#Graph13

dat.A <- data.frame(PA1=GT2$pca1[GT2$TEAM=='cle'],PA2=GT2$pca2[GT2$TEAM=='cle'],PA3=GT2$pca2[GT2$TEAM=='cle'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of CLE')#Graph14
par(opar)

#player
max(GP2$pcb1);min(GP2$pcb1)
max(GP2$pcb2);min(GP2$pcb2)
max(GP2$pcb3);min(GP2$pcb3)
opar <- par(no.readonly=TRUE)
par(mfrow=c(4,2))
maxmin <- data.frame(PA1=c(3,-2),PA2=c(3,-2),PA3=c(3,-3))
dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='Stephen Curry'],PA2=GP2$pcb2[GP2$Player=='Stephen Curry'],PA3=GP2$pcb3[GP2$Player=='Stephen Curry'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of Stephen Curry')#G15

dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='Kevin Durant'],PA2=GP2$pcb2[GP2$Player=='Kevin Durant'],PA3=GP2$pcb3[GP2$Player=='Kevin Durant'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of Kevin Durant')#G16

dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='James Harden'],PA2=GP2$pcb2[GP2$Player=='James Harden'],PA3=GP2$pcb3[GP2$Player=='James Harden'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of James Harden')#G17


dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='Kawhi Leonard'],PA2=GP2$pcb2[GP2$Player=='Kawhi Leonard'],PA3=GP2$pcb3[GP2$Player=='Kawhi Leonard'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of Kawhi Leonard')#G18

dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='LeBron James'],PA2=GP2$pcb2[GP2$Player=='LeBron James'],PA3=GP2$pcb3[GP2$Player=='LeBron James'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of LeBron James')#G19

dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='Kyrie Irving'],PA2=GP2$pcb2[GP2$Player=='Kyrie Irving'],PA3=GP2$pcb3[GP2$Player=='Kyrie Irving'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of Kyrie Irving')#G20

dat.A <- data.frame(PA1=GP2$pcb1[GP2$Player=='Russell Westbrook'],PA2=GP2$pcb2[GP2$Player=='Russell Westbrook'],PA3=GP2$pcb3[GP2$Player=='Russell Westbrook'])
dat.A2 <- rbind(maxmin,dat.A)
radarchart(dat.A2, axistype=0, seg=5,centerzero = TRUE,title ='radarchat of Russell Westbrook')#G21
par(opar)


###逻辑回归
library(psych)
library(GPArotation)
#######基于2017年数据运用逻辑回归推算球队是否可以进季后赛
GT <- read.csv('TEAM.csv')
a <- cbind(GT[,5],GT[,6],GT[7],GT[,8],GT[,9],GT[,10],
           GT[,11],GT[,12],GT[,13],GT[,14],GT[,15])
a <- scale(a)
colnames(a) <-names(GT)[c(-1,-2,-3,-4)] 
team.cor <- cor(a)
fa_t.promax <- fa(team.cor, nfactors=3, rotate="promax", fm="pa")
pca1 <- as.vector(nrow(a))
pca2 <- as.vector(nrow(a))
pca3 <- as.vector(nrow(a))
for(j in 1:nrow(a)){
        pca1[j] <- 0
        for(i in 1:11) pca1[j] <- pca1[j]+a[j,i]*(fa_t.promax$weights)[i,1]
} 
for(j in 1:nrow(a)){
        pca2[j] <- 0
        for(i in 1:11) pca2[j] <- pca2[j]+a[j,i]*(fa_t.promax$weights)[i,2]
} 
for(j in 1:nrow(a)){
        pca3[j] <- 0
        for(i in 1:11) pca3[j] <- pca3[j]+a[j,i]*(fa_t.promax$weights)[i,3]
}
GT <- cbind(GT,pca1,pca2,pca3)
GT1 <- GT[GT$Year!=2017,]
modelteam <- glm(X~pca1+pca2+pca3,family=binomial(link='logit'),data=GT1)
summary(modelteam)
#求球队进季后赛的概率
GT2 <-GT[GT$Year==2017,]
#View(GT2)
x1a <-GT2$pca1 
x2a <-GT2$pca2 
x3a <-GT2$pca3 
p <- exp(-0.7332+ 0.5944*x1a+ 1.0167 *x2a-0.3387*x3a)/(1+exp(-0.7332+ 0.5944*x1a+ 1.0167 *x2a-0.3387*x3a))
te=levels(GT2$TEAM)
te[order(p,decreasing=TRUE)]
data.frame(team=head(te[order(p,decreasing=TRUE)],n=10),p=head(p[order(p,decreasing=TRUE)],n=10))


######基于2017年数据用逻辑回归进入全明星的球员是否可以进首发
GP <- read.csv('PLAYER.csv')
b <- cbind(GP[,5],GP[,6],GP[,7],GP[,8],GP[,9],GP[,10],
           GP[,11],GP[,12],GP[,13],GP[,14],GP[,15])
b <- scale(b)
colnames(b) <-names(GP)[c(-1,-2,-3,-4)] 
player.cor <- cor(b)
fa_p.promax <- fa(player.cor, nfactors=3, rotate="promax", fm="pa")
pcb1 <- as.vector(nrow(b))
pcb2 <- as.vector(nrow(b))
pcb3 <- as.vector(nrow(b))
for(j in 1:nrow(b)){
        pcb1[j] <- 0
        for(i in 1:11) pcb1[j] <- pcb1[j]+b[j,i]*(fa_p.promax$weights)[i,1]
} 
for(j in 1:nrow(b)){
        pcb2[j] <- 0
        for(i in 1:11) pcb2[j] <- pcb2[j]+b[j,i]*(fa_p.promax$weights)[i,2]
} 
for(j in 1:nrow(b)){
        pcb3[j] <- 0
        for(i in 1:11) pcb3[j] <- pcb3[j]+b[j,i]*(fa_p.promax$weights)[i,3]
}
GP <- cbind(GP,pcb1,pcb2,pcb3)
GP1 <- GP[GP$Year!=2017,]
modelplayer <- glm(X~pcb1+pcb2+pcb3,family=binomial(link='logit'),data=GP1)
summary(modelplayer)
#计算球员进入首发的概率
GP2 <-GP[GP$Year==2017,]
#View(GP2)
x1b <-GP2$pcb1 
x2b <-GP2$pcb2 
x3b <-GP2$pcb3 
p <- exp(-0.3616+0.5061*x1b+0.5660*x2b+0.8479*x3b)/(1+exp(-0.3616+0.5061*x1b+0.5660*x2b+0.8479*x3b))
pl=levels(GP2$Player)
pl[order(p,decreasing=TRUE)]
data.frame(player=head(GP2$Player[order(p,decreasing=TRUE)],n=10),p=head(p[order(p,decreasing=TRUE)],n=10))


##########################################################################################################
#运用2017年数据计算球队获得总冠军的概率
ZGT <- read.csv('TEAM1.csv')
aZ <- cbind(ZGT[,5],ZGT[,6],ZGT[7],ZGT[,8],ZGT[,9],ZGT[,10],
           ZGT[,11],ZGT[,12],ZGT[,13],ZGT[,14],ZGT[,15])
aZ <- scale(aZ)
colnames(aZ) <-names(ZGT)[c(-1,-2,-3,-4)] 
Zteam.cor <- cor(aZ)
fa_t.promax <- fa(Zteam.cor, nfactors=3, rotate="promax", fm="pa")
pcaZ1 <- as.vector(nrow(aZ))
pcaZ2 <- as.vector(nrow(aZ))
pcaZ3 <- as.vector(nrow(aZ))
for(j in 1:nrow(aZ)){
        pcaZ1[j] <- 0
        for(i in 1:11) pcaZ1[j] <- pcaZ1[j]+aZ[j,i]*(fa_t.promax$weights)[i,1]
} 
for(j in 1:nrow(aZ)){
        pcaZ2[j] <- 0
        for(i in 1:11) pcaZ2[j] <- pcaZ2[j]+aZ[j,i]*(fa_t.promax$weights)[i,2]
} 
for(j in 1:nrow(aZ)){
        pcaZ3[j] <- 0
        for(i in 1:11) pcaZ3[j] <- pcaZ3[j]+aZ[j,i]*(fa_t.promax$weights)[i,3]
}
ZGT <- cbind(ZGT,pcaZ1,pcaZ2,pcaZ3)
ZGT1 <- ZGT[ZGT$Year!=2017,]
modelteam <- glm(X~pcaZ1+pcaZ2,family=binomial(link='logit'),data=ZGT1)
summary(modelteam)
#计算概率
ZGT2 <- ZGT[ZGT$Year==2017,]
#View(ZGT2)
x1aZ <-ZGT2$pcaZ1 
x2aZ <-ZGT2$pcaZ2 
x3aZ <-ZGT2$pcaZ3 

p <- exp(-3.7468 + 0.5757*x1aZ+  0.8130  *x2aZ)/(1+exp(-3.7468 + 0.5757*x1aZ+  0.8130  *x2aZ))
te=levels(ZGT2$TEAM)
te[order(p,decreasing=TRUE)]
data.frame(team=head(te[order(p,decreasing=TRUE)],n=10),p=head(p[order(p,decreasing=TRUE)],n=10))

###基于2017年数据估算全明星球员获得MVP的概率
ZGP <- read.csv('PLAYER1.csv')
bZ <- cbind(ZGP[,5],ZGP[,6],ZGP[7],ZGP[,8],ZGP[,9],ZGP[,10],
            ZGP[,11],ZGP[,12],ZGP[,13],ZGP[,14],ZGP[,15])
bZ <- scale(bZ)
colnames(bZ) <-names(ZGP)[c(-1,-2,-3,-4)] 
Zplayer.cor <- cor(bZ)
fa_t.promax <- fa(Zplayer.cor, nfactors=3, rotate="promax", fm="pa")
pcbZ1 <- as.vector(nrow(bZ))
pcbZ2 <- as.vector(nrow(bZ))
pcbZ3 <- as.vector(nrow(bZ))
for(j in 1:nrow(bZ)){
        pcbZ1[j] <- 0
        for(i in 1:11) pcbZ1[j] <- pcbZ1[j]+bZ[j,i]*(fa_t.promax$weights)[i,1]
} 
for(j in 1:nrow(bZ)){
        pcbZ2[j] <- 0
        for(i in 1:11) pcbZ2[j] <- pcbZ2[j]+bZ[j,i]*(fa_t.promax$weights)[i,2]
} 
for(j in 1:nrow(bZ)){
        pcbZ3[j] <- 0
        for(i in 1:11) pcbZ3[j] <- pcbZ3[j]+bZ[j,i]*(fa_t.promax$weights)[i,3]
}
ZGP <- cbind(ZGP,pcbZ1,pcbZ2,pcbZ3)
ZGP1 <- ZGP[ZGP$Year!=2017,]
modelplayer2 <- glm(X~pcbZ2+pcbZ3,family=binomial(link='logit'),data=ZGP1)
summary(modelplayer2)
#计算概率
ZGP2 <- ZGP[ZGP$Year==2017,]
#View(ZGP2)
x1bZ <-ZGP2$pcbZ1 
x2bZ <-ZGP2$pcbZ2 
x3bZ <-ZGP2$pcbZ3 
p <- exp(-4.4294+1.0528 *x2bZ+1.8728*x3bZ)/(1+exp(-4.4294+1.0528 *x2bZ+1.8728*x3bZ))
pl <- ZGP2$Player
pl[order(p,decreasing=TRUE)]
data.frame(player=head(pl[order(p,decreasing=TRUE)],n=10),p=head(p[order(p,decreasing=TRUE)],n=10))

