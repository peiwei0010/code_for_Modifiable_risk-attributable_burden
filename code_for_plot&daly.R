setwd("/cancer")
#*********************************Transformation of plot data**************************************************#
library(carData)
library(car)
library(dplyr)
library(circlize)
library(grid)
library(ComplexHeatmap)
library(reshape2)

#input
overallpaf1 <- read.csv("overallpaf1.csv")
pafallfemale <- t(read.csv("pafallfemale.csv", row.names=1))
pafallmale <- t(read.csv("pafallmale.csv", row.names=1))
pafallfemale<-as.data.frame(cbind(pafallfemale,rownames(pafallfemale)))
pafallmale<-as.data.frame(cbind(pafallmale,rownames(pafallmale)))
#修改名字以备合并
pafallfemale$group<-car::recode(pafallfemale$V10,"'paf_lung_all'='Lung cancer combined';'paf_bre_all'='Breast cancer combined';'paf_cer_all'='Cervical cancer combined';'paf_leu_all'='Leukemia combined';'paf_pan_all'='Pancreatic cancer combined'; 'paf_liv_all'='Liver cancer combined';'paf_gas_all'='Gastric cancer combined';'paf_crc_all'='Colorectal cancer combined';'paf_eso_all'='Esophageal cancer combined';'paf_pro_all'='Prostate cancer combined';'paf_lym_all'='Lymphoma combined'; 'paf_ova_all'='Ovarian cancer combined'")
pafallmale$group<-car::recode(pafallmale$V10,"'paf_lung_all'='Lung cancer combined';'paf_bre_all'='Breast cancer combined';'paf_cer_all'='Cervical cancer combined';'paf_leu_all'='Leukemia combined';'paf_pan_all'='Pancreatic cancer combined'; 'paf_liv_all'='Liver cancer combined';'paf_gas_all'='Gastric cancer combined';'paf_crc_all'='Colorectal cancer combined';'paf_eso_all'='Esophageal cancer combined';'paf_pro_all'='Prostate cancer combined';'paf_lym_all'='Lymphoma combined'; 'paf_ova_all'='Ovarian cancer combined'")

colnames(pafallfemale)[10]<-"NAME_"
colnames(pafallmale)[10]<-"NAME_"
pafallfemale$X_NAME_<-car::recode(pafallfemale$NAME_,"'paf_lung_all'='yearpaf_lungfemale';'paf_bre_all'='yearpaf_brefemale';'paf_cer_all'='yearpaf_cerfemale';'paf_leu_all'='yearpaf_leufemale';'paf_pan_all'='yearpaf_panfemale'; 'paf_liv_all'='yearpaf_livfemale';'paf_gas_all'='yearpaf_gasfemale';'paf_crc_all'='yearpaf_crcfemale';'paf_eso_all'='yearpaf_esofemale';'paf_pro_all'='yearpaf_profemale';'paf_lym_all'='yearpaf_lymfemale'; 'paf_ova_all'='yearpaf_ovafemale'")
pafallmale$X_NAME_<-car::recode(pafallmale$NAME_,"'paf_lung_all'='yearpaf_lungmale';'paf_bre_all'='yearpaf_bremale';'paf_cer_all'='yearpaf_cermale';'paf_leu_all'='yearpaf_leumale';'paf_pan_all'='yearpaf_panmale'; 'paf_liv_all'='yearpaf_livmale';'paf_gas_all'='yearpaf_gasmale';'paf_crc_all'='yearpaf_crcmale';'paf_eso_all'='yearpaf_esomale';'paf_pro_all'='yearpaf_promale';'paf_lym_all'='yearpaf_lymmale'; 'paf_ova_all'='yearpaf_ovamale'")
colnames(overallpaf1)<-c("group","X_NAME_","2012","2015","2019","2021","2024","2026","2030","2032","2035")

#combine
overallpaf2<-rbind(overallpaf1[,c(1:11)],pafallfemale[,c(11,12,1:9)])
overallpaf3<-rbind(overallpaf2,pafallmale[,c(11,12,1:9)])
overallpaf3<-as.matrix(overallpaf3)
overallpaf3<-as.data.frame(overallpaf3)
overallpaf3[c(3:11)]<-lapply(overallpaf3[c(3:11)],as.numeric)
overallpaf3[overallpaf3=="Ever smoking"]<-"Smoking"
overallpaf3[overallpaf3=="Alcohol consumption"]<-"Drinking"
#sort
s<-split(overallpaf3,overallpaf3$X_NAME_)
for (i in 1:24) {
  s[[i]]<-s[[i]][order(-s[[i]][,3]),]
}
#gen sort var 
for (i in 1:24) {
  s[[i]][,12]<-rank(-s[[i]][,3])
}
for (i in 1:24) {
  s[[i]][,13]<-rank(-s[[i]][,11])
}


#combine
male<-c(16,14,10,8,6,22,12,18,24)
female<-c(3,15,13,9,1,5,7,21,17,11,19)
#create data frame with 0 rows and 11 columns
df.female <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(df.female) <- c("group","X_NAME_","2012","2015","2019","2021","2024","2026","2030","2032","2035")

df.male <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(df.male) <- c("group","X_NAME_","2012","2015","2019","2021","2024","2026","2030","2032","2035")
a=1
for (i in female) {
  df.female<-rbind(df.female,cbind(s[[i]],rep(a,nrow(s[[i]]))))
  a=a+1
}
a=1
for (i in male) {
  
  df.male<-rbind(df.male,cbind(s[[i]],rep(a,nrow(s[[i]]))))
  a=a+1
}
#data for hotplot
df.female$V12<-df.female$V12-1
df.female$V13<-df.female$V13-1
df.male$V12<-df.male$V12-1
df.male$V13<-df.male$V13-1
df.female[df.female==0]<-NA
df.male[df.male==0]<-NA
#**********************************************************plot*******************************************#
  df.male[,c(3:11)]<-round(df.male[,c(3:11)],1)
data1 <- as.matrix(df.male[,c(3:11)])
rownames(data1)<-df.male[,1]

col_fun <- colorRamp2(
  c(0,1,5,10,20,50),c("skyblue3","lightblue","white", "lightpink","pink","red"))
column_ha <- HeatmapAnnotation(
  foo1 = runif(10),
  bar1 = anno_barplot(runif(10))
)
#
split <- df.male[,14]
dev.new()

P1<-Heatmap(data1,
            column_title = "",
            name = "PAF(%)",
            column_title_side = "top",
            column_title_rot = 0,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            rect_gp = gpar(col = "black"),#小方块颜色
            col= col_fun,
            
            row_names_side = "left",
            
            row_names_centered =FALSE,
            
            row_names_rot = 0,
          
            row_names_gp = gpar(fontsize = 5),
            row_labels =rownames(data1),
            column_names_side="top",
            column_names_gp = gpar(fontsize =5),
            column_names_rot=45,
            border = TRUE, 
            width = unit(8, "cm"),
            height = unit(15, "cm"),
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%.1f", data1[i, j]), x, y, gp = gpar(fontsize =4))
            },
            row_split = split,row_title=c('', '', '', '', '', '', '', '', ''),
            row_title_gp=gpar(fontsize = 5)
)


P1
rankdata <- as.matrix(df.male[,12])
colnames(rankdata)<-c("RANK 2012")
col_fun <- colorRamp2(
  c(0,5,10),
  c("forestgreen","greenyellow","white"))
col_fun(seq(-1, 15))

P2<-Heatmap(rankdata,
            column_title = "",#title
            name = "Leading risk",
            column_title_side = "top",
            column_title_rot = 0,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            rect_gp = gpar(col = "black"),#coler
            col= col_fun,
            # The row labels are placed on the left
            row_names_side = "left",
            # Let the row tree go to the right
            row_names_centered =FALSE,
            # Set selection Angle
            row_names_rot = 0,
            # Set the line label font size
            row_names_gp = gpar(fontsize = 5),
            column_names_side="top",
            column_names_gp = gpar(fontsize =5),
            column_names_rot=45,
            border = TRUE, # Set the border
            width = unit(0.5, "cm"),
            height = unit(15, "cm"),
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%1.f", rankdata[i, j]), x, y, gp = gpar(fontsize =4))
            },na_col = "white",
            row_split = split, row_title=c('', '', '', '', '', '', '', '', ''), row_title_gp=gpar(fontsize = 5),#Heat map segmentation
)
P2
rankdata1 <-as.matrix( df.male$V13)
colnames(rankdata1)<-c("RANK 2035")
col_fun <- colorRamp2(
  c(0,5,10),
  c("forestgreen","greenyellow","white"))
col_fun(seq(-1, 15))
P3<-Heatmap(rankdata1,
            column_title = "",
            name = "Leading risk",
            column_title_side = "top",
            column_title_rot = 0,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            rect_gp = gpar(col = "black"),
            col= col_fun,
            
            row_names_side = "left",
            
            row_names_centered =FALSE,
          
            row_names_rot = 45,
            
            row_names_gp = gpar(fontsize = 5),
            column_names_side="top",
            column_names_gp = gpar(fontsize =5),
            column_names_rot=45,
            border = TRUE, 
            width = unit(0.5, "cm"),
            height = unit(15, "cm"),
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%1.f", rankdata1[i, j]), x, y, gp = gpar(fontsize =4))
            },na_col = "white",
            row_split = split, row_title=c('', '', '', '', '', '', '', '', ''), row_title_gp=gpar(fontsize = 5),
)
P3
P1+P2+P3
pdf('male.pdf',width =7,height =7)
draw(P1+P2+P3, newpage = FALSE, column_title = "",
     column_title_gp = gpar(fontsize = 8, fontface = "bold"), heatmap_legend_side = "right")
dev.off()
#**********************************************************YLD&YLL*******************************************#

#import:data from:Population,prevalence mortality data"
#reshape Demographic data were grouped into five years of age and the end point was 85+
un_male_pop[,24]<-rowSums(un_male_pop[,c(19,20,21,22)])
un_female_pop[,24]<-rowSums(un_female_pop[,c(19,20,21,22)])
#The number of deaths or illnesses was calculated for age groups
#**_pop:population
#**_pre:prevalence (from GBD)
#**_mort: mortality
#combine
male_YLDdata<-merge(male_pre,un_male_pop,by="year")
female_YLDdata<-merge(female_pre,un_female_pop,by="year")
male_YLLdata<-merge(male_mort,un_male_pop,by="year")
female_YLLdata<-merge(female_mort,un_female_pop,by="year")
#Number of persons counted
#YLL
male_YLLcase<-cbind(male_YLLdata[,c(1:3,40)],0.01*male_YLLdata[,c(4:21)]*male_YLLdata[,c(22:39)])
female_YLLcase<-cbind(female_YLLdata[,c(1:3,40)],0.01*female_YLLdata[,c(4:21)]*female_YLLdata[,c(22:39)])
#YLD
male_YLDcase<-cbind(male_YLDdata[,c(1:3,40)],0.01*male_YLDdata[,c(4:21)]*male_YLDdata[,c(22:39)])
female_YLDcase<-cbind(female_YLDdata[,c(1:3,40)],0.01*female_YLDdata[,c(4:21)]*female_YLDdata[,c(22:39)])
#calculate YLL
#Lifetable
life<-c(87.07,82.58,77.58,72.60,67.62,62.66,57.71,52.76,47.83,42.94,38.1,33.33,28.66,24.12,19.76,15.65,11.96,7.05)
lifetable<-matrix(rep(life,each=156),nrow=156,ncol=18)
lifetable_2<-matrix(rep(life,each=180),nrow=180,ncol=18)
male_YLLcase[,23]<-rowSums(male_YLLcase[,c(5:22)]*lifetable)
female_YLLcase[,23]<-rowSums(female_YLLcase[,c(5:22)]*lifetable_2)
#calculate YLD
sequela_cancer_index<-sequela_cancer_index[!duplicated(sequela_cancer_index),]
#Sequelae AND WEIGHTS
male_YLDcase_1<-merge(male_YLDcase,sequela_cancer_index,by="sequela_id")
female_YLDcase_1<-merge(female_YLDcase,sequela_cancer_index,by="sequela_id")
#
male_YLDcase_1[,26]<-male_YLDcase_1[,24]*rowSums(male_YLDcase_1[,c(5:22)])
female_YLDcase_1[,26]<-female_YLDcase_1[,24]*rowSums(female_YLDcase_1[,c(5:22)])
male_YLD_2<-aggregate(male_YLDcase_1[,26],by=list(male_YLDcase_1$cancer,male_YLDcase_1$year),sum)
female_YLD_2<-aggregate(female_YLDcase_1[,26],by=list(female_YLDcase_1$cancer,female_YLDcase_1$year),sum)
#output
write.csv(male_YLD_2,file = "male_YLD.csv")
write.csv(male_YLLcase,file="male_YLL.csv")
write.csv(female_YLD_2,file = "female_YLD.csv")
write.csv(female_YLLcase,file="female_YLL.csv")

