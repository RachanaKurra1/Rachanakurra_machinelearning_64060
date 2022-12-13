---
title: "FML final project"
author: "Rachana"
date: "2022-12-04"
output:
  pdf_document: default
  word_document: default
  html_document: default
---




```r
library(dplyr)
library(ISLR)
library(tidyverse)
library(NbClust)
library(factoextra)
```

*loading the file*

```r
rawdata=read.csv("C:/Users/kurra/Downloads/fuel_receipts_costs_eia923 (3).csv")
```
*replacing empty values with 0*

```r
data1 =  rawdata                           # Duplicate data frame
data1[data1 == ""] <- 0                    # Replace blank by 0
View(data1)                                # Print updated data frame
```
*omitting NA values*

```r
data2= na.omit(data1)
```

*considering 12000 data as a sample* 

```r
set.seed(1234)
data3= sample_n(data2, 12000)
view(data3)
```



*splitting the data into traning and test data*

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
set.seed(3456)
Split_data= createDataPartition(data3$rowid, p = .75, list = FALSE,times = 1) 
                                  
train_data=data3[Split_data,]
test_data=data3[-Split_data,]

View(train_data)
View(test_data)
```

*looking at the data distribution*

```r
library(corrplot)
```

```
## corrplot 0.92 loaded
```

```r
boxplot(train_data[, c(12,13,15,16,17,18,19,20,27,28)])
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
#some variables had big ranges and outliers

corrplot(cor(train_data[, c(12,13,15,16,17,18,19,20,27,28)]), method= "shade")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)

```r
#plotting correlation between different variables, sulfur content and heat produced shows a showing positive correlation relation and fuel received and heat produced shows negative correlation.
```

* Building the clustering model*

```r
#1- Use Kmeans clustering to identify clusters  

#Normalizing variables related to purchases process  using z-score
ncol(Split_data)
```

```
## [1] 1
```

```r
fuel_cost_normalized =scale(train_data[, c(12,13,15,16,17,18,19,20,27,28)])
head(fuel_cost_normalized)
```

```
##    mine_id_pudl mine_id_pudl_label fuel_received_units fuel_mmbtu_per_unit sulfur_content_pct ash_content_pct
## 2    -0.7950106         -0.7950106         -0.12490814          -0.8577856         -0.7609267      -0.6760496
## 3    -0.6688621         -0.6688621         -0.23050958           0.4666931          1.5269385       0.2009245
## 4     0.5062120          0.5062120          2.23237205           1.0616200          1.9481700       0.4135243
## 5    -0.7956414         -0.7956414         -0.68252118          -0.9855359         -0.8435211      -0.8886494
## 6    -0.7943799         -0.7943799         -0.51817634          -0.7689905         -0.8352617      -0.5963247
## 10   -0.6688621         -0.6688621          0.07892807           0.4391952          1.5351980       0.1743495
##    mercury_content_ppm fuel_cost_per_mmbtu moisture_content_pct chlorine_content_ppm
## 2           -0.4168514         -0.54895324            1.0019118           -0.1816172
## 3           -0.4168514         -0.33032647           -0.2794774           -0.1816172
## 4           -0.4168514          0.04480362           -0.8188595           -0.1816172
## 5            1.3054379         -0.83328752            1.1246283           -0.1816172
## 6           -0.4168514          0.10334303            0.7992868           -0.1816172
## 10          -0.4168514         -0.16187633           -0.2547438           -0.1816172
```

```r
View(fuel_cost_normalized)

fuel_cost_normalized=as.data.frame(fuel_cost_normalized)

#Finding the optimal k number using both Elbow method and Silouhette

fviz_nbclust(fuel_cost_normalized, kmeans, method = "wss") +
geom_vline(xintercept = 3, linetype = 2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

wss_kmeans= kmeans(fuel_cost_normalized, centers= 2, nstart= 25)
View(wss_kmeans)
wss_kmeans$size # custer 1 is large size.
wss_kmeans$withinss#cluster 2 has least within cluster sum of squares 


#plotting clusters
fviz_cluster(wss_kmeans, data = fuel_cost_normalized )


#silhouoette method

fviz_nbclust(fuel_cost_normalized, kmeans, method = "silhouette")

silhouette_kmeans=kmeans(fuel_cost_normalized,centers=2,nstart=25)
silhouette_kmeans
fviz_cluster( silhouette_kmeans, data = fuel_cost_normalized)
```


```r
#Running cluster centroids to better understand the characteristics of each cluster
#df = as.data.frame(t(wss_kmeans$centers)) %>% rename(Cluster1 = 1, Cluster2 =2)
#View(df)
#df1 = as.data.frame(t(silhouette_kmeans$centers)) %>% rename(Cluster1 = 1, Cluster2 =2)
#View(df1) 
```



#Summary: Cluster 1  is the largest cluster with size of 4357 , has the  highest amount of fuel received units and moisture content. Cluster 2 has the highest amount of heat produced with less fuel received units, and has 2 nd highest amount of ash and 1 st highest sulfur content   To summarize, Cluster 1 is better in terms of ecologically and economically as it uses less ash and sulfur content with least fuel cost per heat produced and also cluster 2 is better in terms of heat produced.




#library(cluster)
#clusplot(fuel_cost_normalized, wss_kmeans$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
```

**Binding the cluster assignment to the original data frame for analysis,creating dataframe to combine clusters membership with original data including categorical variables to better understand clusters characteristics**
3```{r}
clusters_wss <- wss_kmeans$cluster
clusters_silhouette <- silhouette_kmeans$cluster
fuel_model1 <- cbind(train_data,clusters_wss)
fuel_model2 <- cbind(train_data,clusters_silhouette)
View(fuel_model1)
fuel_model1_new=fuel_model1[,c(12,13,15,16,17,18,19,20,27,28,31)]
fuel_model1_new
```
install.packages("operators")

library(operators)
library(magrittr)




```r
#mean value of the clusters to know the distribution
mean_data=(fuel_model1_new) %>% group_by(clusters_wss) %>% summarise(mean)
```

```
## Error in UseMethod("summarise"): no applicable method for 'summarise' applied to an object of class "function"
```

```r
View(mean_data)
```

*Aggregating the clusters to interpret the attributes - WSS*

```r
int_wss <- aggregate(fuel_model1[,c(12,13,15,16,17,18,19,20,27,28,31)],by=list(fuel_model1$clusters_wss),FUN="median")
print(int_wss[,-1])
```

```
##   mine_id_pudl mine_id_pudl_label fuel_received_units fuel_mmbtu_per_unit sulfur_content_pct ash_content_pct
## 1           21                 21               45591             17.6000               0.26             5.0
## 2         2461               2461               20366             23.7865               2.49             9.1
##   mercury_content_ppm fuel_cost_per_mmbtu moisture_content_pct chlorine_content_ppm clusters_wss
## 1                   0               1.920                 27.2                    0            1
## 2                   0               2.445                  7.9                    0            2
```
*plotting the clusters for different *
    

#ggplot(data =fuel_model1, aes(x = sulfur_content_pct, y = fuel_cost_per_mmbtu)) +geom_point(colour="red")    
    

#ggplot(data = train_data, aes(x = sulfur_content_pct, y = fuel_cost_per_mmbtu)) geom_point(aes(colour="pink"))    
    
#ggplot(data = train_data, aes(x = moisture_content_pct, y = fuel_cost_per_mmbtu)) +geom_point(colour="blue")
    
#ggplot(fuel_model1,aes(x=fuel_cost_per_mmbtu,y= fuel_received_units)) + geom_bar(stat="identity")



*tells that as fuel cost is not much affected by sulfur content *

library(ggplot2)

ggplot(fuel_model1) +
 aes(x = sulfur_content_pct, y = fuel_cost_per_mmbtu) +
 geom_point(shape = "circle", 
 size = 1.5, colour = "#112446") +
 theme_minimal()

 

*similarly  ash content has least effect on fuel price,as ash content increases fuel has less variance.*

library(ggplot2)

ggplot(fuel_model1) +
 aes(x = fuel_cost_per_mmbtu, y = ash_content_pct) +
 geom_point(shape = "circle", 
 size = 1.5, colour = "#112446") +
 theme_minimal()

#here we can observe that most of fuel type used from clusters is coal

library(ggplot2)

ggplot(fuel_model1) +
 aes(x = clusters_wss, fill = fuel_group_code) +
 geom_histogram(bins = 16L) +
 scale_fill_hue(direction = 1) +
 theme_minimal()



#from this we can conclude that fuel cost is most affected in cluster 3  and cluster 1 where the fuel cost is least affected in cluster 2 it uses chlorine content  
library(ggplot2)

ggplot(fuel_model1) +
 aes(x = fuel_cost_per_mmbtu, y = clusters_wss) +
 geom_point(shape = "circle", size = 1.5, 
 colour = "#112446") +
 theme_minimal()






#using multiple linear regression for predidcting data

```r
library(dplyr)
library(caret)
fuel_cost_normalized_testdata =scale(test_data[, c(12,13,15,16,17,18,19,20,27,28)])
head(fuel_cost_normalized_testdata)
```

```
##    mine_id_pudl mine_id_pudl_label fuel_received_units fuel_mmbtu_per_unit sulfur_content_pct ash_content_pct
## 1    -0.8127306         -0.8127306           0.6146180          -0.8998426         -0.8812028     -0.88788993
## 7     0.5325952          0.5325952           0.1698876           0.8677333          1.1724519     -0.05403387
## 8    -0.7452770         -0.7452770          -0.3182440          -1.0471406         -0.8066750     -0.69168850
## 9    -0.7452770         -0.7452770          -0.1803898          -1.1205007         -0.8149559     -0.61811297
## 19   -0.8114815         -0.8114815           0.3249425          -0.9287245         -0.8315176     -0.64263815
## 22   -0.8008638         -0.8008638           2.9602995          -1.2065690         -0.7238664     -0.81431439
##    mercury_content_ppm fuel_cost_per_mmbtu moisture_content_pct chlorine_content_ppm
## 1           -0.4313577         -0.43811004            1.0499901           -0.1917533
## 7           -0.4313577         -1.28201472           -0.5648093           -0.1917533
## 8           -0.4313577         -0.66352698            1.2135872           -0.1917533
## 9            2.4656045          0.24800278            1.2540053           -0.1917533
## 19          -0.4313577          0.14938287            0.9431709           -0.1917533
## 22          -0.4313577          0.09866406            1.3252182           -0.1917533
```

```r
View(fuel_cost_normalized_testdata)
#Developing linear  regression  model using train data
model= lm(fuel_cost_per_mmbtu~fuel_cost_normalized_testdata , data=test_data)

summary(model)
```

```
## Warning in summary.lm(model): essentially perfect fit: summary may be unreliable
```

```
## 
## Call:
## lm(formula = fuel_cost_per_mmbtu ~ fuel_cost_normalized_testdata, 
##     data = test_data)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -4.477e-14 -7.700e-17  2.000e-18  5.800e-17  8.205e-14 
## 
## Coefficients: (1 not defined because of singularities)
##                                                     Estimate Std. Error    t value Pr(>|t|)    
## (Intercept)                                        2.261e+00  3.302e-17  6.848e+16  < 2e-16 ***
## fuel_cost_normalized_testdatamine_id_pudl         -9.210e-17  4.459e-17 -2.066e+00   0.0389 *  
## fuel_cost_normalized_testdatamine_id_pudl_label           NA         NA         NA       NA    
## fuel_cost_normalized_testdatafuel_received_units   1.729e-18  3.455e-17  5.000e-02   0.9601    
## fuel_cost_normalized_testdatafuel_mmbtu_per_unit   1.003e-15  7.642e-17  1.312e+01  < 2e-16 ***
## fuel_cost_normalized_testdatasulfur_content_pct   -4.132e-16  4.642e-17 -8.901e+00  < 2e-16 ***
## fuel_cost_normalized_testdataash_content_pct      -8.053e-17  4.371e-17 -1.842e+00   0.0655 .  
## fuel_cost_normalized_testdatamercury_content_ppm   2.217e-16  3.700e-17  5.991e+00 2.33e-09 ***
## fuel_cost_normalized_testdatafuel_cost_per_mmbtu   7.098e-01  4.048e-17  1.753e+16  < 2e-16 ***
## fuel_cost_normalized_testdatamoisture_content_pct  4.323e-18  6.982e-17  6.200e-02   0.9506    
## fuel_cost_normalized_testdatachlorine_content_ppm -1.798e-17  3.719e-17 -4.840e-01   0.6288    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.808e-15 on 2990 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:      1 
## F-statistic: 5.133e+31 on 9 and 2990 DF,  p-value: < 2.2e-16
```

```r
#Eliminating all the columns with p value greater than 5%( NULL hypothesis).Dependent variables with p value less than 5% has significance in predicting value for the predicted variable(here fuel_cost_per_mmbtu).
new_data=fuel_cost_normalized_testdata[,c(1,4,5,7,8)]
model1= lm(fuel_cost_per_mmbtu~new_data , data=test_data)
summary(model1)
```

```
## Warning in summary.lm(model1): essentially perfect fit: summary may be unreliable
```

```
## 
## Call:
## lm(formula = fuel_cost_per_mmbtu ~ new_data, data = test_data)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -4.497e-14 -6.700e-17 -1.500e-17  4.100e-17  1.006e-13 
## 
## Coefficients:
##                               Estimate Std. Error    t value Pr(>|t|)    
## (Intercept)                  2.261e+00  3.723e-17  6.073e+16  < 2e-16 ***
## new_datamine_id_pudl        -2.882e-16  4.672e-17 -6.170e+00 7.75e-10 ***
## new_datafuel_mmbtu_per_unit -1.428e-15  6.092e-17 -2.345e+01  < 2e-16 ***
## new_datasulfur_content_pct   4.691e-16  5.128e-17  9.147e+00  < 2e-16 ***
## new_datamercury_content_ppm -1.395e-16  3.778e-17 -3.693e+00 0.000226 ***
## new_datafuel_cost_per_mmbtu  7.098e-01  4.448e-17  1.596e+16  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.039e-15 on 2994 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:      1 
## F-statistic: 7.267e+31 on 5 and 2994 DF,  p-value: < 2.2e-16
```

```r
#clusters for test data
fviz_nbclust(fuel_cost_normalized_testdata, kmeans, method = "wss") +
geom_vline(xintercept = 3, linetype = 2)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
wss_kmeans_testdata= kmeans(fuel_cost_normalized_testdata, centers= 2, nstart= 25)
View(wss_kmeans_testdata)
wss_kmeans_testdata$size # custer 1 has high
```

```
## [1] 1426 1574
```

```r
wss_kmeans_testdata$withinss#cluster 2 has least within cluster sum of squares
```

```
## [1]  6019.101 12893.421
```

```r
fviz_nbclust(fuel_cost_normalized_testdata, kmeans, method = "silhouette")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-2.png)

```r
silhouette_kmeans=kmeans(fuel_cost_normalized_testdata,centers=2,nstart=25)
silhouette_kmeans
```

```
## K-means clustering with 2 clusters of sizes 1426, 1574
## 
## Cluster means:
##   mine_id_pudl mine_id_pudl_label fuel_received_units fuel_mmbtu_per_unit sulfur_content_pct ash_content_pct
## 1   -0.7429850         -0.7429850           0.2884592          -0.9252107         -0.7819428      -0.6222200
## 2    0.6731236          0.6731236          -0.2613360           0.8382150          0.7084183       0.5637139
##   mercury_content_ppm fuel_cost_per_mmbtu moisture_content_pct chlorine_content_ppm
## 1           0.1468565          -0.4921554            0.8574555           -0.1866602
## 2          -0.1330479           0.4458790           -0.7768307            0.1691090
## 
## Clustering vector:
##    1    7    8    9   19   22   25   27   31   35   37   42   46   50   52   56   58   61   62   63   65   68 
##    1    2    1    1    1    1    1    2    1    1    1    2    1    1    2    2    1    1    2    1    2    1 
##   71   75   76   77   82   88   90   92   93   97  100  103  109  111  114  116  127  128  131  134  136  138 
##    1    1    1    2    1    1    2    2    2    1    1    1    2    1    1    1    2    2    2    2    1    1 
##  141  147  152  153  157  160  164  166  168  170  173  175  179  184  189  190  192  198  205  206  207  213 
##    1    2    2    2    1    2    2    1    1    2    1    2    1    1    1    2    1    1    1    1    1    1 
##  215  216  223  229  231  232  234  235  243  248  249  254  255  258  260  261  265  266  267  270  271  273 
##    1    2    2    2    1    2    2    2    1    2    1    1    1    2    1    2    2    1    1    2    1    2 
##  274  277  278  284  292  293  294  298  299  302  304  309  311  316  317  319  326  331  334  337  339  344 
##    2    2    2    2    1    1    2    2    2    2    1    1    1    2    1    2    2    1    2    2    1    1 
##  347  349  350  353  359  363  364  365  368  371  372  374  377  381  384  386  387  388  391  392  399  412 
##    1    1    1    2    2    2    2    1    1    2    2    1    1    2    1    1    1    2    2    2    2    1 
##  414  422  423  425  429  439  440  441  452  456  460  461  463  464  468  469  470  471  475  485  490  492 
##    1    1    1    1    1    2    2    2    1    1    1    1    2    2    2    2    2    1    1    2    1    2 
##  511  514  515  521  523  533  534  538  548  559  560  561  562  567  571  574  575  579  583  589  590  595 
##    2    1    1    2    2    1    2    2    2    2    2    2    2    1    1    2    2    2    2    1    1    1 
##  598  603  606  609  616  625  626  629  631  641  644  650  653  656  664  666  668  679  682  683  684  689 
##    2    1    2    1    1    1    1    2    1    1    2    1    1    2    1    1    1    1    2    2    2    2 
##  690  691  692  693  694  696  700  709  713  726  728  734  740  756  764  767  768  772  783  787  788  792 
##    1    2    1    2    1    2    1    2    2    2    2    2    2    1    1    2    1    2    2    1    2    2 
##  793  797  800  801  805  813  814  817  822  823  829  833  835  837  838  843  849  851  853  855  861  867 
##    2    2    1    2    2    2    2    2    1    1    1    1    1    2    2    2    1    2    2    1    2    1 
##  876  881  884  885  887  888  898  903  914  915  918  919  920  921  927  929  930  940  941  942  945  961 
##    2    2    2    2    2    2    1    2    2    1    2    2    2    1    2    2    1    1    2    2    2    2 
##  962  978  982  989  990  991  995  996 1006 1010 1014 1025 1031 1032 1033 1036 1043 1047 1049 1050 1059 1069 
##    1    2    2    2    2    1    2    2    1    1    2    2    2    2    2    1    2    2    1    1    1    1 
## 1074 1081 1085 1088 1093 1096 1098 1099 1103 1108 1116 1117 1122 1124 1132 1134 1146 1149 1161 1163 1168 1171 
##    2    1    1    1    1    1    1    1    2    2    1    1    2    2    2    1    2    2    2    2    1    2 
## 1173 1176 1180 1181 1182 1192 1193 1195 1213 1214 1216 1224 1235 1240 1241 1242 1250 1252 1257 1258 1269 1276 
##    1    1    2    1    1    2    1    2    1    1    2    2    1    2    1    2    2    2    2    1    1    2 
## 1277 1281 1282 1285 1286 1287 1291 1294 1306 1307 1315 1316 1319 1323 1330 1331 1332 1334 1336 1341 1345 1348 
##    2    1    2    2    1    1    1    1    2    1    2    2    1    2    2    2    1    2    1    1    2    2 
## 1358 1363 1365 1368 1374 1376 1379 1384 1393 1399 1406 1409 1413 1415 1418 1421 1423 1425 1427 1430 1433 1434 
##    2    2    1    1    1    1    2    2    1    1    2    1    2    2    1    1    2    1    2    1    1    1 
## 1439 1441 1442 1446 1447 1448 1449 1450 1473 1475 1489 1490 1494 1496 1497 1513 1517 1520 1525 1528 1531 1532 
##    2    2    1    1    1    2    2    2    2    2    1    2    1    1    1    2    2    1    2    1    1    2 
## 1534 1536 1539 1547 1551 1557 1558 1561 1566 1568 1570 1571 1572 1585 1590 1593 1595 1596 1603 1608 1611 1619 
##    1    2    2    2    2    1    1    1    2    2    2    2    2    1    1    1    2    2    2    2    1    2 
## 1620 1625 1627 1628 1629 1632 1637 1639 1647 1648 1649 1656 1663 1664 1674 1677 1678 1681 1684 1686 1688 1690 
##    1    2    2    2    2    1    1    1    1    2    1    2    1    1    1    1    2    1    1    2    1    2 
## 1692 1702 1703 1712 1720 1725 1726 1738 1743 1745 1747 1749 1755 1764 1767 1770 1773 1776 1779 1782 1784 1785 
##    2    2    1    2    1    2    2    1    1    1    2    2    1    2    2    2    2    1    2    2    1    1 
## 1790 1795 1796 1800 1801 1805 1809 1811 1815 1824 1832 1835 1842 1844 1857 1859 1863 1864 1872 1876 1881 1883 
##    2    1    2    2    2    2    1    1    1    2    2    2    1    2    2    2    2    2    1    2    2    1 
## 1885 1888 1895 1910 1911 1912 1914 1915 1924 1941 1944 1945 1952 1955 1956 1959 1965 1979 1981 1983 1987 1990 
##    2    1    1    2    1    1    2    1    2    1    1    2    1    2    2    1    2    2    1    2    1    2 
## 1996 1997 1999 2000 2001 2003 2008 2012 2017 2018 2020 2021 2025 2026 2027 2033 2036 2039 2041 2048 2051 2068 
##    1    2    2    1    2    1    1    2    2    2    2    2    1    2    2    1    1    2    2    1    2    2 
## 2071 2075 2076 2079 2105 2109 2113 2115 2119 2122 2128 2129 2132 2135 2136 2137 2139 2144 2146 2148 2150 2154 
##    2    2    1    2    2    1    2    1    2    2    1    2    2    1    1    1    1    1    2    2    1    1 
## 2155 2158 2160 2162 2175 2178 2181 2182 2185 2191 2197 2201 2203 2206 2212 2213 2215 2217 2224 2227 2229 2238 
##    2    1    1    1    1    2    2    1    2    2    2    1    1    2    2    1    1    1    2    2    2    1 
## 2239 2245 2247 2249 2255 2256 2267 2272 2275 2278 2280 2281 2283 2285 2286 2288 2291 2294 2302 2308 2310 2313 
##    2    1    2    1    2    2    2    1    2    2    2    1    1    1    1    2    1    1    1    1    1    1 
## 2314 2322 2336 2337 2341 2346 2347 2349 2350 2351 2353 2359 2362 2365 2374 2376 2380 2383 2384 2388 2396 2398 
##    2    1    2    1    1    2    2    1    1    1    1    2    2    1    2    1    2    1    2    2    2    2 
## 2399 2402 2404 2405 2412 2415 2416 2423 2438 2441 2443 2445 2452 2454 2456 2457 2459 2461 2463 2489 2491 2501 
##    2    1    2    1    1    1    1    2    1    1    1    1    2    2    1    2    2    1    2    1    1    1 
## 2504 2505 2509 2511 2518 2519 2521 2526 2528 2529 2530 2544 2548 2555 2560 2562 2563 2566 2567 2572 2573 2584 
##    2    2    2    1    1    1    2    1    1    2    1    2    1    2    1    2    2    2    1    2    1    2 
## 2586 2589 2594 2601 2602 2604 2611 2614 2627 2629 2639 2641 2642 2644 2650 2663 2665 2668 2673 2674 2678 2679 
##    2    2    1    1    2    2    1    2    2    1    2    2    1    1    1    1    1    2    2    1    2    2 
## 2680 2681 2682 2685 2691 2695 2696 2697 2705 2706 2709 2717 2720 2721 2723 2724 2725 2729 2731 2737 2740 2743 
##    2    1    1    2    1    2    2    1    1    2    2    1    2    1    1    2    2    2    2    2    2    2 
## 2753 2757 2760 2765 2769 2774 2775 2776 2778 2781 2794 2797 2799 2802 2803 2807 2824 2827 2833 2835 2844 2845 
##    2    2    2    2    2    1    2    1    1    1    2    2    1    2    1    1    2    1    2    1    2    1 
## 2851 2855 2856 2860 2862 2870 2875 2880 2881 2884 2886 2888 2892 2893 2897 2900 2908 2915 2916 2919 2929 2933 
##    1    2    1    1    2    1    1    1    1    2    1    1    2    1    1    2    2    1    2    2    2    2 
## 2934 2939 2943 2954 2956 2957 2961 2962 2963 2964 2970 2971 2972 2973 2977 2988 2999 3000 3002 3009 3011 3013 
##    1    1    2    1    1    2    1    1    1    2    2    1    2    2    1    1    2    2    1    1    1    1 
## 3020 3024 3029 3031 3034 3036 3037 3047 3048 3051 3058 3064 3075 3076 3088 3090 3091 3097 3104 3108 3112 3119 
##    1    2    1    1    1    2    2    1    2    2    2    2    2    1    1    1    2    1    2    2    1    2 
## 3134 3136 3138 3143 3149 3153 3154 3166 3169 3171 3175 3190 3200 3206 3207 3211 3214 3218 3223 3228 3230 3231 
##    2    2    2    2    1    2    2    1    1    1    2    2    2    2    1    1    2    1    2    2    1    2 
## 3240 3243 3245 3262 3268 3270 3272 3275 3278 3282 3285 3290 3291 3299 3300 3309 3316 3317 3318 3319 3321 3323 
##    2    1    1    1    1    2    2    1    2    2    1    2    1    1    2    1    2    1    2    1    1    1 
## 3324 3325 3326 3331 3334 3338 3340 3341 3344 3347 3350 3355 3356 3362 3363 3371 3382 3384 3385 3390 3391 3398 
##    1    2    1    1    2    2    1    2    1    1    1    2    2    1    1    1    1    2    1    2    1    1 
## 3399 3412 3413 3427 3432 3434 3439 3441 3446 3448 3450 3451 3457 3466 3471 3473 3474 3475 3477 3488 3492 3493 
##    2    2    1    2    2    1    1    1    1    1    1    2    2    2    1    2    1    1    1    1    1    2 
## 3496 3501 3505 3507 3508 3509 3515 3517 3519 3521 3523 3534 3535 3545 3546 3552 3555 3558 3565 3572 3576 3577 
##    2    2    1    1    2    1    1    2    2    1    2    1    2    2    1    2    1    1    1    1    1    2 
## 3586 3589 3591 3592 3594 3596 3604 3614 3616 3628 3632 3636 3642 3649 3652 3654 3655 3656 3666 3669 3673 3679 
##    1    2    1    1    2    1    1    1    2    2    1    2    2    2    1    1    1    2    2    2    1    2 
## 3681 3684 3686 3687 3692 3693 3694 3695 3703 3710 3712 3713 3718 3720 3726 3728 3735 3742 3747 3752 3758 3768 
##    2    2    1    1    1    2    2    2    2    1    2    1    1    2    2    2    1    2    2    1    2    1 
## 3770 3771 3772 3773 3775 3776 3777 3781 3783 3784 3787 3790 3791 3796 3803 3805 3811 3814 3816 3829 3834 3835 
##    2    1    1    2    1    1    2    2    2    2    1    1    2    2    1    2    2    2    1    1    2    1 
## 3839 3845 3854 3857 3860 3863 3869 3870 3874 3877 3878 3885 3886 3888 3890 3897 3905 3907 3914 3919 3924 3926 
##    2    2    1    1    2    1    1    1    2    1    2    2    1    1    1    1    1    1    2    2    2    2 
## 3932 3933 3938 3941 3944 3951 3953 3954 3956 3958 
##    2    2    2    1    1    2    1    1    2    2 
##  [ reached getOption("max.print") -- omitted 2000 entries ]
## 
## Within cluster sum of squares by cluster:
## [1]  6019.101 12893.421
##  (between_SS / total_SS =  36.9 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
## [8] "iter"         "ifault"
```

```r
#plotting cluster
fviz_cluster(wss_kmeans_testdata, data = fuel_cost_normalized_testdata )
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-3.png)

```r
varImp(model1, scale = FALSE)
```

```
## Warning in summary.lm(object): essentially perfect fit: summary may be unreliable
```

```
##                                  Overall
## new_datamine_id_pudl        6.169941e+00
## new_datafuel_mmbtu_per_unit 2.344597e+01
## new_datasulfur_content_pct  9.147009e+00
## new_datamercury_content_ppm 3.692552e+00
## new_datafuel_cost_per_mmbtu 1.595915e+16
```

```r
value_To_Predict<-model1[c(1,4,5,7,8)]

Prob<-predict(model1, data = value_To_Predict, type = "response")
Pred_data<-ifelse(Prob>0.3,"yes","no")

head(Pred_data)
```

```
##     1     7     8     9    19    22 
## "yes" "yes" "yes" "yes" "yes" "yes"
```


