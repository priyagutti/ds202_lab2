library(classdata)
library(tidyverse)
data(choco)
str(choco) 
View(choco)
# 1. How are the cocoa percent  distributed?
hist(choco$CocoaPercent)
boxplot(choco$CocoaPercent, main="Box Plot of Cocoa Percent",
        xlab="Chocolates", ylab="Cocoa Percent")
# There are multiple 100 percent Cocoa Percent chocolate that is not on the
#quartiles of the box plot.
#2. How does the number of ratings depend on the country of bean origin? Draw a bar chart of the number of ratings by bean origin. [Hint: If your graph is too dense and the labels are not legible, show fewer bean origins.]
choco$CountryBeanOrigin
summary(choco$CountryBeanOrigin)
tapply(choco$Rating,choco$CountryBeanOrigin, sum)%>%
sort(decreasing = TRUE)%>%
head(5)%>%
barplot(ylim=c(0,1000))

# 3. Do ratings depend on the cacao percentage of a chocolate bar? Create both numerical and graphical summaries, and explain.
cor(choco$Rating, choco$CocoaPercent, method = "pearson")
# The pearson correlation coefficient measure the linear dependency between two variables. 
#Since it is close to 0, there is little dependency.
tapply(choco$Rating, choco$CocoaPercent, summary)
test <- cor.test(choco$Rating, choco$CocoaPercent)
test
plot(x = choco$CocoaPercent,y =  choco$Rating,
     xlab = "Cocoa Percent",
     ylab = "Rating",
     main = "Cocoa Percent vs Ratings"
)
# The scatter plot isn't good visualization of the variables because it isn't a linear graph.
boxplot(choco$Rating ~ choco$CocoaPercent)


#4. How do different bean origins compare in terms of the flavor of chocolates? Are beans from some places better than the other? Create both numerical and graphical summaries, and explain.
flavor <- table(choco$Characteristics1)
length(flavor)
summary(as.numeric(flavor))
boxplot(flavor)
flavor_filtered <- flavor[flavor > 50]
choco_filtered <- filter(choco, Characteristics1 %in% names(flavor_filtered))
country_filtered <- table(choco$CountryBeanOrigin) %>% 
  sort(decreasing = TRUE) %>%
  head(10) # select the top 10 countries
tapply(choco$Rating, choco$CountryBeanOrigin, summary) %>%
head(5)
boxplot(tapply(choco$Rating, choco$CountryBeanOrigin, summary) %>%
          head(5))

# 5. If you work on this lab by yourself, brainstorm two analyses you would like to perform. 
cocoaandGood <- choco[choco$Rating >= 3.5 & choco$Characteristics1 == 'cocoa', ]
# This filters out everything that is below the rating of 3.5 and has the characteristics of only cocoa
summary(cocoaandGood$CocoaPercent)
#This shows the Cocoa Percent of the filtered data set.
