mydata<-read.csv("C:\\Users\\sreen\\Desktop\\Gt Acads\\Regression\\Project\\Data\\clean_data.csv")
model<-lm(SalePrice~Gr_Liv_Area, data=mydata)
library(MASS)
boxcox(model)

model2<-lm(log(SalePrice)~ Gr_Liv_Area, data=mydata)
cooksd <- cooks.distance(model)
mydata[cooksd>0.1,]

# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(mydata)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 0.1, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>0.1, names(cooksd),""), col="red")  # add labels
