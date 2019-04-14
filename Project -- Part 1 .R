### PROJECT PART 1 ### 

library(ggplot2)
library(ggthemes)

## Importing the countries data here via CSV from Kaggle ## 
setwd("/Users/vincientarnold/Desktop/STAT3080/")

# --- This data comes from World Bank and was retrieved from Kaggle.com --- # 

## Making some Important Subsets of the Data ## 
GDP_data <- read.csv("GDP_data_mine.csv")
   # GDP_data simply reads in the entire file as a CSV (edited from the original from Kaggle), #
   # so this is the GDP (USD) of all nations from 1960-2016 (World Bank data)                  #    


# --- Making subset of East Asian and Pacific GDP figures against World from 1960 to 2016 --- # 

East_Asia_Pacific <- as.numeric( (GDP_data[which(GDP_data$Country.Code=="EAS"), (5:61) ]) ) 
World <- as.numeric( (GDP_data[which(GDP_data$Country.Code=="WLD"),(5:61) ]) )

World_and_East_mat <- matrix(NA, 57, 3) 
World_and_East_mat[,1] <- c(1960:2016)
World_and_East_mat[,2] <- World
World_and_East_mat[,3] <- East_Asia_Pacific
colnames(World_and_East_mat) <- c("Years", "World", "East Asia and Pacific")
World_Asia_dfrm <- as.data.frame(World_and_East_mat)


# --- Making a basic ggplot() plot of the above data to show yearly trends in GPD growth Asia vs World --- # 
theme_set(theme_economist())
World_plot <- ggplot(World_Asia_dfrm, aes(x=Years, y= (World/10^12)) ) 
World_plot <- World_plot + geom_point(shape=1, size = 2) + 
  geom_smooth(colour= "red",alpha=0.01, linetype="dashed") + 
  labs(title = "World GDP (USD) 1960 - 2016", y="World GDP (trillion USD)") +
  scale_x_continuous(name="Year", breaks =seq(1960, 2016,5) ) + 
  theme(axis.text.x = element_text(angle=45, size =8, vjust = 0.8, family = "Times"),
        axis.text.y = element_text(size = 8, family = "Times"), 
        axis.title.x = element_text(size = 12, family = "Times"), 
        axis.title.y = element_text(size = 12, family = "Times"), 
        plot.title = element_text(size = 16, family = "Times") ) 
   
World_plot


# --- Asian Pacific Share of Global GDP --- # 
Asia_share_World_GDP <- (World_Asia_dfrm[,3] / World_Asia_dfrm[,2] ) * 100 
World_Asia_dfrm[,4] <- Asia_share_World_GDP 
colnames(World_Asia_dfrm) <- c("Years", "World", "East Asia and Pacific", "Asian share of World GDP")
World_Asia_dfrm



# --- ggplot() plot of Asian share of World GDP --- # 
theme_set(theme_economist())
Asian_share_plot <- ggplot(World_Asia_dfrm, aes(x = Years, y = (World_Asia_dfrm[,4] )))
Asian_share_plot <- Asian_share_plot + geom_point(shape=16, size =2, colour="black", fill="black") +
  labs(title="East Asia Pacific Share of Global GDP (%)", y= "East Asia Pacific Share of World GDP (%)") + 
  scale_x_continuous(name="Year", breaks =seq(1960, 2016, 5) ) + 
  theme(axis.text.x = element_text(angle=45, size=9, vjust = 0.8, family = "Times"), 
        axis.text.y = element_text(size=10, family ="Times"), 
        plot.title = element_text(size = 16, family = "Times"), 
        axis.title.y = element_text(size = 12, family = "Times"), 
        axis.title.x = element_text(size = 12, family = "Times"))
Asian_share_plot



# --- Numerical summary of growth rates for Asia and World GPD --- # 

# Below is a loop to calculate change in GDP in the EAS region # 
for(i in 2:57){
  World_Asia_dfrm[i,5] <- (( World_Asia_dfrm[i,3] - World_Asia_dfrm[(i-1),3] ) / 
                             World_Asia_dfrm[(i-1),3])*100
  i <- i + 1
}

# Below is a loop to calculate change in GDP in the World # 
for(i in 2:57){
  World_Asia_dfrm[i,6] <- (( World_Asia_dfrm[i,2] - World_Asia_dfrm[(i-1),2] ) / 
                             World_Asia_dfrm[(i-1),2])*100
  i <- i + 1
}



World_Asia_dfrm[,5] <- round(World_Asia_dfrm[,5], 3) 
World_Asia_dfrm[,4] <- round(World_Asia_dfrm[,4], 3)
colnames(World_Asia_dfrm) <- c("Years", "World GDP", "East Asia and Pacific GDP", 
                               "Asian share of World GDP (%)", "East Asia and Pacific Growth Rates (%)",
                               "World Growth Rates (%)")

avg_world_growth <- mean(World_Asia_dfrm[2:57,6])
avg_Asian_growth <- mean(World_Asia_dfrm[2:57,5])
avg_world_growth
avg_Asian_growth


World_Asia_dfrm_by5 <- World_Asia_dfrm[seq(1,57,5),]

# table <- apply_labels(World_Asia_dfrm_by5, World_Asia_dfrm_by5$Years="Years",
                      # World_Asia_dfrm_by5$`World GDP`="World GDP")

# cro(World_Asia_dfrm_by5$Years, World_Asia_dfrm_by5$`World GDP`) 

