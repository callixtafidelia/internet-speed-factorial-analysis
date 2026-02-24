#Investigating Factors Influencing Internet Speed on UBC Vancouver Campus
#Callixta Fidelia C, Yifan Hao, Jolin Lin, Louis Charistias Saragih  

#Load the data into R 
project_data<- read.csv("observation.csv")
project_data$Campus.Area = as.factor(project_data$Campus.Area)
project_data$Area = as.factor(project_data$Area)
project_data$Device = as.factor(project_data$Device)
project_data$Browser = as.factor(project_data$Browser)
project_data$Time = as.factor(project_data$Time)
project_data$Block = as.factor(project_data$Block)
project_data$Download..Mbps. = as.numeric(project_data$Download..Mbps.)


#Try the full linear model
model <- lm(Download..Mbps.~Campus.Area*Area*Device*Browser*Time+Block, data=project_data)
summary(model)

#Check whether the model need transformation or not from the boxcox
library(MASS)
boxcox(model)

#Try the ANOVA model 
aovmodel <- aov(Download..Mbps.~Campus.Area*Area*Device*Browser*Time+Block, data=project_data)
summary(aovmodel)

#Check the normality by plotting the residuals 
plot(aovmodel)

#Since the assumption of the normality violated, the number of interaction was reduced, in which we're looking for the model with: 

#Main effects + 2-way interaction

#Linear model
model2 <- lm(Download..Mbps. ~ (Campus.Area + Area + Device + Browser + Time)^2 + Block,
              data = project_data)
summary(model2)

#ANOVA model
aovmodel2 <- aov(
  Download..Mbps. ~ (Campus.Area + Area + Device + Browser + Time)^2 + Block,
  data = project_data)
summary(aovmodel2) 

#Check the normality by plotting the residuals 
plot(aovmodel2)

#Check whether the model need transformation or not from the boxcox
library(MASS)
boxcox(aovmodel2) # sqrt(Y) transformation needed

#The model we use: main + 3 way + transformation because of large improvement the adjusted r^2 = 0.8756

#ANOVA model
aovmodel3 <- aov(
  sqrt(Download..Mbps.) ~ (Campus.Area + Area + Device + Browser + Time)^3 + Block,
  data = project_data)
summary(aovmodel3)

#Linear model
model3 <- lm(sqrt(Download..Mbps.) ~ (Campus.Area + Area + Device + Browser + Time)^3 + Block,
               data = project_data)
summary(model3)

#Check the normality by plotting the residuals 
plot(aovmodel3)

#Check whether the model need transformation or not from the boxcox
boxcox(aovmodel3) #lambda = 1 already included in the 95% confidence interval

#Tukey test
TukeyHSD(aovmodel3, conf.level=.95)

#Shapiro test
shapiro.test(aovmodel3$residuals)

#Interaction plot
library(ggplot2)
interaction.plot(
  x.factor = project_data$Campus.Area,     # X-axis: The 3 campus zones
  trace.factor = project_data$Time,        # Lines: Morning vs. Afternoon
  response = sqrt(project_data$Download..Mbps.), # Response: Transformed speed
  fun = mean,                              # Plot the average speed
  type = "b",                              # 'b' = show both points and lines
  col = c("blue", "red"),                  # Blue line vs Red line
  pch = c(19, 17),                         # Solid circle vs Triangle
  fixed = TRUE,                            # Ensures legend order matches lines
  main = "Interaction: Campus Area by Time",
  ylab = "Mean Sqrt(Download Speed)",
  xlab = "Campus Area",
  trace.label = "Time of Day"
)

ggplot(project_data, aes(x = Campus.Area, 
                         y = sqrt(Download..Mbps.), 
                         color = Area, 
                         group = Area)) + 
  # Draw points and lines
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) + 
  
  # Split the graph by 'Time' (2 panels: Morning vs. Afternoon)
  facet_wrap(~ Time) + 
  
  # Labels and styling
  labs(title = "3-Way Interaction: Campus Area x Building Area x Time",
       y = "Mean Sqrt(Download Speed)",
       x = "Campus Zone (South/Middle/North)",
       color = "Building Position") + # Legend title
  theme_bw()

#Another model with the main effect + 4 way + transformation, getting adjusted r^2 = 0.9103 (not improved much since the last model with 3 interactions, therefore this model is not used)
#ANOVA model
aovmodel4 <- aov(
  sqrt(Download..Mbps.) ~ (Campus.Area + Area + Device + Browser + Time)^4 + Block,
  data = project_data)
summary(aovmodel4)
#Check the normality by plotting the residuals 
plot(aovmodel4)

#Linear model
model4 <- lm(sqrt(Download..Mbps.) ~ (Campus.Area + Area + Device + Browser + Time)^4 + Block,
                 data = project_data)
summary(model4)

#Check the normality by plotting the residuals 
plot(model4)

#Check whether the model need transformation or not from the boxcox
boxcox(aovmodel4) 

