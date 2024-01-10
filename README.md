export <- read.csv("C:/Payel/KPMG Internal_cv/Brief/export.csv")
View(export)
# Change the column names
names(export) = gsub(" ", "_", names(export))
head(export)
# Fit the multiple linear regression model
export_model_1 = lm(formula = ExportspercentageofGDP ~ exchange_rate + inflation +  gdp + FDI + manufacturing + Import , data = export)
summary(export_model_1)

cor(export)
# Install and load the ggcorrplot package
install.packages("ggcorrplot")
library(ggcorrplot)

# Remove the export column
reduced_data <- subset(export, select = -ExportspercentageofGDP)


# Compute correlation at 2 decimal places
corr_matrix = round(cor(reduced_data), 2)

# Compute and show the  result
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",lab = TRUE)

export_model_2 = lm(formula = ExportspercentageofGDP ~  exchange_rate + inflation  + FDI + manufacturing + Import , data = export)
summary(export_model_2)

anova(export_model_1, export_model_2) 
# export=3.653e-01 + 5.767e-02(exchange_rate)+ (-4.076e-02) inflation...

# define new data frame of export

newdata = data.frame(exchange_rate = c(83.29, 87.97, 92.66),FDI = c(55153239245, 60366220086, 70792181768), Import = c(28, 31, 33), manufacturing = c(12, 11, 9), inflation = c(8, 9, 11), gdp = c(3690000000000, 3960000000000, 4230000000000))
newdata 

# use model to predict 
predict(export_model_1, newdata)
                                      


                    
