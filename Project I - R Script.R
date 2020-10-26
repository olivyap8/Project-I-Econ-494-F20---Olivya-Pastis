library(readxl) #open excel file with compiled dataset
> Project_I <- read_excel("Desktop/Project I.xlsx") #name dataset
> View(Project_I) #open dataset
Project_1<-Project_I  #create new dataset from starting point for deletion when necessary
Project_1$`Child Mortality (0-5 year olds dying per 1000 born)`[Project_1$`Child Mortality (0-5 year olds dying per 1000 born)`>1000]<-NA
View(Project_1) #assigns NA values for invalid responses
Project_1$`Life Expectancy (years)`[Project_1$`Life Expectancy (years)`>115]<-NA #assign necessary NA values for each variable
Project_1$`Median Age (years)`[Project_1$`Median Age (years)`>115]<-NA
Project_1$`Income per person (GDP/capita)`[Project_1$`Income per person (GDP/capita)`>100000]<-NA
Project_1$`Population, total`[Project_1$`Population, total`>7000000000]<-NA
Project_1$`Employment Rate Aged 15+`[Project_1$`Employment Rate Aged 15+`>1]<-NA
dim(Project_1) #no data lost, same as Project_I
library(ggplot2) #run ggplot2

#scatterplot comparing income vs. child mortality rate, add & center title
ggplot(Project_1, aes(x = `Child Mortality (0-5 year olds dying per 1000 born)`, y = `Income per person (GDP/capita)`)) + 
  geom_point() + geom_smooth() + ggtitle("Income vs. Child Mortality Rate") + 
  theme(plot.title = element_text(hjust = 0.5))

#histogram displaying the distribution of income per person
plot_income<-ggplot(Project_1, aes(x=`Income per person (GDP/capita)`)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth=1000)
plot_income + ggtitle("Income Density Distribution") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_density(color = "darkblue", linetype="dashed")
summary(`Income per person (GDP/capita)`)

#violin plot of employment rate for 15+ vs. income per person
plot_violin<-ggplot(Project_1, aes(x = `Income per person (GDP/capita)`, y = `Employment Rate Aged 15+`)) +
  geom_violin() + ggtitle("Employment Rate vs. Income") + 
  theme(plot.title = element_text(hjust = 0.5))
plot_violin
summary(`Employment Rate Aged 15+`, na.rm = TRUE)

#two graph comparison of median age and income per person
ggplot(Project_1, aes(`Median Age (years)`)) + geom_histogram() + 
  ggtitle("Median Age Distribution") + theme(plot.title = element_text(hjust = 0.5))

ggplot(Project_1, aes(`Median Age (years)`, `Income per person (GDP/capita)`)) + geom_boxplot() +
  ggtitle("Income vs. Median Age per Country") + theme(plot.title = element_text(hjust = 0.5))
summary(`Median Age (years)`)

#histogram for life expectancy
ggplot(Project_1, aes(x = `Life Expectancy (years)`, y = `Income per person (GDP/capita)`)) + 
  geom_point() + geom_smooth() + ggtitle("Income vs. Life Expectancy") + 
  theme(plot.title = element_text(hjust = 0.5))

#histogram for population
ggplot(Project_1, aes(`Population, total`)) + geom_histogram() + 
  ggtitle("Total Population Distribution") + theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0,1.0e+08)
