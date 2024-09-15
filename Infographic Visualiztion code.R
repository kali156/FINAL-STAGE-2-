#used library
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsci)

# to Download data
AMR_Products<- read.delim("https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/WHO_AMR_PRODUCTS_DATA.tsv", sep = "\t", header = TRUE)
#preprocessing
AMR_Products[is.na(AMR_Products)]<-'unknown'

write.table(AMR_Products,"AMR_Product.tsv")
#Identify Key Trends:


#1- distribution of Product Type
Product_type_summary<-AMR_Products%>%
  filter(!duplicated(AMR_Products$Product.name))%>%
  group_by(Product.type)%>%
  summarise(count=n())

ggplot(Product_type_summary, aes(x = Product.type, y = count, fill =Product.type)) +
  geom_bar(stat = 'identity',width = 0.3,position = 'dodge') +
  scale_fill_manual(values = c("#4F81BD", "#C0504D"))  +
  theme_minimal()+xlab('')+
  theme(text = element_text(size = 9),
        plot.title = element_text(hjust =0.5,face = 'bold' ))

#

#-----------------
#Create the  bar chart..
#..that shows distribution of Product Type with activity status
product_activity <- AMR_Products %>%
  filter(Active.against.priority.pathogens. !='N/A')%>%
  group_by(Product.type, Active.against.priority.pathogens.) %>%
  summarise(Count = n(), .groups = 'drop')

# Create faceted bar plot
ggplot(product_activity, aes(x = Product.type, y = Count, fill = Active.against.priority.pathogens.)) +
  geom_bar(stat = "identity",width = 0.4) +
  facet_wrap(~ Active.against.priority.pathogens.) +
  scale_fill_manual(values = c("Yes" = "#BC0CFF", "No" = "#008080","Possibly"="#8B0000")) +
  labs(
    x = "Product Type",
    y = "Count",
    fill = "Activity Status") +
  xlab('')
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 4),
        axis.title.x  = element_text(size = 2),
        axis.text = element_text(size = 4))
