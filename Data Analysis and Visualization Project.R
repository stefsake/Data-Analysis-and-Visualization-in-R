# install the packages we will use
install.packages(
  c(
    "dplyr",
    "stringr",
    "pdftools",
    "tesseract",
    "writexl",
    "readxl",
    "tidyr",
    "scales",
    "ggpubr",
    "cowplot",
    "ggplot2"
  )
)

# activate the relevant libraries
library(stringr)
library(dplyr)
library(pdftools)
library(tesseract)
library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(cowplot)
theme_set(theme_bw() +
            theme(legend.position = "top"))

# read the R file that contains the CSR reports data
filename <- file.choose()
GRIexcel <- readRDS(filename)
as.data.frame(GRIexcel)
CSR_data <- GRIexcel

# check the data
View(CSR_data)

# create a new subset data frame only including Food and Beverage sector companies
FB_data <-
  (dplyr::filter(GRIexcel, Sector %in% "Food and Beverage Products"))

# remove spaces from the name column
FB_data$Name <- gsub(" ", "", FB_data$Name)

# rename for compatibility purposes
FB_data$Name <-
  paste(FB_data$Name, FB_data$`Publication Year`, sep = "_")

# Exclude unnecessary columns
FB_data <- FB_data[, c(1, 2, 4:7, 9, 10, 19, 20)]
View(FB_data)

# choose the directory we will use to retrieve the files and
# create a vector that contains all files
directory <-
  "C:/Users/Stefanos/Desktop/Data Visualization/Food_and_Beverage_Products"
files <- list.files(directory, pattern = ".pdf$" , full.names = T)

# creating a vector with all the keywords we want to include
# in our analysis
keywords <-
  c(
    "greenhouse",
    "gas",
    "emission",
    "diversity",
    "diversify",
    "diversification",
    "diverse",
    "diverseness",
    "employee health",
    "employee safety",
    "customer welfare",
    "consumer welfare",
    
    #Chinese
    "??????",
    "??????",
    "??????",
    "?????????",
    "?????????",
    "?????????",
    "???????????????",
    "?????????",
    "????????????",
    "????????????",
    "????????????",
    "???????????????",
    
    #German
    "Gewachshaus",
    "Gas",
    "Emission",
    "Diversitat",
    "diversifizieren",
    "Diversifizierung",
    "vielfaltig",
    "Vielfalt",
    "Mitarbeitergesundheit",
    "Mitarbeitersicherheit",
    "Gesundheit und Sicherheit der Mitarbeiter",
    "Kundenwohl",
    "Verbraucherwohlfahrt",
    
    #French
    "serre",
    "gaz",
    "emission",
    "la diversite",
    "diversifier",
    "diversification",
    "diverse",
    "diversite",
    "sante des salaries",
    "la securite des employes",
    "sante et securite des employes",
    "bien-etre client",
    "bien-etre des consommateurs",
    
    #Spanish
    "invernadero",
    "gas",
    "emision",
    "diversidad",
    "diversificar",
    "diversificacion",
    "diverso",
    "diversia",
    "salud de los empleados",
    "seguridad de los empleados",
    "salud y seguridad de los empleados",
    "bienestar del cliente",
    "bienestar del consumidor"
    
  )


# assign the files and the keywords length to variables
filelength <- length(files)
wordlength <- length(keywords)

# create a matrix for our data
word_count <- seq(1, filelength * wordlength)
dim(word_count) <- c(filelength, wordlength)

# process the files for the text analysis
for (j in 1:length(files)) {
  P1 <- pdftools::pdf_text(pdf = files[j]) %>%
    str_to_lower() %>%
    str_replace_all("\\t", "") %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("      ", " ") %>%
    str_replace_all("    ", " ") %>%
    str_replace_all("   ", " ") %>%
    str_replace_all("  ", " ") %>%
    str_replace_all("[:digit:]", "") %>%
    str_replace_all("[:punct:]", "") %>%
    str_trim()
  
  for (i in 1:length(keywords)) {
    word_count[j, i] <- P1 %>% str_count(keywords[i]) %>% sum()
  }
  
}

# convert the matrix into a data frame
word_count <- as.data.frame(word_count)

# set the rows and columns accordingly
rownames(word_count) <- files
colnames(word_count) <- keywords

# check how the matrix looks like
View(word_count)

word_count <-
  cbind(Name = rownames(word_count_sums), word_count_sums)


#export the data frame to excel to analyze the data more efficiently
write_xlsx(CSR_data,
           "C:/Users/Stefanos/Desktop/Data Visualization/CSR_data.xlsx")

#import the edited excel file in R as data frame
exceldata2 = read_excel("C:/Users/Stefanos/Desktop/Data Visualization/word_count_test.xlsx")
word_count_sums = data.frame(exceldata2)
View(word_count_sums)

# remove the unnecessary characters of the path
# to get the "Name.pdf" format
word_count_sums_name <-
  transform(word_count_sums, Name = substr(Name, 73, 150))

# remove the .pdf format to only have the company Name
word_count_sums_name_separated <-
  separate(word_count_sums_name, Name,
           into = "Name", sep = ".pdf")

# check the data frame
View(word_count_sums_name_separated)

# merge (join) the two data frames on the column "Name"
merged_df <-
  merge(FB_data, word_count_sums_name_separated, by = "Name")

# exclude the year 2018 because of limited data
merged_df <-
  (dplyr::filter(merged_df, `Publication Year` < 2018))

# view the merged data frame
View(merged_df)

# create a subset of the CSR_data without the year 2018
filtered_CSR_data <-
  (dplyr::filter(CSR_data, `Publication Year` < 2018))

# Doing the same for the FB_data
filtered_FB_data <-
  (dplyr::filter(exceldata, Publication_Year < 2018))

# Making value names shorter for visual purposes
filtered_FB_data$Country[filtered_FB_data$Country == "United Kingdom of Great Britain and Northern Ireland"] <-
  "United Kingdom"

# create percentage columns for each one of the topics
# greenhouse gas emission
filtered_FB_data$percent_greenhouse <-
  (
    filtered_FB_data$greenhouse_gas_emmision /
      sum(filtered_FB_data$greenhouse_gas_emmision)
  ) * 100

# diversity
filtered_FB_data$percent_diversity <- (filtered_FB_data$diversity /
                                         sum(filtered_FB_data$diversity)) *
  100

# employee health and safety
filtered_FB_data$percent_employee_health <-
  (
    filtered_FB_data$employee_health_and_safety /
      sum(filtered_FB_data$employee_health_and_safety)
  ) * 100

# customer welfare
filtered_FB_data$percent_customer_welfare <-
  (filtered_FB_data$customer_welfare /
     sum(filtered_FB_data$customer_welfare)) *
  100

# view the updated data frame
View(filtered_FB_data)


## Start creating visualizations using ggplot ##

# first convert in percentage
merged_# install the packages we will use
install.packages(
  c(
    "dplyr",
    "stringr",
    "pdftools",
    "tesseract",
    "writexl",
    "readxl",
    "tidyr",
    "scales",
    "ggpubr",
    "cowplot",
    "ggplot2"
  )
)

# activate the relevant libraries
library(stringr)
library(dplyr)
library(pdftools)
library(tesseract)
library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(cowplot)
theme_set(theme_bw() +
            theme(legend.position = "top"))

# read the R file that contains the CSR reports data
filename <- file.choose()
GRIexcel <- readRDS(filename)
as.data.frame(GRIexcel)
CSR_data <- GRIexcel

# check the data
View(CSR_data)

# create a new subset data frame only including Food and Beverage sector companies
FB_data <-
  (dplyr::filter(GRIexcel, Sector %in% "Food and Beverage Products"))

# remove spaces from the name column
FB_data$Name <- gsub(" ", "", FB_data$Name)

# rename for compatibility purposes
FB_data$Name <-
  paste(FB_data$Name, FB_data$`Publication Year`, sep = "_")

# Exclude unnecessary columns
FB_data <- FB_data[, c(1, 2, 4:7, 9, 10, 19, 20)]
View(FB_data)

# choose the directory we will use to retrieve the files and
# create a vector that contains all files
directory <-
  "C:/Users/Stefanos/Desktop/Data Visualization/Food_and_Beverage_Products"
files <- list.files(directory, pattern = ".pdf$" , full.names = T)

# creating a vector with all the keywords we want to include
# in our analysis
keywords <-
  c(
    "greenhouse",
    "gas",
    "emission",
    "diversity",
    "diversify",
    "diversification",
    "diverse",
    "diverseness",
    "employee health",
    "employee safety",
    "customer welfare",
    "consumer welfare",
    
    #Chinese
    "??????",
    "??????",
    "??????",
    "?????????",
    "?????????",
    "?????????",
    "???????????????",
    "?????????",
    "????????????",
    "????????????",
    "????????????",
    "???????????????",
    
    #German
    "Gewachshaus",
    "Gas",
    "Emission",
    "Diversitat",
    "diversifizieren",
    "Diversifizierung",
    "vielfaltig",
    "Vielfalt",
    "Mitarbeitergesundheit",
    "Mitarbeitersicherheit",
    "Gesundheit und Sicherheit der Mitarbeiter",
    "Kundenwohl",
    "Verbraucherwohlfahrt",
    
    #French
    "serre",
    "gaz",
    "emission",
    "la diversite",
    "diversifier",
    "diversification",
    "diverse",
    "diversite",
    "sante des salaries",
    "la securite des employes",
    "sante et securite des employes",
    "bien-etre client",
    "bien-etre des consommateurs",
    
    #Spanish
    "invernadero",
    "gas",
    "emision",
    "diversidad",
    "diversificar",
    "diversificacion",
    "diverso",
    "diversia",
    "salud de los empleados",
    "seguridad de los empleados",
    "salud y seguridad de los empleados",
    "bienestar del cliente",
    "bienestar del consumidor"
    
  )


# assign the files and the keywords length to variables
filelength <- length(files)
wordlength <- length(keywords)

# create a matrix for our data
word_count <- seq(1, filelength * wordlength)
dim(word_count) <- c(filelength, wordlength)

# process the files for the text analysis
for (j in 1:length(files)) {
  P1 <- pdftools::pdf_text(pdf = files[j]) %>%
    str_to_lower() %>%
    str_replace_all("\\t", "") %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("      ", " ") %>%
    str_replace_all("    ", " ") %>%
    str_replace_all("   ", " ") %>%
    str_replace_all("  ", " ") %>%
    str_replace_all("[:digit:]", "") %>%
    str_replace_all("[:punct:]", "") %>%
    str_trim()
  
  for (i in 1:length(keywords)) {
    word_count[j, i] <- P1 %>% str_count(keywords[i]) %>% sum()
  }
  
}

# convert the matrix into a data frame
word_count <- as.data.frame(word_count)

# set the rows and columns accordingly
rownames(word_count) <- files
colnames(word_count) <- keywords

# check how the matrix looks like
View(word_count)

word_count <-
  cbind(Name = rownames(word_count_sums), word_count_sums)


#export the data frame to excel to analyze the data more efficiently
write_xlsx(CSR_data,
           "C:/Users/Stefanos/Desktop/Data Visualization/CSR_data.xlsx")

#import the edited excel file in R as data frame
exceldata2 = read_excel("C:/Users/Stefanos/Desktop/Data Visualization/word_count_test.xlsx")
word_count_sums = data.frame(exceldata2)
View(word_count_sums)

# remove the unnecessary characters of the path
# to get the "Name.pdf" format
word_count_sums_name <-
  transform(word_count_sums, Name = substr(Name, 73, 150))

# remove the .pdf format to only have the company Name
word_count_sums_name_separated <-
  separate(word_count_sums_name, Name,
           into = "Name", sep = ".pdf")

# check the data frame
View(word_count_sums_name_separated)

# merge (join) the two data frames on the column "Name"
merged_df <-
  merge(FB_data, word_count_sums_name_separated, by = "Name")

# exclude the year 2018 because of limited data
merged_df <-
  (dplyr::filter(merged_df, `Publication Year` < 2018))

# view the merged data frame
View(merged_df)

# create a subset of the CSR_data without the year 2018
filtered_CSR_data <-
  (dplyr::filter(CSR_data, `Publication Year` < 2018))

# Doing the same for the FB_data
filtered_FB_data <-
  (dplyr::filter(exceldata, Publication_Year < 2018))

# Making value names shorter for visual purposes
filtered_FB_data$Country[filtered_FB_data$Country == "United Kingdom of Great Britain and Northern Ireland"] <-
  "United Kingdom"

# create percentage columns for each one of the topics
# greenhouse gas emission
filtered_FB_data$percent_greenhouse <-
  (
    filtered_FB_data$greenhouse_gas_emmision /
      sum(filtered_FB_data$greenhouse_gas_emmision)
  ) * 100

# diversity
filtered_FB_data$percent_diversity <- (filtered_FB_data$diversity /
                                         sum(filtered_FB_data$diversity)) *
  100

# employee health and safety
filtered_FB_data$percent_employee_health <-
  (
    filtered_FB_data$employee_health_and_safety /
      sum(filtered_FB_data$employee_health_and_safety)
  ) * 100

# customer welfare
filtered_FB_data$percent_customer_welfare <-
  (filtered_FB_data$customer_welfare /
     sum(filtered_FB_data$customer_welfare)) *
  100

# view the updated data frame
View(filtered_FB_data)


##Start plotting our findings using ggplot##

# first create a version of the FB_data including a percentage column
merged_df %>%
  count(`Publication Year`, Region, Size, Country) %>%
  mutate(perc = n / nrow(merged_df)) -> merged_df_percentage

# Plot 1
# Number of reports (%) by company size through the years
# for the Food and Beverage sector companies
my_plot1 <-
  ggplot(merged_df_percentage,
         aes(`Publication Year`, y = perc, fill = Size)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Number of reports by company size\n (Food & Beverage sector)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))
my_plot1

# create a version of the CSR_data including a percentage column
filtered_CSR_data %>%
  count(`Publication Year`, Size, Region) %>%
  mutate(perc = n / nrow(filtered_CSR_data)) -> filtered_CSR_data_percentage

# check the updated data frame
View(filtered_CSR_data_percentage)

# Plot 2
# Number of reports (%) by company size through the years
# for companies from all sectors
my_plot2 <-
  ggplot(filtered_CSR_data_percentage,
         aes(`Publication Year`, y = perc, fill = Size)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Number of reports by company size\n (All sectors)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))
my_plot2


#Plot 3
# Number of reports by company size for each Region through the years (All sectors)
my_plot3 <-
  ggplot(filtered_CSR_data_percentage,
         aes(`Size`, y = perc, fill = Size)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Region) +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Company Size") +
  ylab("Number of Reports %") +
  ggtitle("Number of reports by company size for each Region\n (All sectors)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))
my_plot3


# plot 4
# Number of reports by company size for each Region through the years (F&B sector)
my_plot4 <-
  ggplot(merged_df_percentage,
         aes(`Size`, y = perc, fill = Size)) +
  geom_bar(stat = "identity") +
  facet_grid(~ Region) +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Company Size") +
  ylab("Number of Reports %") +
  ggtitle("Number of reports by company size for each Region\n (Food & Beverage sector)") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))
my_plot4


# create a data frame grouped by Region and summed by greenhouse_gas_emission
test5 <- filtered_FB_data %>% group_by(Region)  %>%
  summarise(greenhouse_gas_emmision = sum(greenhouse_gas_emmision))

# create an updated data frame including a percentage column
test5 %>%
  count(`Region`, greenhouse_gas_emmision) %>%
  mutate(perc =  test5$greenhouse_gas_emmision / sum(test5$greenhouse_gas_emmision)) -> test5_percentage



# plot 5
# greenhouse gas emissions by Region
myplot5 <- ggplot(test4_percentage, aes(Region, perc)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkgreen") +
  xlab("Region") +
  ylab("Number of times mentioned (%)") +
  ggtitle("Greenhouse Gas Emissions") +
  theme(plot.title = element_text(hjust = 0.5) +
          theme(axis.title.x = element_text(margin = margin(r = 10)))) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(vjust = 2, hjust = 0.5))
myplot5


# create a data frame grouped by Region and summed by diversity
test6 <- filtered_FB_data %>% group_by(Region)  %>%
  summarise(diversity = sum(diversity))
View(test6)

# create an updated data frame including a percentage column
test6 %>%
  count(`Region`, diversity) %>%
  mutate(perc =  test6$diversity / sum(test6$diversity)) -> test6_percentage


# plot 6
# diversity by Region
myplot6 <- ggplot(test6_percentage, aes(Region, perc)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkblue") +
  xlab("Region") +
  ylab("Number of times mentioned (%)") +
  ggtitle("Diversity") +
  theme(plot.title = element_text(hjust = 0.5) +
          theme(axis.title.x = element_text(margin = margin(r = 10)))) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(vjust = 2, hjust = 0.5))
myplot6


# create a data frame grouped by Region and summed by employee_health_and_safety
test7 <- filtered_FB_data %>% group_by(Region)  %>%
  summarise(employee_health_and_safety = sum(employee_health_and_safety))

# create an updated data frame including a percentage column
test7 %>%
  count(`Region`, employee_health_and_safety) %>%
  mutate(perc =  test7$employee_health_and_safety / sum(test7$employee_health_and_safety)) -> test7_percentage


# plot 7 - Employee Health & Safety by Region
myplot7 <- ggplot(test7_percentage, aes(Region, perc)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkorange2") +
  xlab("Region") +
  ylab("Number of times mentioned (%)") +
  ggtitle("Employee Health & Safety") +
  theme(plot.title = element_text(hjust = 0.5) +
          theme(axis.title.x = element_text(margin = margin(r = 10)))) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(vjust = 2, hjust = 0.5))
myplot7


# create a data frame grouped by Region and summed by customer_welfare
test8 <- filtered_FB_data %>% group_by(Region)  %>%
  summarise(customer_welfare = sum(customer_welfare))

# create an updated data frame including a percentage column
test8 %>%
  count(Region, customer_welfare) %>%
  mutate(perc =  test8$customer_welfare / sum(test8$customer_welfare)) -> test8_percentage

# plot 8 - Customer Welfare by Region
myplot8 <- ggplot(test8_percentage, aes(Region, perc)) +
  geom_bar(stat = "identity", width = 0.5, fill = "brown4") +
  xlab("Region") +
  ylab("Number of times mentioned (%)") +
  ggtitle("Customer Welfare") +
  theme(plot.title = element_text(hjust = 0.5) +
          theme(axis.title.x = element_text(margin = margin(r = 10)))) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  theme(plot.title = element_text(vjust = 2, hjust = 0.5))
myplot8


# create a data frame grouped by Region and summed by each one of the topics
merged_df_grouped <- merged_df %>% group_by(Region)  %>%
  summarise(
    greenhouse_gas_emmision = sum(greenhouse_gas_emmision),
    diversity = sum(diversity),
    employee_health_and_safety = sum(employee_health_and_safety),
    customer_welfare = sum(customer_welfare)
  )

# create an updated data frame including percentage columns
merged_df_grouped %>%
  count(`Publication Year`, Size, Region) %>%
  mutate(perc = n / nrow(filtered_CSR_data)) -> filtered_CSR_data_percentage

# check the data frame
View(filtered_CSR_data_percentage)



# create a data frame grouped by Publication Year and summed by greenhouse gas emission
test_ss <- merged_df %>% group_by(`Publication Year`)  %>%
  summarise(greenhouse_gas_emmision = sum(greenhouse_gas_emmision))

# create an updated data frame including a percentage column
test_ss %>%
  count(`Publication Year`, greenhouse_gas_emmision) %>%
  mutate(perc =  test_ss$greenhouse_gas_emmision / sum(test_ss$greenhouse_gas_emmision)) -> test_ss_percentage

# check the data frame
View(test_ss_percentage)

# Plot 9 - greenhouse gas emissions by Publication Year
my_plot_greenhouse <-
  ggplot(test_ss_percentage,
         aes(`Publication Year`, y = perc)) +
  geom_bar(stat = "identity", width = 0.7, fill = "darkgreen") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Greenhouse Gas Emissions") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))

my_plot_greenhouse


# create a data frame grouped by Publication Year and summed by diversity
test_ss2 <- merged_df %>% group_by(`Publication Year`)  %>%
  summarise(diversity = sum(diversity))

# create an updated data frame including a percentage column
test_ss2 %>%
  count(`Publication Year`, diversity) %>%
  mutate(perc =  test_ss2$diversity / sum(test_ss2$diversity)) -> test_ss2_percentage

# check the data frame
View(test_ss2_percentage)


# Plot 10 - diversity by Publication Year
my_plot_diversity <-
  ggplot(test_ss2_percentage,
         aes(`Publication Year`, y = perc)) +
  geom_bar(stat = "identity", width = 0.6, fill = "darkblue") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Diversity") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))

my_plot_diversity


# create a data frame grouped by Publication Year and summed by employee health & safety
test_ss3 <- merged_df %>% group_by(`Publication Year`)  %>%
  summarise(employee_health_and_safety = sum(employee_health_and_safety))

# create an updated data frame including a percentage column
test_ss3 %>%
  count(`Publication Year`, employee_health_and_safety) %>%
  mutate(perc =  test_ss3$employee_health_and_safety / sum(test_ss3$employee_health_and_safety)) -> test_ss3_percentage


# check the data frame
View(test_ss3_percentage)


# Plot 11 - employee health & safety by Publication Year
my_plot_employee_health_and_safety <-
  ggplot(test_ss3_percentage,
         aes(`Publication Year`, y = perc)) +
  geom_bar(stat = "identity", width = 0.6, fill = "darkorange2") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Employee Health & Safety") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))

my_plot_employee_health_and_safety



# create a data frame grouped by Publication Year and summed by customer welfare
test_ss4 <- merged_df %>% group_by(`Publication Year`)  %>%
  summarise(customer_welfare = sum(customer_welfare))

# create an updated data frame including a percentage column
test_ss4 %>%
  count(`Publication Year`, customer_welfare) %>%
  mutate(perc =  test_ss4$customer_welfare / sum(test_ss4$customer_welfare)) -> test_ss4_percentage

# check the data frame
View(test_ss4_percentage)


# Plot 12 - customer welfare by Publication Year
my_plot_customer_welfare <-
  ggplot(test_ss4_percentage,
         aes(`Publication Year`, y = perc)) +
  geom_bar(stat = "identity", width = 0.6, fill = "brown2") +
  theme(axis.text.x = element_text(
    angle = 42,
    vjust = 0.6,
    hjust = 1,
    size = 6,
    margin = margin(r = 20)
  )) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlab("Publication Year") +
  ylab("Number of Reports (%)") +
  ggtitle("Customer Welfare") +
  theme(
    axis.title.x = element_text(margin = margin(t = 13)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.25))

my_plot_customer_welfare


### create the final visualizations ###

# create combined plot from plots 1 & 2
figure1 <- ggarrange(my_plot2, my_plot1)
figure1


# create combined plot from plots 3 & 4
figure2 <- ggarrange(my_plot3, my_plot4)
figure2


# create combined plot that shows the frequency of each topic by Publication Year
figure3 <- ggarrange(
  my_plot_greenhouse,
  my_plot_diversity,
  my_plot_employee_health_and_safety,
  my_plot_customer_welfare
)
figure3


# create combined plot from plots 5,6,7 & 8
figure4 <- ggarrange(myplot5, myplot6, myplot7, myplot8)
figure4


# save and export the final visualizations
save_plot("DV_combined_plots_1&2.pdf",
          figure1,
          ncol = 2,
          nrow = 2)

save_plot("DV_combined_plots_3&4.pdf",
          figure2,
          ncol = 2,
          nrow = 2)

save_plot(
  "DV_combined_plots_topics_by_publication_year.pdf",
  figure3,
  ncol = 2,
  nrow = 2
)

save_plot(
  "DV_number_of_reports_by_company_size_each_region.pdf",
  figure4,
  ncol = 2.5,
  nrow = 2
)
