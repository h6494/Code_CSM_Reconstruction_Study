#Breakpoint analysis on metals data obtained from individual coring sites
#________________________________________________________________________

#format your excel file with:
  #_ depth in the first column
  #_ depth values should read as numbers ("0,1,2..."), not intervals ("0-0.5, 1-1.5, 2-2.5...")
  #_ add your metal concentrations for each depth in the following columns
  #_ column headers should read as "depth", "silver", "arsenic", etc.

#save your file in .csv format

#set your working directory to the file where your data is located
setwd("~/Desktop")

#installation may differ depending on the type of operating system you're using (Windows, IO, Ubuntu)
#if using ubuntu, start by opening your terminal on your Desktip and installing libcurl and vegan
sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev libv8-dev
sudo apt-get install -y r-cran-vegan 

#install the following tidy packages in R
install.packages("tidyverse")
install.packages("vegan")
install.packages("tidypaleo")

#load the tidy libraries
library("tidyverse")
library("tidypaleo")

#load your .csv file containing metals data, define as "metals"
metals<-read_csv("metals_REF3.csv")

#make metals concentration data long, define as "metals_long"
metals_long <- metals %>%
  pivot_longer(!depth, names_to = "metals", values_to = "concentration")

#make sure depth column is numeric
metals_long$depth <- as.numeric(metals_long$depth)

#run the stratigraphically constrained cluster analysis on your data (CONISS)
nested_coniss <- metals_long %>%
  nested_data(depth, metals, concentration, fill = 0) %>%
  nested_chclust_coniss()
plot(nested_coniss)


#create a broken stick plot showing 
nested_coniss %>%
  select(broken_stick) %>%
  unnest(broken_stick) %>%
  tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
  ggplot(aes(x = n_groups, y = value, col = type)) +
  geom_line() +
  geom_point()
 
  