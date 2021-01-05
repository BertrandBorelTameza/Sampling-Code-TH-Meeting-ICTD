#############################################################################################################################################
# Purpose:                Data cleaning and Report Writing: This Code produces graphs to enrich the report                                  #
# Author:                 Bertrand T. Tameza                                                                                                #
# R version               3.5.1 (2018-07-02)                                                                                                #
#############################################################################################################################################



# load the necessary packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
#install.packages("xlsx")
library(xlsx)
library(writexl)
library(ggplot2)
library(bbplot)
#install.packages("scales")
library(scales)

# Set worksplace
setwd("C:/Users/Bertrand Borel/Desktop/Tax_Report")

# loading the datasets: Same data collection conducted by 7 teams in the field
team1 = read_xlsx("TaxTeam1.xlsx")
team2 = read_xlsx("TaxTeam2.xlsx")
team3 = read_xlsx("TaxTeam3.xlsx")
team4 = read_xlsx("TaxTeam4.xlsx")
team5 = read_xlsx("TaxTeam5.xlsx")
team6 = read_xlsx("TaxTeam6.xlsx")
team7 = read_xlsx("TaxTeam7.xlsx")

# Data structure convertion: From 'list' to 'dataframe'
# Datatype conversion
team11 = as.data.frame(team1)
team22 = as.data.frame(team2)
team33 = as.data.frame(team3)
team44 = as.data.frame(team4)
team55 = as.data.frame(team5)
team66 = as.data.frame(team6)
team77 = as.data.frame(team7)

# Check if the datasets have the same headers
check_name = function(x,y){
  for (i in names(x)){
    if (!(i %in% names(y))){
      print("Warning: Columns names are not the same")
      print(x$i)
      print(y$i)
      break
    }
    else if(i == tail(names(y), n = 1 )){
      print('Names are identical')
    }
  }
}

check_name(team11,team22)

# Concatenation of the datasets (number of comumns and Columns' labels must be the same )
# Fist stage
intermediate1 = rbind(team11, team22)
intermediate2 = rbind(team33, team44)
intermediate3 = rbind(team55, team66)

# Second stage
intermediate1_ = rbind(intermediate1, intermediate2)
intermeduate2_ = rbind(intermediate1_, intermediate3)

# Final stage: Concatenation of the 7 datasets
final_data = rbind(intermeduate2_, team77)
final_data = as.data.frame(final_data)
size = nrow(final_data)


# Interviews conducted: 
# Prepare the data: The table() function counts the occurences of a value in a variable
interview_status <- final_data %>%
  select(Consent, property_owner_located, property_located)
status_col1_value = c("Co", "N Co","O L", "O N L", "P L", "P N L")
status_col2_value = c(table(interview_status$Consent)[2], table(interview_status$Consent)[1],
                      table(interview_status$property_owner_located)[2], table(interview_status$property_owner_located)[1],
                      table(interview_status$property_located)[2], table(interview_status$property_located)[1])
status_percent = percent(status_col2_value/size)
interview_status_df <- data.frame(status = status_col1_value, occurence = status_col2_value, status_percent)
  
#Make the plot
bars_consent <- ggplot(interview_status_df, aes(x = interview_status_df$status, y = interview_status_df$occurence)) +
  geom_bar(stat="identity",  position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + bbc_style() +
  labs(title="Interviews Status", subtitle = "Successful Interviews vs unsuccessful interviews", color = "Legend") + 
  geom_text(aes(label = status_percent), size = 3, color = "black", position = position_dodge(width = 0.9),
            vjust = -2) 
# show the plot
bars_consent

# save the plot
finalise_plot(plot_name = bars_consent,
              source = "Source: Baseline Tax Survey ",
              save_filepath = "C:/Users/Bertrand Borel/Desktop/Tax_Report/figs/Interviews_consented.png",
              width_pixels = 640,
              height_pixels = 550)

# I remove the NA for further exploration to restrict the analysis to consented interviews
consented_interviews = filter(final_data, Consent == "Yes")

# demographic background of the respondents: 
# Age
consented_interviews$resp_age = as.numeric(consented_interviews$resp_age)
Summary_age = summary(consented_interviews$resp_age)
Summary_age

# Distribution of the properties visited  in Freetown
HH_location <- consented_interviews %>%
  select(Area)
location_col1_value = c("Western", "Central", "Eastern")
location_col2_value = c(table(HH_location$Area)[3],table(HH_location$Area)[1], table(HH_location$Area)[2])
area_percent = percent(location_col2_value/sum(location_col2_value))
HH_location_df <- data.frame(area = location_col1_value, occurence = location_col2_value, proportion = area_percent)

#Make the plot
bars_area <- ggplot(HH_location_df, aes(x = HH_location_df$area, y = HH_location_df$occurence)) +
  geom_bar(stat="identity",  position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + bbc_style() +
  labs(title="Properties' Distribution in Freetown", subtitle = "Number of visited Properties per Area") + 
  geom_text(aes(label = proportion), size = 3, color = "black", position = position_dodge(width = 0.9),
            vjust = -2)

# show the plot
bars_area

# save the plot
finalise_plot(plot_name = bars_area,
              source = "Source: Baseline Tax Survey ",
              save_filepath = "C:/Users/Bertrand Borel/Desktop/Tax_Report/figs/property_distribution.png",
              width_pixels = 640,
              height_pixels = 550)

# Regular Tax Payers: The number of the respondents who received an RDN last year against those who did not.

RDN <- consented_interviews %>% select(RDN_received_last_year.)
RDN_col1_value = c("Received", "Not Received", "Don't Remember")
RDN_col2_value = c(table(RDN$RDN_received_last_year.)[3],table(RDN$RDN_received_last_year.)[2],
                      table(RDN$RDN_received_last_year.)[1])
RDN_percent = percent(RDN_col2_value/sum(RDN_col2_value))
RDN_df <- data.frame(rdn_received = RDN_col1_value, occurence = RDN_col2_value, prop = RDN_percent)

#Make the plot
bars_rdn <- ggplot(RDN_df, aes(x = RDN_df$rdn_received, y = RDN_df$occurence)) +
  geom_bar(stat="identity",  position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + bbc_style() +
  labs(title="RDN reception in 2019", subtitle = "Most of the participants did not receive an RDN") + 
  geom_text(aes(label = prop), size = 3, color = "black", position = position_dodge(width = 0.9),
            vjust = -2)

# show the plot
bars_rdn

# save the plot
finalise_plot(plot_name = bars_rdn,
              source = "Source: Baseline Tax Survey ",
              save_filepath = "C:/Users/Bertrand Borel/Desktop/Tax_Report/figs/rdn_reception.png",
              width_pixels = 640,
              height_pixels = 550)

### Willingness to pay taxes

# Citizen should pay taxes
should_pay <- consented_interviews %>%
  select(citizen_should_pay)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(should_pay$citizen_should_pay)[1],table(should_pay$citizen_should_pay)[2], table(should_pay$citizen_should_pay)[3],
                        table(should_pay$citizen_should_pay)[4], table(should_pay$citizen_should_pay)[5], table(should_pay$citizen_should_pay)[6])
modolities_percent = percent(value/sum(value))
should_pay_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

#Make the plot
bars_citizen_should_pay <- ggplot(should_pay_df, aes(x = should_pay_df$values, y = should_pay_df$occurence)) +
  geom_bar(stat="identity",  position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + bbc_style() +
  labs(title="Willingness to pay taxes", subtitle = "The majority agrees that they should normally pax their taxis") + 
  geom_text(aes(label = proportion), size = 3, color = "black", position = position_dodge(width = 0.9),
            vjust = -2)

# show the plot
bars_citizen_should_pay

# Citizen should pay taxes if the government represent their interests 
should_pay_conditional <- consented_interviews %>%
  select(citizen_pay_conditional)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(should_pay_conditional$citizen_pay_conditional)[1],table(should_pay_conditional$citizen_pay_conditional)[2], table(should_pay_conditional$citizen_pay_conditional)[3],
          table(should_pay_conditional$citizen_pay_conditional)[4],table(should_pay_conditional$citizen_pay_conditional)[5], table(should_pay_conditional$citizen_pay_conditional)[6])
modolities_percent = percent(value/sum(value))
should_pay_conditional_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
should_pay_conditional_df

# Citizen should pay taxes if their neighbors could see them paying their taxes 
pay_if_neighbor_sees <- consented_interviews %>%
  select(citizen_pay_if_public)
modalities_col1_value = c("Completely agree", "Completely disagree", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(pay_if_neighbor_sees$citizen_pay_if_public)[1],table(pay_if_neighbor_sees$citizen_pay_if_public)[2], table(pay_if_neighbor_sees$citizen_pay_if_public)[3],
          table(pay_if_neighbor_sees$citizen_pay_if_public)[4], table(pay_if_neighbor_sees$citizen_pay_if_public)[5])
modolities_percent = percent(value/sum(value))
pay_if_public_notice_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
pay_if_public_notice_df

# Citizen should pay taxes if other people are also paying 
pay_if_horizontal_equity <- consented_interviews %>%
  select(horizontal_equity)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(pay_if_horizontal_equity$horizontal_equity)[1],table(pay_if_horizontal_equity$horizontal_equity)[2], table(pay_if_horizontal_equity$horizontal_equity)[3],
          table(pay_if_horizontal_equity$horizontal_equity)[4], table(pay_if_horizontal_equity$horizontal_equity)[5], table(pay_if_horizontal_equity$horizontal_equity)[6])
modolities_percent = percent(value/sum(value))
pay_if_horizontal_equity_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
pay_if_horizontal_equity_df

# Citizens should pay taxes if they see the FCC doing good things around the city even not in their own area  
pay_if_service <- consented_interviews %>%
  select(pay_if_services)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(pay_if_service$pay_if_services)[1],table(pay_if_service$pay_if_services)[2], table(pay_if_service$pay_if_services)[3],
          table(pay_if_service$pay_if_services)[4], table(pay_if_service$pay_if_services)[5], table(pay_if_service$pay_if_services)[6])
modolities_percent = percent(value/sum(value))
pay_if_service_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
pay_if_service_df

# Citizens should pay taxes if they see the FCC doing good things in their own area  where they lived
pay_if_service_in_their_area <- consented_interviews %>%
  select(pay_if_services_in_area)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(pay_if_service_in_their_area$pay_if_services_in_area)[1],table(pay_if_service_in_their_area$pay_if_services_in_area)[2], table(pay_if_service_in_their_area$pay_if_services_in_area)[3],
          table(pay_if_service_in_their_area$pay_if_services_in_area)[4], table(pay_if_service_in_their_area$pay_if_services_in_area)[5], table(pay_if_service_in_their_area$pay_if_services_in_area)[6])
modolities_percent = percent(value/sum(value))
pay_if_service_in_their_area_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
pay_if_service_in_their_area_df

# willingness to pay more taxes for improved services
pay_more_for_better_services <- consented_interviews %>%
  select(pay_more_if_better_services)
modalities_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
value = c(table(pay_more_for_better_services$pay_more_if_better_services)[1],table(pay_more_for_better_services$pay_more_if_better_services)[2], table(pay_more_for_better_services$pay_more_if_better_services)[3],
          table(pay_more_for_better_services$pay_more_if_better_services)[4], table(pay_more_for_better_services$pay_more_if_better_services)[5], table(pay_more_for_better_services$pay_more_if_better_services)[6])
modolities_percent = percent(value/sum(value))
pay_more_for_better_services_df <- data.frame(values = modalities_col1_value, occurence = value, proportion = modolities_percent)

# visualize the dataframe
pay_more_for_better_services_df

# The Tax system

# Prepare the data about the fairness of the system
coll_fairly <- consented_interviews %>%
  select(collect_fairly)
fairly_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
faily_col2_value = c(table(coll_fairly$collect_fairly)[1],table(coll_fairly$collect_fairly)[2], table(coll_fairly$collect_fairly)[3],
          table(coll_fairly$collect_fairly)[4], table(coll_fairly$collect_fairly)[5], table(coll_fairly$collect_fairly)[6])
fairly_percent = percent(faily_col2_value/sum(faily_col2_value))
coll_fairly_df <- data.frame(values = fairly_col1_value, occurence_fairness = faily_col2_value, proportion = fairly_percent)

# prepare the data about the accuracy of the system
coll_accurate <- consented_interviews %>%
  select(collect_accurately)
accuracy_col1_value = c("Completely agree", "Completely disagree", "Don't know", "In the middle", "Somewhat agree", "Somewhat disagree")
accuracy_col2_value = c(table(coll_accurate$collect_accurately)[1],table(coll_accurate$collect_accurately)[2], table(coll_accurate$collect_accurately)[3],
                     table(coll_accurate$collect_accurately)[4], table(coll_accurate$collect_accurately)[5], table(coll_accurate$collect_accurately)[6])
accuracy_percentage = percent(accuracy_col2_value/sum(accuracy_col2_value))
coll_accurate_df <- data.frame(values = accuracy_col1_value, occurence_accuracy = accuracy_col2_value, proportion = accuracy_percentage)

# the final dataframe
fairness_accuracy = data.frame(values = accuracy_col1_value, occ_fairness = coll_fairly_df$occurence_fairness, fainess_propotion = coll_fairly_df$proportion,
                               occ_accuracy = coll_accurate_df$occurence_accuracy, occ_propotion = coll_accurate_df$proportion)

fairness_accuracy


# The plot
modal = c(rep("C. Agree", 2), rep("C. disagree", 2), rep("D.K", 2), rep("middle", 2), rep("S.agree",2), rep("S. disagree", 2))
state = rep(c("fairly", "accurately"), 6)
value = c(table(coll_fairly$collect_fairly)[1], table(coll_accurate$collect_accurately)[1],table(coll_fairly$collect_fairly)[2], table(coll_accurate$collect_accurately)[2],
          table(coll_fairly$collect_fairly)[3], table(coll_accurate$collect_accurately)[3],table(coll_fairly$collect_fairly)[4], table(coll_accurate$collect_accurately)[4], 
          table(coll_fairly$collect_fairly)[5], table(coll_accurate$collect_accurately)[5],table(coll_fairly$collect_fairly)[6], table(coll_accurate$collect_accurately)[6])
prop = c(fairly_percent[1], accuracy_percentage[1], fairly_percent[2], accuracy_percentage[2], fairly_percent[3], accuracy_percentage[3],
         fairly_percent[4], accuracy_percentage[4], fairly_percent[5], accuracy_percentage[5], fairly_percent[6], accuracy_percentage[6])

fair_acc_df = data.frame(modal, state, value, prop)

fair_acc <- ggplot(fair_acc_df, aes(x = modal, y = value, fill = state)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  labs(title="Fairness and accuracy of the FCC tax system", subtitle = "Most of the participants believes that the tax system is fair and accurate") + 
  geom_text(aes(label = prop), size = 3, color = "black", position = position_dodge(width = 0.9),
            vjust = -2)

# show the plot
fair_acc

# save the plot
finalise_plot(plot_name = fair_acc,
              source = "Source: Baseline Tax Survey ",
              save_filepath = "C:/Users/Bertrand Borel/Desktop/Tax_Report/figs/fairness_accuracy_tax_system.png",
              width_pixels = 700,
              height_pixels = 590)


#################################                            End of the script                                    #############################################