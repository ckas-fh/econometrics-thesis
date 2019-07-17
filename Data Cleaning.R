############################################################
# data_setup.r
# Dec 12, 2018
############################################################
library(tidyverse)

setwd("U:/Thesis")

master = read_csv("Grant Disbursements.csv")
account = read_csv("V&A data.csv")
trade_open = read_csv("Trade Openness.csv")

master = master %>% 
  rename(Year=YEAR, Donor_Group=`Funder Type`,Donor=`Donor Name`, Recipient=`Country recipient`,DAH=`2015 Value (millions)`,Income_Group=`Income Group`)


master = master %>% 
  mutate(Recipient = recode(Recipient, "Viet nam" = "Vietnam",
                            "Viet Nam" = "Vietnam"))

relevant_data = master %>% 
  select(Donor_Group,Year,Donor,Recipient,Income_Group,DAH )

bilat_data = relevant_data %>% 
  filter(Donor_Group=="Bilaterals")

trade_open = trade_open %>% 
  rename(Recipient=`Country Name`,Recipient_Code=`Country Code`) %>% 
  select(Recipient,Recipient_Code,`2010`:`2015`)

trade_long=trade_open %>% 
  gather("Year", "Trade_Openness",`2010`:`2015`) %>% 
  arrange(Recipient,Year) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  mutate(Recipient=recode(Recipient, "Venezuela, RB"="Venezuela",
                          "Yemen, Rep." = "Yemen",
                          "Egypt, Arab Rep." = "Egypt"))

bilat_data2=left_join(bilat_data,trade_long)

# Look at what didn't merge (master)
bilat_data2 %>% 
  filter(is.na(Recipient_Code)) %>% 
  count(Recipient) %>% 
  print(n=Inf)

# Look at the countries in trade_long
trade_long %>% count(Recipient) %>% print(n=Inf)

# Keep cleaning!

# Only keep rows that merged with trade data
#bilat_data3 = bilat_data2 %>% 
# filter(!is.na(Recipient_Code))

