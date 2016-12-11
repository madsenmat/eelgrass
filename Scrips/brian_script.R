invert_all <- read_csv("eelgrasscompiled.csv")
invert_all$date <- as.Date(invert_all$date, format = "%d-%h-%y")
invert_all_comp <- invert_all %>%
  filter(comp == 1)

invert_all_comp <- invert_all_comp %>% 
  mutate(eel = ifelse(eel == 1, "Eelgrass", eel)) 

invert_all_comp <- invert_all_comp %>% 
  mutate(eel = ifelse(eel == 0, "NonEelgrass", eel)) 

invert_all_comp <- rename(invert_all_comp, Habitat = eel)

a <- summarise(group_by(invert_all_comp, region, Habitat, orgnsm), sumcount =sum(count))



a$regions <- paste(a$region, a$Habitat)

a[, "regions"] <- a$regions

a <- subset(a, select = -c(region,Habitat) )


#make second to check
b <- a 
#log values into new nue

a.log <- log(a[,2])

#insert values in 
b[,2] <- a.log

a <- a %>% 
  spread(regions, sumcount)

b <- b %>% 
  spread(regions, sumcount)

#b <- transpose(b)



write_csv(b, "fresh_invert_eel_log.csv")
write_csv(a, "fresh_invert_eel.csv")


