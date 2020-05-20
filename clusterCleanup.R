data_orig = read.csv("data-final.csv", head = TRUE, quote="", sep = "\t")
data = data_orig[complete.cases(data_orig),]
data = data %>% add_count(country, sort = T, name = 'total_country') %>% 
  filter(total_country > 1000)

data[,1:50] = apply(data[,1:50], 2, function(x) as.numeric(as.character(x)))

data = data[complete.cases(data),]

data = data %>% transmute(COUNTRY = country,
                          EXTROVERSION = EXT1 + EXT2 + EXT3 - EXT4 + EXT5 + EXT6 + EXT7 - EXT8 + EXT9 - EXT10,
                          AGREEABLENESS = -AGR1 + AGR2 - AGR3 + AGR4 - AGR5 + AGR6 - AGR7 + AGR8 + AGR9 + AGR10,
                          OPENNES = OPN1 - OPN2 + OPN3 - OPN4 + OPN5 - OPN6 + OPN7 + OPN8 + OPN9 + OPN10,
                          CONCIENTIOUSNESS = CSN1 - CSN2 + CSN3 - CSN4 + CSN5 - CSN6 + CSN7 - CSN8 + CSN9 + CSN10,
                          NEUROTICISM = EST1 - EST2 + EST3 - EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10) %>% 
                filter(COUNTRY != 'NONE')

avg_data = data %>% group_by(COUNTRY) %>% summarise(avgEX = mean(EXTROVERSION),
                              avgAG = mean(AGREEABLENESS),
                              avgOP = mean(OPENNES),
                              avgCO = mean(CONCIENTIOUSNESS),
                              avgNE = mean(NEUROTICISM))

write.csv(avg_data, 'data-scored-avg-1000.csv', row.names = F)

