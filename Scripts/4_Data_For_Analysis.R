#Read zip data generated in scripts 3 for USA and Europe
#Bind Europa and USA data, last step before analysis!
europe <- data.frame(fread("Data/Europe_data/all_above_50_europe.csv.gz"))
usa <- data.frame(fread("Data/Usa_data/all_above_50_usa.csv.gz"))
#Bind data
final_data <- rbind(europe, usa)

#Make all cols with first capital letter
colnames(final_data) <- str_to_title(colnames(final_data))

#Save data again
write.csv(final_data, file=gzfile("Data/final_data.csv.gz"), row.names=FALSE)
#MA, this would read as follows:
final_data <- data.frame(fread("Data/final_data.csv.gz"))
