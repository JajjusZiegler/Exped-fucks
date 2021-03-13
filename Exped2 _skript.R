
df = data_exped2_factorized %>% select(-5:-38)

write.csv(df, "C:\\Users\\Janus\\Desktop\\UNI LENC\\WS20_21\\Exped 2\\Exped-fucks\\df_long.csv", row.names=FALSE)

data[data == "9999"] <- NA
