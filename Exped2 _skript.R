library(ggplot2)


df <- data_exped2_factorized2
df[df == "9999"] <- NA

str(df)

#ggplot(data= df, aes(x = date , y = open_buds))+
  #geom_line(aes(df$tree_id == "2", color = "red"))?lm
par(c(1,1,1,1))

plot(df$open_buds ~ df$date) 

