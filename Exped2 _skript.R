library(ggplot2)
library(readr)

#load in data frame, with max_buds, and open_buds as integer, treatment as factor(1 = 0°, 2= 6°, 3 = 12°, 4,5 to be defined), tree_id as factor (1:36, 100:119)
data_exped2_factorized2 <- read_delim("data_exped2_factorized2.csv",";", escape_double = FALSE, col_types = cols(max_buds = col_integer(),open_buds = col_integer(), treatment = col_factor(levels = c("1","2", "3", "4", "5")), tree_id = col_factor(levels = c("1","2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12", "13","14", "15", "16", "17", "18","19", "20", "21", "22", "23","24", "25", "26", "27", "28","29", "30","34", "35", "36", "100", "101","102", "103", "104", "105", "106","107", "108", "109", "110", "111","112", "113", "114", "115", "116","117", "118", "119"))), trim_ws = TRUE)

# Set 9999 to be NA value
df <- data_exped2_factorized2
df[df == "9999"] <- NA

# transform df$date (character) in a dateformat
as.Date(df$date, "%d.%m.%Y")
str(df)

ggplot(data= df, aes(x = date , y = open_buds))+
  geom_line(aes(df$tree_id, color = "red"))
par(c(1,1,1,1))

plot(df$open_buds ~ df$date) 

