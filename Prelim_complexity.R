library(tidyverse)
setwd("datasets/gop_frags/")
files <- list.files()
data <- map(files,function(x) read_csv(x))
gop_data <- map2(files,data, function(x,y) cbind(x,y))
gop_df <- do.call(rbind,gop_data)
names(gop_df)[1] <- "date"
df1 <- gop_df %>%
  separate(date,"date",sep = "\\.") %>%
  separate(text, "speaker", sep = ":", remove = FALSE) %>%
  mutate(text_length = nchar(text))
df2 <- df1 %>%
  group_by(speaker) %>%
  summarise(talking_turns = n(),
            total_length = sum(text_length),
            ave_length = mean(text_length)) %>%
  pivot_longer(-speaker,names_to = "variable", values_to = "value")
