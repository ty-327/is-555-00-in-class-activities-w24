library(tidyverse)
library(dplyr)
library(ggplot2)


df1 <- read_csv('https://www.dropbox.com/s/df2w04r0c09kw3f/df_1.csv?dl=1')
df2 <- read_csv('https://www.dropbox.com/s/xfp1qzxvo19ym0x/df_2.csv?dl=1')
df3 <- read_csv('https://www.dropbox.com/s/uzusr9723ffn546/df_3.csv?dl=1')
df4 <- read_csv('https://www.dropbox.com/s/js8tehtsk7btpeq/df_4.csv?dl=1')

# A few sample calculations:
df1 %>% summarize(across(everything(), list(mean = ~mean(.x), 
                                            sd = ~sd(.x))))

# correlation matrix:
df1 %>% cor()

# simple regression analysis:
lm(y ~ x, data = df1) %>% coefficients()

bind_rows(df1, df2, df3, df4, .id = 'df') %>% 
  ggplot(aes(x = x, y = y, color = df)) +
  geom_point(size = 1) +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~df)


df_final <- rbind(df1, df2, df3, df4)

df_final %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()


df1 %>% 
  summarize(across(everything(), list(mean = ~mean(.x), sd = ~sd(.x))))
df2 %>% 
  summarize(across(everything(), list(mean = ~mean(.x), sd = ~sd(.x))))
df3 %>% 
  summarize(across(everything(), list(mean = ~mean(.x), sd = ~sd(.x))))
df4 %>% 
  summarize(across(everything(), list(mean = ~mean(.x), sd = ~sd(.x))))

df1 %>% cor()
df2 %>% cor()
df3 %>% cor()
df4 %>% cor()

df1 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

df2 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

df3 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

df4 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
