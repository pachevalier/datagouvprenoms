library("readr")
library("dplyr")
library("ggplot2")
library("ggthemes")
read_tsv(file = "nat2015.txt") %>%
  glimpse()

table_prenoms <- read_tsv(file = "nat2015_utf8.txt") %>%
  group_by(sexe, preusuel) %>%
  summarise(
    n = sum(nombre) 
    ) %>%
  ungroup() %>%
  group_by(sexe) %>%
  mutate(share = 100 * n / sum(n)) %>%
  filter(preusuel != "_PRENOMS_RARES") 

png("prenoms_1.png", width = 420, height = 340)
table_prenoms %>%
  filter(sexe == 1) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(preusuel, n), y = share)
    ) + 
  theme_fivethirtyeight() + 
  scale_y_continuous(name = "Pourcentage", labels = function(x) {paste(x, "%")}) +
  coord_flip() + 
  labs(main = "Proportion des prénoms masculins dans les naissances depuis 1900")
dev.off()

png("prenoms_2.png", width = 420, height = 340)
table_prenoms %>%
  filter(sexe == 2) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(preusuel, n), y = share)
  ) + 
  theme_fivethirtyeight() + 
  scale_y_continuous(name = "Pourcentage", labels = function(x) {paste(x, "%")}) +
  coord_flip() + 
  labs(main = "Proportion des prénoms féminins dans les naissances depuis 1900")
dev.off()


