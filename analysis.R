library(ggplot2)
load("rda/data.goteo.rda")
data_goteo %>% filter(Amount < 500000) %>% mutate(Amount = log2(Amount)) %>% ggplot(aes(Population_cat, Amount)) + geom_boxplot()
View(data_goteo) 
ggsave("figs/boxplot_popcat_amount.png")

#Categorical and numerical variables (data frames)
Goteo_categorical <- data_goteo[, sapply(data_goteo, is.character)]
Goteo_numerical <- data_goteo[, !sapply(data_goteo, is.character)]
data_goteo %>% summary()
#Percentages
percentages_sector <- table(Goteo_categorical$Sector, useNA = "always") %>% 
  prop.table()*100
data_goteo %>% count(Sector) %>% mutate(pct_sector = prop.table(n))

#Plots
#Sector
plot_sector <- data_goteo %>%
  count(Sector) %>%
  mutate(pct_sector = prop.table(n)) %>%
  ggplot(aes(x = Sector, y = pct_sector, fill = Sector, label = scales::percent(pct_sector))) +
  geom_col(position = 'dodge', width = 0.5) +
  ggtitle("Crowdfunding Rural por Sector") +
  scale_x_discrete(labels = c("Agricultura", "Industria", "Servicios")) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none", axis.title.y = element_blank(), panel.background = element_blank()) +
  geom_text(position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  geom_hline(yintercept = c(0.2, 0.4), linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, color = "black")

scale_fill_manual(values = c("Agricultura" = "lightgreen", "Industria" = "red", "Servicios" = "blue"))#no funciona, revisar
#Plot Tipo de población
breakpoints <- c(0, 1000, 5000, 10000, 20000, 100000, 6000000)
labels <- c("rural_1", "rural_2", "rural_3", "rurb", "urb_1", "urb_2")
data_goteo <- data_goteo %>% mutate(Population_cat = cut(Population_n, breakpoints, labels = labels))

plot_tipo_poblacion <- data_goteo %>%
  count(Population_cat) %>%
  mutate(pct_rur_urb = prop.table(n)) %>%
  ggplot(aes(x = Population_cat, y = pct_rur_urb, fill = Population_cat, label = scales::percent(pct_rur_urb))) +
  geom_col(position = 'dodge', width = 0.5) +
  ggtitle("Crowdfunding Rural por tipo de Población") +
  scale_x_discrete(labels = c("Rural (< 1 mil)", "Rural (1-5 mil)", "Rural (5-10mil)", "Rurbano (10-20mil)", "Urbano (20-100mil)", "Urbano (> 100 mil)")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete() +
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  geom_text(position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  geom_hline(yintercept = c(0.2, 0.4), linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, color = "black")

print(plot_tipo_poblacion)

#Inversores
data_goteo %>% rename("Investors" = "Nº Investors") %>% group_by(Population_cat) %>%
  summarise(mean = mean(Investors))

data_goteo %>% rename("Investors" = "Nº Investors") %>% filter(Investors < 400) %>%
  ggplot(aes(x = Population_cat, y = Investors, fill = Population_cat)) + geom_boxplot() +
  ggtitle("Inversora/es por tipo de Población") +
  scale_x_discrete(labels = c("Rural (< 1 mil)", "Rural (1-5 mil)", "Rural (5-10mil)", "Rurbano (10-20mil)", "Urbano (20-100mil)", "Urbano (> 100 mil)")) +
  scale_fill_discrete() +
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  geom_hline(yintercept = c(0.2, 0.4), linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, color = "black")

?summarise
?geom_text
