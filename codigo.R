library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(RColorBrewer)

train_data <- read.csv("train.csv")
train <- train_data

#Una primera mirada al dataset
view(train)
glimpse(train)
summary(train)
sapply(train, function(x) sum(is.na(x)))

#Saco la columna Id ya que no me aporto nada para el EDA
train$Id <- NULL

#Cambio estas columnas para que aparezcan como variables categoricas
train$OverallQual <- as.character(train$OverallQual)
train$OverallCond <- as.character(train$OverallCond)


# Mostrar las medidas de tendencia central de la variable SalePrice
sale_price_mode <- names(table(train$SalePrice)[table(train$SalePrice) == max(table(train$SalePrice))])
sale_price_summary <- data.frame(
  measure = c("Mean", "Median", "Mode"),
  value = c(mean(train$SalePrice), median(train$SalePrice), as.numeric(sale_price_mode))
)
ggplot(sale_price_summary, aes(x = measure, y = value)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
  ggtitle("Medidas de tendencia central de SalePrice")

#Saco los outliers de precio de ventas para que graficar su distribucion 
Q1 <- quantile(train$SalePrice, 0.25)
Q3 <- quantile(train$SalePrice, 0.75)
IQR <- Q3 - Q1
lim_inf <- Q1 - 1.5*IQR
lim_sup <- Q3 + 1.5*IQR
train_filtered <- train %>% filter(SalePrice >= lim_inf & SalePrice <= lim_sup)

ggplot(train_filtered, aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000, fill = "#69b3a2", color = "#e9ecef") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Precio de venta", y = "Frecuencia", 
       title = "Distribución de los precios de las casas") +
  theme_minimal()


#Analisis de correlación de las variables cuantitativa con el precio de venta de las casas
df_num = train %>% select_if(is.numeric)
glimpse(df_num)
GGally::ggcorr(
  df_num, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

#Analisis de correlación de las variables numéricas entre ellas
cor_matrix = cor(df_num, method="spearman", use="pairwise")
cor_matrix[upper.tri(cor_matrix, diag=T)] = NA
df_cor = cor_matrix %>% as.table() %>% as.data.frame()
df_cor %>% 
  rename(corr = Freq) %>% 
  filter(!is.na(corr) & Var1 != Var2) %>% 
  arrange(-abs(corr)) %>% 
  knitr::kable() %>%
  kableExtra::kable_styling()

#Relación entre el año de construcción y el precio de la casa
#como comprende un período muy garnde, lo divido por decadas

table_yearbuilt <- data.frame(
  Min = min(train$YearBuilt),
  Mean = mean(train$YearBuilt),
  Median = median(train$YearBuilt),
  Max = max(train$YearBuilt),
  Variance = var(train$YearBuilt)
)

train$Decade <- cut(train$YearBuilt, breaks = seq(1870, 2010, by = 10), 
                    labels = paste0(seq(1870, 2000, by = 10), "-", seq(1880, 2010, by = 10)))

ggplot(train, aes(x = Decade, y = SalePrice)) + 
  geom_bar(stat = "summary", fun = mean, fill = "#0072B2") +
  labs(title = "Promedio de Precio por Década de Construcción", 
       x = "Década de Construcción", y = "Precio de Venta Promedio") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(train, aes(x = Decade, y = SalePrice, fill="red")) +
  geom_boxplot() +
  labs(x = "Década de construcción", y = "Precio de venta")

ggplot(train, aes(x = YearBuilt, y = SalePrice, color = Decade)) +
  geom_point(alpha = 0.5) +
  labs(x = "Año de construcción", y = "Precio de venta", color = "Década de construcción") +
  scale_color_discrete(labels = c("1870s", "1880s", "1890s", "1900s", "1910s", "1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s","2010s"))

#Relación entre la capacidad del estacionamiento en autos y el precio de la casa

table_garage <- data.frame(
  Min = min(train$GarageCars),
  Mean = mean(train$GarageCars),
  Median = median(train$GarageCars),
  Max = max(train$GarageCars),
  Variance = var(train$GarageCars)
)

#uso el train sin los outliers de SalePrice
ggplot(train_filtered, aes(x = SalePrice, fill = as.factor(GarageCars))) +
  geom_density(alpha = 0.4) +
  xlab("SalePrice") +
  ylab("Densidad") +
  ggtitle("Densidad de SalePrice vs GarageCars")

ggplot(train, aes(x = as.factor(GarageCars), y = SalePrice, fill="red")) +
  geom_boxplot() +
  xlab("GarageCars") +
  ylab("SalePrice") +
  ggtitle("Boxplot de SalePrice vs GarageCars")

ggplot(train, aes(x = factor(GarageCars), y = SalePrice)) +
  stat_summary(fun.y = "mean", geom = "bar", fill = "lightblue") +
  labs(x = "GarageCars", y = "SalePrice")

ggplot(train, aes(x = as.factor(GarageCars), y = SalePrice)) +
  geom_violin(trim = FALSE) +
  xlab("GarageCars") +
  ylab("SalePrice") +
  ggtitle("Violinplot de SalePrice vs GarageCars")

#Relación entre el area en pies al cuadrado donde hay construccion y el precio de la casa

table_ground <- data.frame(
  Min = min(train$GrLivArea),
  Mean = mean(train$GrLivArea),
  Median = median(train$GrLivArea),
  Max = max(train$GrLivArea),
  Variance = var(train$GrLivArea)
)

ggplot(train, aes(x=GrLivArea, y=SalePrice)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlab("GrLivArea") + ylab("SalePrice") +
  ggtitle("Relación entre SalePrice y GrLivArea") +
  scale_y_continuous(limits = c(0, 800000))

mean_grlivarea <- mean(train$GrLivArea)
sd_grlivarea <- sd(train$GrLivArea)
train_filtered2 <- train[train$GrLivArea < mean_grlivarea + 3 * sd_grlivarea,]
ggplot(train_filtered2, aes(x = GrLivArea, fill = "red")) +
  geom_density(alpha = 0.5) +
  labs(x = "GrLivArea", y = "Density")

ggplot(train, aes(x = GrLivArea, y = ..density.., fill = cut_number(SalePrice, 10))) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis_d() +
  labs(x = "GrLivArea", y = "Density", fill = "SalePrice")


#Relación entre la cantidad de baños enteros y el precio de la casa

table_bath <- data.frame(
  Min = min(train$FullBath),
  Mean = mean(train$FullBath),
  Median = median(train$FullBath),
  Max = max(train$FullBath),
  Variance = var(train$FullBath)
)

ggplot(train_filtered, aes(x = SalePrice, fill = as.factor(FullBath))) +
  geom_density(alpha = 0.4) +
  xlab("SalePrice") +
  ylab("Densidad") +
  ggtitle("Densidad de SalePrice vs FullBath")

ggplot(train, aes(x = as.factor(FullBath), y = SalePrice, fill="red")) +
  geom_boxplot() +
  xlab("FullBath") +
  ylab("SalePrice") +
  ggtitle("Boxplot de SalePrice vs FullBath")

ggplot(train, aes(x = factor(FullBath), y = SalePrice)) +
  stat_summary(fun.y = "mean", geom = "bar", fill = "lightblue") +
  labs(x = "FullBath", y = "SalePrice")

ggplot(train, aes(x = as.factor(FullBath), y = SalePrice, fill="red")) +
  geom_violin(trim = FALSE) +
  xlab("FullBath") +
  ylab("SalePrice") +
  ggtitle("Violinplot de SalePrice vs FullBath")


#Armo un data set con las variables categoricas

df_cat = train %>% select_if(function(x) !is.numeric(x))
glimpse(df_cat)

#Pruebo la correlación con SalePrice de las variables que creo que podrian estar correlacionadas

# Relacion entre el precio de las casas y su calidad
anova_res = aov( train$SalePrice ~ train$OverallQual, train)
summary(anova_res)
anova_tab = anova_res %>% sjstats::anova_stats() 
anova_tab$omegasq[1]

train$OverallQual <- factor(train$OverallQual, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

ggplot(train, aes(x = OverallQual, fill="lightblue")) +
  geom_bar() +
  ggtitle("Distribución de OverallQual")

ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_bar(stat = "summary", fun = "mean", fill = "orange", color = "blue") +
  ggtitle("Promedio de SalePrice por OverallQual") +
  xlab("OverallQual") +
  ylab("SalePrice")

ggplot(train, aes(x = OverallQual, y = SalePrice, fill=factor(OverallQual))) +
  geom_boxplot() +
  ggtitle("Distribución del precio de las casas por su calidad") +
  xlab("OverallQual") +
  ylab("SalePrice")

ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_violin(fill = "lightblue", color = "blue", alpha = 0.5) +
  ggtitle("Distribución de SalePrice por OverallQual") +
  xlab("OverallQual") +
  ylab("SalePrice")

price_by_qual <- aggregate(SalePrice ~ OverallQual, data = train, FUN = mean)
ggplot(price_by_qual, aes(x = OverallQual, y = SalePrice)) +
  geom_line(color = "blue") +
  ggtitle("Media de SalePrice por OverallQual") +
  xlab("OverallQual") +
  ylab("SalePrice")

ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, color = "blue") +
  ggtitle("Relación entre OverallQual y SalePrice") +
  xlab("OverallQual") +
  ylab("SalePrice")


#Relacion entre Calidad de los materiales externos de la casa y el precio

anova_res2 = aov( train$SalePrice ~ train$ExterQual, train)
summary(anova_res2)
anova_tab2 = anova_res2 %>% sjstats::anova_stats() 
anova_tab2$omegasq[1]

ggplot(train, aes(x = ExterQual)) + 
  geom_bar(fill = "lightblue") +
  labs(x = "Calidad del material exterior", 
       y = "Frecuencia",
       title = "Distribución de la calidad del material exterior")

ggplot(train, aes(x = ExterQual, y = SalePrice, fill = ExterQual)) +
  geom_boxplot() +
  ggtitle("Relación entre ExterQual y SalePrice")

ggplot(train, aes(x = ExterQual, y = SalePrice)) +
  geom_jitter(width = 0.2, height = 0, color = "darkblue") +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  labs(x = "Calidad del material exterior",
       y = "Precio de venta",
       title = "Relación entre ExterQual y SalePrice (punto y jitter plot)")

ggplot(train, aes(x = ExterQual, y = SalePrice)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue", color = "black") +
  labs(x = "Calidad del material exterior",
       y = "Precio de venta",
       title = "Relación entre ExterQual y SalePrice (barplot)")

ggplot(train, aes(x = ExterQual, y = SalePrice)) +
  geom_violin(fill = "lightblue") +
  labs(x = "Calidad del material exterior",
       y = "Precio de venta",
       title = "Relación entre ExterQual y SalePrice (violin plot)")


#Relacion entre el barrio y el precio 

anova_res3 = aov( train$SalePrice ~ train$Neighborhood, train)
summary(anova_res3)
anova_tab3 = anova_res3 %>% sjstats::anova_stats() 
anova_tab3$omegasq[1]

top10_neighborhood <- train %>%
  count(Neighborhood) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  select(Neighborhood)

train %>%
  filter(Neighborhood %in% top10_neighborhood$Neighborhood) %>%
  ggplot(aes(x = Neighborhood, fill="orange")) +
  scale_fill_identity() +
  geom_bar() +
  ggtitle("Distribución de los 10 Neighborhood con más casas")

train %>%
  filter(Neighborhood %in% top10_neighborhood$Neighborhood) %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_bar(stat = "summary", fun = "mean", fill = "orange", color = "blue") +
  ggtitle("Promedio de SalePrice por barrio") +
  xlab("Barrio") +
  ylab("SalePrice")

train %>%
  filter(Neighborhood %in% top10_neighborhood$Neighborhood) %>%
  ggplot(aes(x=Neighborhood, y=SalePrice, fill="red")) +
  geom_boxplot() +
  ggtitle("Distribución de SalePrice por vecindario") +
  xlab("Vecindario") +
  ylab("SalePrice")


#Relación entre la calidad del sotano y el precio

anova_res4 = aov( train$SalePrice ~ train$BsmtQual, train)
summary(anova_res4)
anova_tab4 = anova_res4 %>% sjstats::anova_stats() 
anova_tab4$omegasq[1]

ggplot(train, aes(x=BsmtQual)) +
  geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Gráfico de barras de BsmtQual", x = "BsmtQual", y = "Frecuencia")

ggplot(train, aes(x=BsmtQual, y=SalePrice)) +
  geom_boxplot(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Boxplot de SalePrice por cada categoría de BsmtQual", x = "BsmtQual", y = "SalePrice")

#uso el train sin los outliers de SalePrice
ggplot(train_filtered, aes(x=SalePrice, fill=BsmtQual)) +
  geom_density(alpha=0.6) +
  labs(title = "Gráfico de densidad de SalePrice por cada categoría de BsmtQual", x = "SalePrice", y = "Densidad") +
  scale_fill_manual(values=c("#69b3a2","#404080","#803030","#CCCC00"))
