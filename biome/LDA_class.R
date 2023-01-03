#test LDA for biome assignments
rm(list = ls())

library(MASS)
library(klaR)
library(psych)
library(ggord)
library(devtools)

df_full <- read.csv("C:/Users/lschild/Documents/pollen/bistab/biome/biome_modern.csv")%>%
  filter(!(BIOME_NAME %in% c("Mangroves", "Deserts & Xeric Shrublands",
                             "Tropical & Subtropical Coniferous Forests",
                             "Tropical & Subtropical Moist Broadleaf Forests",
                             "Flooded Grasslands & Savannas")))%>%
  mutate(BIOME_NAME = ifelse(BIOME_NAME %in% c("Boreal Forests/Taiga","Tundra"),
                             BIOME_NAME,
                             "other"))
df_full$BIOME_NAME <- as.factor(df_full$BIOME_NAME)

head(df)

#training data set
sub <- sample(nrow(df),
              round(nrow(df)*0.7))
df <- df_full[sub,]

pairs.panels(df[,2:6],
             gap  =0,
             bg = c("red", "green", "blue")[as.factor(df$BIOME_NAME)],
             pc = 21)
df[,2:6] <- sqrt(df[,2:6])

linear <- lda(BIOME_NAME ~.,
              data = df[,-1])
linear
attributes(linear)
p <- predict(linear, df[,-1])
ldahist(data = p$x[,1],
        g = df$BIOME_NAME)
ldahist(data = p$x[,2],
        g = df$BIOME_NAME)
ggord(linear,
      df$BIOME_NAME)

partimat(BIOME_NAME ~.,
         data = df[sample(nrow(df),200),-1],
         method = "lda")

####confusion matrix ####
p1 <- predict(linear,df)$class
tab <- table(Predicted = p1,
             Actual = df$BIOME_NAME)
tab
sum(diag(tab))/sum(tab)

p2 <- predict(linear, df[-sub,])$class
tab1 <- table(Predicted = p2,
              Actual = df[-sub,7])
tab1
sum(diag(tab1))/sum(tab1)
