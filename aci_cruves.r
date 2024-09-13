# eco-phys a-ci curves

# packages ####
library(tidyverse)
library(plantecophys)

# data ####
# brings in a lot of NA columns for some reason, so I am selecting the columns I want
# this df is composed of the plant name, A and ci columns
df <- data_for_aci_cruves %>% 
  dplyr::select(plant, a, ci) %>% 
  rename(Photo = a, 
         Ci = ci) %>% 
  mutate(plant = as.factor(plant))
# fitacis() needs a data frame object
df <- as.data.frame(df)

# test data set ####
# this df comes with the package
# I matched my variable names based on what this df had
manyacidat

test_df <- manyacidat %>% 
  dplyr::select(Curve, Ci, Photo)

test <- fitacis(test_df, 'Curve', fitmethod = 'bilinear')

plot(test, how = 'oneplot')

# my data now ####

obs <- fitacis(df, 'plant', fitmethod = 'bilinear', id = 'plant')
# coef() will output the values calculated in excel
coef(obs)
# plot() the object created above
plot(obs, how = 'oneplot', colour_by_id = TRUE, id_legend = TRUE)


# alterantive ggplot visual option ####
ggplot(df, aes(x = Ci, y = Photo, color = plant))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(title = 'A-Ci Curves')+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 22),
    title = element_text(size = 24)
  )









