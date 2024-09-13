# eco-phys a-ci curves

# packages ####
library(tidyverse)
library(plantecophys)


# data ####
# brings in a lot of NAs for some reason
df <- data_for_aci_cruves %>% 
  dplyr::select(plant, a, ci) %>% 
  rename(Photo = a, 
         Ci = ci) %>% 
  mutate(plant = as.factor(plant))
df <- as.data.frame(df)

# fit some curves
huh <- manyacidat %>% 
  dplyr::select(Curve, Ci, Photo)
?fitacis

obs <- fitacis(df, 'plant', fitmethod = 'bilinear', id = 'plant')
coef(obs)
plot(obs, how = 'oneplot', colour_by_id = TRUE, id_legend = TRUE)


test <- fitacis(huh, 'Curve', fitmethod = 'bilinear')

plot(test, how = 'oneplot')


#####
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









