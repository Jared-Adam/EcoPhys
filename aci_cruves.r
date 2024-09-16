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
  mutate(plant = as.factor(plant)) %>% 
  mutate(plant = case_when(plant =='lily' ~ 'Lily',
                           plant == 'hosta' ~ 'Hosta',
                           .default = as_factor(plant)))
?case_when
# fitacis() needs a data frame object
df <- as.data.frame(df)
?fct_relabel
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
  geom_smooth(se = TRUE)+
  labs(title = 'A-Ci Curves')+
  xlab(bquote(C[i]~ppm))+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    title = element_text(size = 24),
    legend.text = element_text(size =20)
  )

# part 2 ####

df2 <- data_for_individual_plots %>% 
  mutate_at(1:2, as_factor) %>% 
  dplyr::select(-unit) %>% 
  rename(plant = lily) %>% 
  mutate(plant = case_when(plant == 'lily' ~ 'Lily',
                           .default = as_factor(plant)))

unique(df2$var)

ggplot(filter(df2, var == 'Vcmax'), aes(y=val, x = plant, color = plant))+
    geom_point(size = 10)+
  labs(title = 'Vcmax ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))
       

ggplot(filter(df2, var == 'J'), aes(y=val, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Jmax ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(filter(df2, var == 'TPU'), aes(y=val, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'TPU ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(filter(df2, var == 'Rd'), aes(y=val, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Rd ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(filter(df2, var == 'Gamma'), aes(y=val, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Gamma ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(filter(df2, var == 'Amax'), aes(y=val, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Amax ~ Plant',
       x = "Plant")+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))
