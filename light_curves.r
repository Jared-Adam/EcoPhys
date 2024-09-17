# packages ####
library(plantecophys)
library(tidyverse)

# all on one data ####
df1 <- data_for_light_curves_and_solutiosn %>% 
  mutate(plant = as_factor(plant)) %>% 
  mutate(plant = case_when(plant == 'hosta' ~'Hosta',
                           plant == 'lily' ~'Lily',
                           plant == 'pial' ~ 'PIAL',
                           plant == 'pifl' ~ 'PIFL',
                           plant == 'pilo' ~ 'PILO'))

ggplot(df1, aes(x = Qin, y = A, color = plant))+
  geom_point()+
  geom_smooth(se = TRUE)+
  theme_bw()+
  labs(title = 'Light Response Curves ~ Five Plant Species')+
  ylab(bquote(A[net]~(Photosynthesis)~mu~mol~m^-2~s^-1))+
  xlab(bquote(Q~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    title = element_text(size = 24),
    legend.text = element_text(size =20)
  )



# the solution points for comparing data ####
df2 <- solutions_from_l2 %>% 
  na.omit() %>% 
  mutate(plant = as_factor(plant))%>% 
  mutate(plant = case_when(plant == 'hosta' ~'Hosta',
                           plant == 'lily' ~'Lily',
                           plant == 'pial' ~ 'PIAL',
                           plant == 'pifl' ~ 'PIFL',
                           plant == 'pilo' ~ 'PILO'))

colnames(df2)

ggplot(df2, aes(y=Rdark, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Rdark ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(R[dark]~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(df2, aes(y=theta, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Theta ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(Theta))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(df2, aes(y=asat, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Asat ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(A[sat]~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(df2, aes(y=LCP, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'LCP ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(LCP~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(df2, aes(y=q, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Q ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(Q~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))

ggplot(df2, aes(y=qsat, x = plant, color = plant))+
  geom_point(size = 10)+
  labs(title = 'Qsat ~ Plant (n=5)',
       x = "Plant")+
  ylab(bquote(Q[sat]~mu~mol~m^-2~s^-1))+
  scale_color_discrete(name = 'Plant name')+
  theme_bw()+
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 30),
        title = element_text(size = 30),
        legend.text = element_text(size = 24))
