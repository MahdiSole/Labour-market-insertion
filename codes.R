list.of.packages <- c("tidyverse", "descr", "broom")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

df <- read_delim(file = "Data/fr-esr-insertion_professionnelle-master.csv", delim = ";", col_names = TRUE, skip = 0, locale = locale(encoding = "UTF-8"))

View(df)
summary(df)


unique.values <- df %>% 
  select(poids_de_la_discipline:salaire_net_mensuel_regional_3eme_quartile) %>%
  unlist(.) %>%
  unique(.) %>%
  sort(.)

wrong.values <- c(".", "fe", "nd", "ns") 

df.clean <- df %>% 
  mutate_all(funs(ifelse(. %in% wrong.values, NA, .))) %>%
  mutate_at(vars(nombre_de_reponses:salaire_net_mensuel_regional_3eme_quartile), funs(as.numeric(.)))

summary(df.clean)

missing <- df.clean %>% select_if(is.numeric) %>% summarise_all(funs(mean(!is.na(.))))

mean.women <- mean(df.clean$femmes, na.rm=T)

df.graph <- df.clean %>% subset(situation == "30 mois après le diplôme")


plot1 <- ggplot(df.graph, aes(x=taux_dinsertion)) 
plot1

plot1 <- plot1 +
  geom_histogram()

plot1 <- plot1 +
  theme_classic() +
  labs(x = "Employment rates")

df.mean.empl<- df.graph %>% summarise(mean.empl = mean(taux_dinsertion, na.rm=T))

plot1 <- plot1 +
  geom_vline(data=df.mean.empl, aes(xintercept=mean.empl),
             linetype="dashed", size=1) 
plot1

df.graph.evol  <- df.graph %>% 
  subset(annee %in% c("2010","2014")) %>% 
  mutate_at(vars(annee), funs(factor(.)))

plot2 <- ggplot(df.graph.evol, aes(x=taux_dinsertion, fill = annee)) + 
  geom_histogram(binwidth=1, position="dodge") + 
  theme_classic() +
  labs(x = "Employment rates")

plot2 <- ggplot(df.graph.evol, aes(x=taux_dinsertion, fill = annee)) + 
  geom_histogram(binwidth=.5, alpha=.5, position="identity") + 
  theme_classic() +
  labs(x = "Employment rates")

mean.empl.year <- df.graph.evol %>% group_by(annee) %>% summarise(mean.empl = mean(taux_dinsertion, na.rm = T))

plot2 <- plot2 +
  geom_vline(data=mean.empl.year, aes(xintercept=mean.empl,  colour=annee),
             linetype="dashed", size=1) 

plot2

ggsave("Output/hist.empl.rate.pdf", width = 5, height = 5)

df.women <- df.clean %>% 
  subset(!is.na(femmes))
plot3 <- ggplot(df.women, aes(x=femmes, y = taux_dinsertion)) + 
  geom_point() + 
  theme_classic() 

plot3 <- ggplot(df.women, aes(x=femmes, y = taux_dinsertion)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_classic() 

plot3

df.women <- df.women %>%
  mutate(quartile.women = ntile(femmes, 4)) %>%
  mutate_at(vars(code_du_domaine), funs(factor(.,
                                               levels = c("DEG","LLA","MEEF","SHS","STS"),
                                               labels = c("Economics", "Humanities", "Teaching","Human Sciences","Sciences"))))


crosstab(df.women$quartile.women,df.women$code_du_domaine, prop.r =T, plot = F, cell.layout = F, dnn = c("Quartiles","Subjects"))

df.women %>% group_by(quartile.women) %>% summarise(n_universities = n(),
                                                    mean_empl = mean(taux_dinsertion, na.rm=T),
                                                    sd_empl = sd(taux_dinsertion, na.rm=T),
                                                    med_empl = median(taux_dinsertion, na.rm=T),
                                                    min_empl = min(taux_dinsertion, na.rm=T),
                                                    max_empl = max(taux_dinsertion, na.rm=T),
                                                    n_missing_empl = sum(is.na(taux_dinsertion)))





fit <- lm(taux_dinsertion ~ femmes, data = df.women)
fit$coefficients
summary(fit)
tidy(fit)


quartile1 <- df.women %>% subset(quartile.women ==1)
quartile4 <- df.women %>% subset(quartile.women ==4)


t.test(quartile1$taux_dinsertion, quartile4$taux_dinsertion)


chisq.test(df.women$academie, df.women$quartile.women)













































