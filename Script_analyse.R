##############################################################################
##############################################################################
# Contexte : Production d'énergie électrique au sein de l'UE
# Auteurs : Bernadette BIAYE ; Ndeye Fatou DIAGNE ; 
# Diele-Eunice DIAVOU-DIAVOU ; Boukola GBAYE ; Karline OTCHOFFA

# Chargement de librairies
library(treemap)
library(ggplot2)
library(treemapify)
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
# Chargement des données
donnees = readRDS("Electricity_Power_UE.RDS")

glimpse(donnees)

#Thème pour les titres, axes et captions
theme_general = function(){
  theme(
    plot.title = element_text(face = "bold", 
                              size = 20,
                              hjust = 0.5),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    axis.title.y = element_text(face = "bold", 
                                size = 18),
    axis.title.x = element_text(face = "bold", 
                                size = 18),
    plot.caption = element_text(hjust = c(0, 1),
                                size = 10
                                ))
}


# Ajout d'une colonne de précision renouvelable, fossile, nucléaire, autre
# Définition des énergies renouvelables (pour le filtrage)
#renewable_types = c("Wind", "Geothermal", "Hydro", "Solar PV", 
#                    "Solar Thermal", "Tide, Wave and Ocean", "Other Sources")


donnees %>% 
  mutate(Category = ifelse(Type =="Combustible Fuels", "Fossil",
                           ifelse(Type == "Nuclear", "Nuclear",
                           ifelse(Type == "Other Sources", "Other", "Renewable"))
                           ))%>%
  filter( Year >= 1990 & Year <= 2020) -> donnees_cat

# ============================================================
# PROBLEMATIQUE 1
# ============================================================
# Top 10 des pays européens selon la quantité de
# production d’énergie renouvelable individuelle en 2020.

# On filtre pour récupérer les données sur les énérgies renouvelables en 2020
donnees_cat %>% 
  filter((Category == "Renewable") & (Year == 2020)) %>% 
  group_by(Country) %>% 
  summarize(Quantity = sum(Power)) %>% 
  arrange(desc(Quantity)) %>% 
  slice(1:10) %>% 
  mutate(Rang = 1:10) -> top10_en_ren
print(top10_en_ren)

# Couleur du texte
top10_en_ren <- top10_en_ren %>% 
  mutate(text_color = ifelse(Quantity > 100000, "white", "black"))


top10_en_ren %>% 
  ggplot(aes(area = Quantity, 
             fill = Quantity,
             label = paste0(Rang, ". ", Country, "\n", round(Quantity/1000, 2), " GW"))) +
  geom_treemap(colour = "white", size = 3) +
  geom_treemap_text(#colour = "black", 
                    aes(colour = text_color),
                    place = "centre", 
                    size = 20) + #, grow = TRUE
  scale_fill_gradient(
    low  = "#E8F5E8",  # vert très clair
    high = "#1B5E20"   # vert foncé
  ) +
  scale_color_identity() +
  labs(title = "Top 10 des pays européens selon 
  la production en énergie renouvelable en 2020",
       caption = c("BUT Science des Données", "Source : EUROSTAT"))+
  theme(legend.position = "none") +
  theme_general()

# ============================================================
# PROBLEMATIQUE 2
# ============================================================
# Top 5 des pays européens selon la
# part de production d’énergie éolienne individuelle parmi les énergies renouvelables en 2020

donnees_cat %>% 
  filter((Category == "Renewable")& (Year == 2020)) %>% 
  nest(data = !Country) -> result

# Fonction pour calculer la proportion d'énergie éolienne produite
prop_eol = function(tib){
  #tib est un tibble contenant les colonnes Type d'énergie, Year et Power pour un pays donné.
  tib %>% 
    mutate(Percent = round(100*Power/sum(Power), 2)) %>% # On crée la colonne des pourcentages
    filter(Type == "Wind") %>% 
    pull(Percent) -> eol
  return(eol)
}

# Tous les pays
donnees %>% 
  filter(!(Type  %in% c("Combustible Fuels", "Nuclear", "Other Sources")) 
         & Year == 2020) %>% 
  nest(data = !Country) %>% #regroupement des données par pays
  mutate(Prop_eol_sur_renouvelables = map_dbl(.x = data, 
                                              .f = prop_eol)) -> donnees_eol

# Réponse à la question
donnees_eol %>% 
  arrange(desc(Prop_eol_sur_renouvelables)) %>% 
  slice(1 : 5) %>% 
  ggplot(aes(x = reorder(Country, -Prop_eol_sur_renouvelables),
             y = Prop_eol_sur_renouvelables))+
  geom_bar(stat = "identity", 
           position = "dodge", 
           fill = "#66BB6A", 
           color = "black")+
  scale_y_continuous(limits = c(0, 100))+
  geom_text(aes(label = paste0(Prop_eol_sur_renouvelables, "%")),
                position = position_dodge(width = 0.9), 
            vjust = -0.3,
            size = 5) +
  labs(
    title = "Top 5 des pays de l'UE selon\n la part d'éolienne parmi les renouvelables en 2020",
    #"Top 5 des pays européens selon la part de production 
    #d’énergie éolienne individuelle parmi les énergies renouvelables en 2020",
    x = "Pays",
    y = "Pourcentage",
    caption = c("BUT Science des Données", "Source : EUROSTAT")
  )+
  theme_general()




# ============================================================
# PROBLEMATIQUE 3
# ============================================================


# Filtrer uniquement les énergies renouvelables
renewable <- donnees_cat %>%
  filter(Category == "Renewable")

print(renewable)

# Calcul du taux d'évolution par pays
Evolution <- renewable %>%
  group_by(Country) %>%  
  summarise(
    power_start = sum(Power[Year == min(Year)], na.rm = TRUE),
    power_end   = sum(Power[Year == max(Year)], na.rm = TRUE),
    taux_evolution = (power_end - power_start) / power_start * 100
  ) %>%
  filter(!is.na(taux_evolution), is.finite(taux_evolution))

print(Evolution)

# Sélection du top 10
Evolution_top10 <- Evolution %>%
  arrange(desc(taux_evolution)) %>%
  slice_head(n = 10)

print(Evolution_top10)

# Diagramme à barres horizontal
ggplot(Evolution_top10, aes(x = reorder(Country, taux_evolution), y = taux_evolution)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste0(round(taux_evolution, 1), "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, max(Evolution_top10$taux_evolution), by = 5000),  # Échelle par 1000
    limits = c(0, max(Evolution_top10$taux_evolution) * 1.15),
    labels = scales::number_format()  # Format standard sans séparateur de milliers
  ) +
  labs(
    title = "Top 10 des pays européens selon le taux d'évolution\nde la production des énergies renouvelables",
    x = "Pays",
    y = "Taux d'évolution (%)",
    caption = c("BUT Science des Données", "Source : EUROSTAT")
  ) +
  theme_minimal() +
  theme_general()

# ============================================================
# PROBLEMATIQUE 4
# ============================================================

# Production totale par année
UE_total <- donnees_cat %>%
  group_by(Year)  %>% 
  summarise(total_power = sum(Power, na.rm = TRUE)) %>%
  arrange(Year)

print(UE_total)

# Graphique
ggplot(UE_total, 
       aes(x = Year, 
           y = total_power/1000)) +
  geom_line(color = "steelblue", 
            size = 1.2) +
  geom_point(color = "purple", 
             size = 2) +
  geom_area(fill = "steelblue", 
            alpha = 0.2) +
  scale_x_continuous(
    breaks = seq(min(UE_total$Year), 
                 max(UE_total$Year), 
                 by = 5),
    limits = c(min(UE_total$Year), 
               max(UE_total$Year))
  ) +
  scale_y_continuous(
    limits = c(0, 1200),
    breaks = seq(0, 1200, by = 200),  
    labels = scales::comma_format(),
    expand = c(0, 0)  
  ) +
  labs(
    title = "Évolution de la production électrique globale \ndes 27 pays membres de l'UE",
    x = "Année",
    y = "Production électrique (en GW)",
    caption = c("BUT Science des Données", "Source : EUROSTAT")
  ) +
  theme_minimal() +
  theme_general()

# ============================================================
# PROBLEMATIQUE 5
# ============================================================

donnees_cat %>% 
  group_by(Type,Year) %>% 
summarise(Power_total = sum(Power)) %>% 
ggplot(aes(x = Year, 
           y = Power_total/1000,
           colour = Type))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.2) +
  scale_y_continuous(
    limits = c(0, 500),
    breaks = seq(0, 500, by = 100),  
    labels = scales::comma_format(),
    expand = c(0, 0)  
  )+
  theme_bw()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.border = element_blank())+
  labs(title = "Évolution de la production électrique
  des pays de l'UE par type de production",
       x = "Année", 
       y = "Production totale (en GW)",
       caption = c("BUT Science des Données", "Source : EUROSTAT"))+
  theme_general()


# Faire un focus sur les plus petits
donnees_cat %>% 
  group_by(Type,Year) %>% 
  summarise(Power_total = sum(Power)) %>%
  filter(Type %in% c("Tide, Wave and Ocean", "Geothermal",
                     "Solar Thermal", "Other Sources")) %>% 
  ggplot(aes(x = Year, 
             y = Power_total/1000,
             colour = Type))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.2) +
  scale_y_continuous(
    limits = c(0, 3),
    breaks = seq(0, 3, by = 0.3),  
    labels = scales::comma_format(),
    expand = c(0, 0)  
  )+
  theme_bw()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.border = element_blank())+
  labs(title = "Évolution de la production électrique 
  des pays de l'UE par type de production (focus)",
       x = "Année", 
       y = "Production totale (en GW)",
       caption = c("BUT Science des Données", "Source : EUROSTAT"))+
  theme_general()

# ============================================================
# PROBLEMATIQUE 6
# ============================================================
donnees_cat %>% 
  group_by(Year, Category) %>%
  filter(!(Category %in% "Other")) %>% 
  summarise(Power_total = sum(Power)) %>% 
  ggplot(aes(x = Year, 
             y = Power_total/1000,
             colour = Category))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.2) +
  scale_y_continuous(
    limits = c(100, 500))+
  theme_bw()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        panel.border = element_blank())+
  labs(title = "Évolution de la production électrique
  des pays de l'UE par catégorie de production",
       x = "Année", y = "Production totale (en GW)",
       caption = c("BUT Science des Données", "Source : EUROSTAT"))+
  theme_general()


# ============================================================
# PROBLEMATIQUE 7
# ============================================================
donnees_cat %>% 
  group_by(Country, Year, Category) %>% 
  summarise(Total_Power = sum(Power, na.rm = TRUE)) %>% 
  filter(Category %in% c("Fossil", "Renewable"))-> essai1

essai1 %>% 
  pivot_wider(names_from = Category, values_from = Total_Power, values_fill = 0) -> essai2

ggplot(essai2, aes(x = Fossil,
                   y = Renewable)) +
  geom_point(size = 2) +
  labs(title = "Production d'électricité par énergie renouvelable vs 
       Production d'électricité par énergie fossile",
       caption = c("BUT Science des Données", "Source : EUROSTAT")
  )

ggplot(essai2, aes(x = Fossil/1000, y = Renewable/1000)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "loess", color="green4", se = FALSE)+
  labs(title = "Énergies renouvelables vs fossiles",
       x = "Production d’énergie fossile (GW)",
       y = "Production d’énergie renouvelable (GW)",
       caption = c("BUT Science des Données", "Source : EUROSTAT")
  )+theme_general()
# L'association semble linéaire, on vérifie avec le coef de Pearson

# Coefficient de corrélation de Pearson entre Renewable et Fossil
cor_pearson <- cor(essai2$Renewable, essai2$Fossil, method = "pearson")
cor_pearson

modele <- lm(Renewable/1000 ~ Fossil/1000, data = essai2)
summary(modele)
coef(modele)
intercept <- round(coef(modele)[1], 3)
pente <- round(coef(modele)[2], 3)
R2 <- round(100*summary(modele)$r.squared, 2)

# Équation formatée
equation <- paste0("Prod. renouvelable = ", pente, " * Prod. fossile + ", intercept, 
                   "\nR² = ", R2, "%")

ggplot(essai2, aes(x = Fossil/1000, y = Renewable/1000)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  geom_smooth(method = "loess", color="green4", se = FALSE)+
  annotate("text", 
           x = min(essai2$Fossil/1000) + 0.05*(max(essai2$Fossil/1000)-min(essai2$Fossil/1000)),
           y = max(essai2$Renewable/1000) * 0.95,
           label = equation,
           size = 4,
           hjust = 0,
           vjust = 1,
           color = "red",
           fontface = "bold") +
  labs(
    title = "Énergies renouvelables vs fossiles",
    x = "Production d’énergie fossile (GW)",
    y = "Production d’énergie renouvelable (GW)",
    caption = c("BUT Science des Données", "Source : EUROSTAT")
  ) +
  theme_general()



# ============================================================
# PROBLEMATIQUE 8
# ============================================================

# La production totale de chaque pays pendant toute la période 
# 1990-2020

production_totale_pays <- donnees_cat %>%
  filter(Year >= 1990, Year <= 2020) %>%
  group_by(Country) %>%
  summarise(Total_Power = sum(Power, na.rm = TRUE)) %>%
  arrange(desc(Total_Power)) 

production_totale_pays

# identification du top 5  

top5 <- production_totale_pays %>% slice_max(Total_Power, n = 5)

top5
top5 %>% 
  ggplot(mapping = aes(x = reorder(Country, -Total_Power), y = Total_Power/1000))+
  geom_bar(fill = "lightblue", stat="identity")+
  geom_text(aes(label = paste0(round(Total_Power/1000, 2), " GW")), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_y_continuous(limits = c(0,5000)) +
  labs(title = "Top 5 des pays les plus producteurs d'énergie", x = "Pays", y = "Production totale (GW)") +
  theme_minimal() +
  theme_general()

# Filtrer les données uniquement pour ces 5 pays et sommer par année

top5_evolution <- donnees_cat %>%
  filter(Country %in% top5$Country,
         Year >= 1990, Year <= 2020) %>%
  group_by(Country, Year) %>%
  summarise(Production_Annuelle = sum(Power, na.rm = TRUE)) %>%
  ungroup()
top5_evolution


# Visualisation de l’évolution dans le temps

ggplot(top5_evolution, aes(x = Year, 
                           y = Production_Annuelle/1000, 
                           color = Country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Évolution de la production électrique\n des 5 pays les plus producteurs (1990-2020)",
       x = "Année",
       y = "Production totale (en GW)",
       color = "Pays",
       caption = c("BUT Science des Données", "Source : EUROSTAT")) +
  theme_minimal()+
  theme_general()


# ============================================================
# PROBLEMATIQUE 9
# ============================================================

type_renouvelables <- c("Hydro", "Wind", "Solar", "Geothermal", "Tide, wave, ocean", "Other renewables")

# Préparation du taux de croissance annuel par pays et année
df_ren <- donnees_cat %>%
  filter(Category == "Renewable") %>% 
  group_by(Country, Year) %>% 
  summarise(Power = sum(Power, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(GrowthRate = (Power / lag(Power) - 1) * 100) %>%
  ungroup()
print(df_ren)
# Graphique facet (multiples petits graphiques par pays)
ggplot(df_ren, aes(x = Year, y = GrowthRate, color = Country)) +
  geom_line(size = 1) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Évolution du taux de croissance annuel des énergies renouvelables 
       dans chaque pays de l'UE",
       x = "Année", y = "Taux de croissance annuel (%)",
       caption = c("BUT Science des Données", "Source : EUROSTAT")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        strip.text = element_text(size = 7))+
  theme_general()

#=====================================================================================
#Problématique10:Quels pays européens ont le plus développé le solaire (Solar PV + Solar Thermal)
#au cours des 20 dernières années ?
#=====================================================================================

#Graphique  : Classement Top 10 de la croissance solaire (2003 → 2023)
# -----------------------------
# Données solaires
# -----------------------------

types_solaire <- c("Solar PV", "Solar Thermal")

df_solar <- donnees_cat %>%
  filter(Type %in% types_solaire) %>%
  group_by(Country, Year) %>%
  summarise(Power = sum(Power, na.rm = TRUE), .groups = "drop")

# -----------------------------
# Calcul variation sur 20 ans
# -----------------------------

annee_debut <- max(df_solar$Year) - 20
annee_fin   <- max(df_solar$Year)

df_var <- df_solar %>%
  filter(Year %in% c(annee_debut, annee_fin)) %>%
  pivot_wider(names_from = Year, values_from = Power) %>%
  mutate(
    Variation = !!sym(as.character(annee_fin)) -
      !!sym(as.character(annee_debut))
  ) %>%
  arrange(desc(Variation))

# -----------------------------
# Top 10 pays
# -----------------------------

top10_solar <- df_var %>% slice_head(n = 10)

g1 <- ggplot(top10_solar,
             aes(x = reorder(Country, Variation/1000), y = Variation/1000)) +
  geom_col(fill = "#FDB813", width = 0.7) +
  geom_text(
    aes(label = round(Variation/1000, 2)),
    hjust = -0.1,
    size = 3.8
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = paste("Top 10 des pays ayant le plus développé le solaire \n(",
                  annee_debut, " → ", annee_fin, ")", sep = ""),
    x = "Pays",
    y = "Quantité d'énergie solaire produite (GW)",
    caption = c("BUT Science des Données", "Source : EUROSTAT")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10))
  )+
  theme_general()

print(g1)

