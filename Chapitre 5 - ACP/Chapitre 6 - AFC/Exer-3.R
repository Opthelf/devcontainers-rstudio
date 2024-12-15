
# AFC : Exercice 3

Dataset <- as.matrix(read.table('industrieInfoAFC.txt', header = T, sep=''))


chisq <- chisq.test (Dataset)
chisq


Total=addmargins(Dataset)
Total

total.Secteur= Total[4,1:5]  # Total Colonnes (Secteur)
total.Secteur
total.Continent=Total [1:3,6] # Total Lignes (Continent)
total.Continent

#a) Profils lignes


# Profil moyenne ligne ou aussi frequence marginale

n=sum(Dataset)
Prof.Moy.ligne=total.Continent/n
Prof.Moy.ligne

# Profil ligne 


profligne=prop.table(Dataset,1) #Donne tableau de profil ligne
profligne


new.prof.lignes=rbind(profligne,Prof.Moy.ligne) 

plot(as.table(new.prof.lignes),main="Profils lignes",col=heat.colors(5))



#b) Profils colonnes


# Profil moyenne colonne ou aussi frequence marginale


Prof.Moy.colonne=total.Secteur/n
Prof.Moy.colonne

# Profil colonnes


profcolonne=prop.table(Dataset,2)
profcolonne

new.prof.colonnes=cbind(profcolonne,Prof.Moy.colonne)

plot(t(as.table(new.prof.colonnes)),main="Profils Colonnes",col=heat.colors(4))


# Réaliser un AFC

library(FactoMineR)

AFC <- CA (Dataset, graph =T)


#valeurs propre : pour voire chaque axe represente quel pourcantage de Khi-deux
AFC$eig

# 100% du la distance du chi-deux est expliquée sur le plan principal.
#Donc les 2 premiers axes sont très suffisants.


#Contributions: pour voire la contribution de chaque modalités à la constructions de deux axes
#factoriels

AFC$row$contrib
#Etant donné qu'il y a 3 continents,la contribution moyenne est 1/3=33%.
#Asie dépasse largement cette contribution sur l'axe 1.

AFC$col$contrib

# On a 5 secteurs d'activite, donc la contribution moyenne de chaque secteurs est 1/5=20%.
# On remarque que Hardware depassent cette contribution.

# Coscarré : on peut voire ou chaque modalité est mieux representé

AFC$row$cos2
AFC$col$cos2





