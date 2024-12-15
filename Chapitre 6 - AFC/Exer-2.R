
# Analyses Factorielles des corespondances


#Exercice 2


Nobel<-as.matrix(read.table('Nobel.txt', header = T))


#Test d'independance :
chisq <- chisq.test (Nobel)
chisq

#p-vqlue = 0.001<0.05  alors les deux variables sont li?es. Alors, avec AFC on va voire quelles sont
#les modalit?s les plus li?es.

#Ici khi-deux =73.161

# Le warning affich? car on a des effectifs th?oriques qui sont inferieur ? 5. 

chisq$expected # Affiche les effectifs th?oriques


#Q1 : Tout d'abord on donne le total de chaque ligne et chaque colonne

# Total de chaque ligne (variable Discplines)


Total=addmargins(Nobel)
Total
total.Discipline= Total[1:6,10] # On enleve le total
total.Discipline
total.Pays =Total [7,1:9]
total.Pays

#nombre total de tous les observations : Effectif total

n=sum(Nobel)
n


#a) Profil Moyen colonne

Prof.Moy.col=total.Discipline/n
Prof.Moy.col #Donne Fr?quence marginale ou Profil colonne Moyen de la variable colonne Noms


#Profile colonne

profcolonne=prop.table(Nobel,2)
profcolonne

new.prof.colonnes=cbind(profcolonne,Prof.Moy.col)

plot(t(as.table(new.prof.colonnes)),main="Profils Colonnes",col=heat.colors(4))


#Etude Lignes (Discplines)

#Profil Moyenne ligne
Prof.Moy.ligne=total.Pays/n  
Prof.Moy.ligne


#Profil ligne


profligne=prop.table(Nobel,1)


new.prof.lignes=rbind(profligne,Prof.Moy.ligne) 

plot(as.table(new.prof.lignes),main="Profils lignes",col=heat.colors(4))


#Q2 : R?aliser un AFC

library(FactoMineR)

AFC <- CA (Nobel, graph =T)


#valeurs propre : pour voire chaque axe represente quel pourcantage de Khi-deux
AFC$eig

#74,8% du la distance du chi-deux est expliqu?e sur le plan principal.
#Donc les 2 premiers axes sont suffisants.


#Contributions: pour voire la contribution de chaque modalit?s ? la constructions de deux axes
#factoriels

AFC$row$contrib
#Etant donn? qu'il y a 6 displines,la contribution moyenne est 1/6=16%.
#Litterature d?passe largement cette contribution sur l'axe 1.

AFC$col$contrib

# On a 9 pays, donc la contribution moyenne de chaque pays est 1/9=11%.
# On remarque que France, US, Suede et Italie depassent cette contribution.

# Coscarr? : on peut voire ou chaque modalit? est mieux represent?

round(AFC$row$cos2,3)
AFC$col$cos2

