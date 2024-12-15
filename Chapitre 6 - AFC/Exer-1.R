
# Analyses Factorielles des corespondances

#Exercice 1

Marketing =as.matrix(read.table("Marketing.txt",header=T))

# On ajoute as.matrix pour que R voit ce jeu de donn?es comme une matrice 
# et realisera apres tout les calculs matricielles.

#Test de dependance Khi-deux entre les deux variables.
chisq <- chisq.test (Marketing)
chisq
# p-value <0.05 alors les deux variables sont li?es. Alors, avec AFC on va voire quelles sont
#les modalit?s les plus li?es.


# Pour r?pondre aux questions, on commence par calculer les totales de chaque lignes c'?-dire
# l'effectif de chaque modalit?s de la variable (Qualitatifs), et l'effectif total de chaque 
# colonnes (Noms).


Total=addmargins(Marketing)
Total
total.Qualitatif= Total[1:11,13]  # Total Colonnes (Noms)
total.Qualitatif
total.Nom=Total [12,1:12] # Total Lignes (Qualitatif)
total.Nom


# Q1-

#a) Profils lignes


# Calcule de profil moyenne ligne ou aussi frequence marginale

n=sum(Marketing)
Prof.Moy.ligne=total.Nom/n
Prof.Moy.ligne

# Calcule de profil ligne 


profligne=prop.table(Marketing,1) #Donne tableau de profil ligne
profligne

new.prof.lignes=rbind(profligne,Prof.Moy.ligne) # On a mis dans le m?me tableau le profils
# moyenne ligne et les profils lignes pour faire un plot et comparer

plot(as.table(new.prof.lignes),main="Profils lignes",col=heat.colors(4))

# Les profils lignes sont diff?rents de profil ligne moyen calculer, et ?a valide le resultat
#du test de Khi-deux appliqu? au d?but et qui a nous montr? que les deux variables sont li?es.

#Dans le cas o? les variables sont pas li?es (ind?pendants), les profils lignes vont avoir des valeur
#tr?s proche de profil moyen ligne.



#b) Profils colonnes


# Calcule de profil moyenne colonne ou aussi frequence marginale


Prof.Moy.colonne=total.Qualitatif/n
Prof.Moy.colonne

# Calcule de profil colonnes


profcolonne=prop.table(Marketing,2)
profcolonne

new.prof.colonnes=cbind(profcolonne,Prof.Moy.colonne)

plot(t(as.table(new.prof.colonnes)),main="Profils Colonnes",col=heat.colors(4))

# sans utiliser le transpos?, je veux avoir un graphe avec des barres inverses, c-?-dire, 
#le colonne Prof Moy colonne ca va etre en bas horizentalement.



#c) distance de Khi-deux

chisq <- chisq.test (Marketing)
chisq

# chi.deux=696.79

#Les cases qui contribuent le plus ? la distance de khi-deux sont les cases ou on a une grande
# difference entre les effectifs th?oriques et observ?es.



#Q2 : AFC

library("FactoMineR")

AFC <- CA (Marketing, graph =T)

#a)

AFC$eig # Donne les valeurs propres

#Le plan principal permet d'expliquer uniquement 51,8% de la distance du chi-deux 
#(c'est-?-dire l'?cart entre effectifs observ?s et effectifs th?oriques).
#Si on ajoute l'axe 3, on a 68,9% et il faut aller jusqu'? l'axe 4 pour avoir un pourcentage 
#int?ressant. L'interpr?tation que nous ferons sur le plan 
# principale reste donc incompl?te. 


# N.B : Pourcentage du chi-deux sur dim 1 est 29.92%
# N.B : Si on fait la somme des valeurs propres on obtient:0.6085.. 
# Si on multiple ce somme par n on obtient la valeur de khi-deux qui est 696.78.


#b) Il faut regarder les contributions de la variable (Qualitatif) qui est variable ligne

AFC$row$contrib

#Etant donn? qu'il y a 11 qualificatifs,la contribution moyenne est 1/11=9,1%.
#Les qualificatifs d?passant cette contribution sur l'axe 1 sont : Distingue, Vulgaire,Cocasse
#et Nouveau-riche

AFC$row$coord

#c) Il faut regarder les contributions de la variable (Noms) qui est variable colonne

AFC$col$contrib

#Etant donn? qu'il y a 12 noms, la contribution moyenne est 1/12=8,3%. 

# Les noms d?passant cette contribution sur l'axe 1 sont : Zodiaque, Hotesse et Corsaire
AFC$row$cos2

#d) Faire m?me travail sur l'axe 2

#l'Axe 2 represente 21,84% du chi-deux.
# Contribution de la variable (Qualitatifs) : les modalit?s qui depassent la contribution 
#moyenne de 9.1% sont : Vieillot (23.8%), Vulgaire (18.25%) et Pourfemme ( 44.6%).

#Contribution de la variable (Noms) : les modalit?s qui d?passent sur l'axe 2, la contribution
#moyenne de 8.3% sont : Directoire, Corsaire, Escale et Hotesse.

#N.B on peut utiliser aussi les commandes res.ca$col$cos2 et 
# res.ca$row$cos2 pour voire le qualit? de r?presentation de 
# chaque modalit?s dans le plan former par les deux axes 
# factoriels.

#e)
round(AFC$col$cos2,3)

#Orly est mieux repr?sent? sur les axe 1 et 5, donc
#Alezan bien repr?sent? sur l'axe 2
#Corsaire sur l'axe 4


######################################################################
######################################################################
######################################################################

