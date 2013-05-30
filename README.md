inpg_cge
========

# Auteur

Grégory Mounié @2013
Tous les scripts sont couverts par la licence GPL v3+.

# Motivation

Le but de ces scripts est d'aider à l'analyse des données issues
de l'enquête sur le devenir de diplômés réalisée tout les ans par
l'Ensimag.

L'enquête est fondée sur l'enquête de la Conférence des Grandes écoles
(CGE) avec des questions Grenoble-INP en plus.

Le but est surtout de construire un ensemble d'images, courbes et
jolies graphiques, bien fondés statistiquement.

# Outils de référence

L'outil de référence est R. Les courbes sont réalisées avec ggplot2.

# Contributions

Bienvenue :-)

NB: je préfère les contributions bien fondés statistiquement.

Exemple de statistiques que je trouve mal fondées (je peux changer
d'avis :-) ):

- salaire moyen: c'est une question récurrente mais qui est souvent
  mal posée et rends difficile les comparaisons: parle-t-on du salaire
  en france ? à l'étranger ? en entreprise ? en thèse ?  avec prime
  (important à l'étranger) ? sans prime ? Avec quel modèle pour
  l'intervalle de confiance ?
- salaire moyen par filière: tous les inconvénients du précédent en
  pire car l'échantillon est plus petit.

Je préfère de beaucoup le salaire median, beaucoup plus robuste aux
valeurs exceptionnelles: un boxplot pour les inter-quartiles et les
points bruts pour les valeurs exceptionnelles.


# Échantillon de données

Cet entrepot ne contient pas les vrais données. Mais les
données-exemples devraient permettre de construire des schémas avec un
rendu proche de celui avec les véritables entrées.

Les entêtes des fichiers CSV sont correctes. Mais chaque entrée est
reconstruite à partir des données en réalisant trois opérations:

1. Contruire 50 entrées (lignes)

2. Tirer chaque valeurs de chaque colonne au hasard parmi les
réponses, pour densifier les colonnes. Néanmoins cela rend les
réponses incohérentes: les doctorants en france auront donc aussi un
salaire de VIE, d'entreprise, travaillant en france et dans un pays
étranger en même temps. par contre la densité de réponses sur une
colonne devrait être pas trop mal.

3. Anonymiser les champs personnels (noms, prénoms, nom de la société,
commentaires, champs libres, ... ) en les remplaçant par des chaînes
aléatoires.
