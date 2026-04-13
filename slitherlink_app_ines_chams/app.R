# On charge Shiny (le framework web interactif de R) et bslib (pour le thème Bootstrap 5)
library(shiny)
library(bslib)

# ==============================================================================
# 0. SOLVEUR MATHÉMATIQUE (Garantit l'unicité de la solution)
# ==============================================================================
# Ce bloc entier existe pour répondre à UNE question :
# "Si je retire tel chiffre, est-ce que le puzzle a toujours exactement une solution ?"
# Sans ce solveur, retirer des chiffres au hasard donne des puzzles ambigus (= injouables).

# --- Fonctions utilitaires pour manipuler les arêtes ---
# Le solveur travaille sur des arêtes (les segments entre deux points de la grille).
# Chaque arête est identifiée par son type ("h" horizontal ou "v" vertical) et sa position (l, c).
# Ces 4 petites fonctions permettent de lire/écrire les arêtes sans se soucier
# de quel type c'est — ça évite de dupliquer la logique partout dans le solveur.

# Un sommet = une intersection de la grille. Il a jusqu'à 4 arêtes qui en partent.
# Cette fonction renvoie la liste de ces arêtes (en tenant compte des bords).
obtenir_aretes_sommet <- function(l, c, nb_lignes, nb_colonnes) {
  aretes <- list()
  # Le sommet (l,c) a une arête vers le haut seulement s'il n'est pas sur la première ligne
  if (l > 1)            aretes <- append(aretes, list(list(type = "v", l = l - 1, c = c)))
  # Vers le bas : seulement s'il n'est pas sur la dernière ligne + 1
  if (l <= nb_lignes)   aretes <- append(aretes, list(list(type = "v", l = l, c = c)))
  # Vers la gauche : seulement s'il n'est pas sur la première colonne
  if (c > 1)            aretes <- append(aretes, list(list(type = "h", l = l, c = c - 1)))
  # Vers la droite : seulement s'il n'est pas sur la dernière colonne + 1
  if (c <= nb_colonnes) aretes <- append(aretes, list(list(type = "h", l = l, c = c)))
  return(aretes)
}

# Une cellule (case de la grille) est entourée de 4 arêtes : haut, bas, gauche, droite.
# Le chiffre dans cette case = le nombre de ces 4 arêtes qui doivent être allumées.
obtenir_aretes_cellule <- function(l, c) {
  list(
    list(type = "h", l = l,     c = c),   # arête horizontale du haut de la case
    list(type = "h", l = l + 1, c = c),   # arête horizontale du bas
    list(type = "v", l = l, c = c),       # arête verticale de gauche
    list(type = "v", l = l, c = c + 1)    # arête verticale de droite
  )
}

# Le solveur utilise 3 valeurs pour chaque arête :
#   -1 = on ne sait pas encore (inconnu)
#    0 = éteint (pas de trait ici)
#    1 = allumé (trait dessiné ici)
# lire_arete et ecrire_arete font l'accès à la bonne matrice (h ou v) selon le type.
lire_arete <- function(etat, arete) {
  if (arete$type == "h") return(etat$h[arete$l, arete$c])
  else return(etat$v[arete$l, arete$c])
}

ecrire_arete <- function(etat, arete, valeur) {
  if (arete$type == "h") etat$h[arete$l, arete$c] <- valeur
  else etat$v[arete$l, arete$c] <- valeur
  return(etat)  # R passe par valeur, donc on doit retourner l'état modifié
}

# --- Propagation de contraintes ---
# C'est le moteur de déduction logique. Il applique deux familles de règles en boucle :
#   1) Les règles des cellules (un chiffre impose combien d'arêtes sont ON autour)
#   2) Les règles des sommets (chaque intersection a 0 ou 2 arêtes — jamais 1, jamais 3+)
# À chaque fois qu'une règle force une arête, ça peut déclencher d'autres déductions
# → d'où la boucle while(modifié). On s'arrête au "point fixe" (plus rien ne change).
# Si on tombe sur une contradiction (ex: 3 arêtes ON autour d'un "2"), on renvoie NULL.
propager_contraintes <- function(etat, chiffres, nb_lignes, nb_colonnes) {
  modifie <- TRUE  # Drapeau : est-ce qu'on a changé quelque chose au dernier tour ?
  while (modifie) {
    modifie <- FALSE  # On suppose que rien ne va changer — on corrigera si c'est faux
    
    # ─── Règle 1 : contraintes de cellule ───
    # Pour chaque case qui porte un chiffre, on vérifie la cohérence
    # entre ce chiffre et l'état actuel des 4 arêtes autour.
    for (l in 1:nb_lignes) {
      for (cc in 1:nb_colonnes) {
        if (is.na(chiffres[l, cc])) next  # Case vide (indice retiré) → aucune contrainte
        cible <- chiffres[l, cc]  # Le chiffre attendu (ex: "2" signifie 2 arêtes ON)
        aretes <- obtenir_aretes_cellule(l, cc)  # Les 4 arêtes autour de cette case
        valeurs <- sapply(aretes, function(a) lire_arete(etat, a))  # Leurs états actuels
        
        nb_on  <- sum(valeurs == 1)   # Combien sont déjà confirmées allumées
        nb_unk <- sum(valeurs == -1)   # Combien sont encore indéterminées
        
        # Détection de contradiction :
        # - Plus d'arêtes ON que ce que le chiffre autorise → impossible
        # - Même en allumant toutes les inconnues, on n'atteint pas le chiffre → impossible
        if (nb_on > cible || nb_on + nb_unk < cible) return(NULL)
        
        # Déduction : on a déjà le bon nombre d'allumées → les inconnues sont forcément OFF
        # (Exemple : un "2" avec déjà 2 arêtes ON → les 2 restantes doivent être OFF)
        if (nb_on == cible && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
        # Déduction inverse : il manque exactement autant d'ON que d'inconnues → tout allumer
        # (Exemple : un "3" avec 1 ON et 2 inconnues → les 2 inconnues sont forcément ON)
        if (nb_on + nb_unk == cible && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 1); modifie <- TRUE }
          }
        }
      }
    }
    
    # ─── Règle 2 : contraintes de sommet ───
    # La boucle fermée impose qu'à chaque intersection de la grille, exactement
    # 0 ou 2 arêtes se rejoignent. C'est ce qui fait que les traits forment un circuit.
    # - 0 arêtes = la boucle ne passe pas par ce sommet
    # - 2 arêtes = la boucle entre d'un côté et sort de l'autre
    # - 1 arête  = cul-de-sac, interdit (la boucle ne peut pas s'arrêter)
    # - 3+ arêtes = croisement, interdit (la boucle ne se croise pas)
    for (l in 1:(nb_lignes + 1)) {         # Les sommets vont de 1 à n+1 en ligne
      for (cc in 1:(nb_colonnes + 1)) {    # et de 1 à m+1 en colonne
        aretes <- obtenir_aretes_sommet(l, cc, nb_lignes, nb_colonnes)
        valeurs <- sapply(aretes, function(a) lire_arete(etat, a))
        
        nb_on  <- sum(valeurs == 1)
        nb_unk <- sum(valeurs == -1)
        
        # Contradictions directes
        if (nb_on > 2) return(NULL)                  # Croisement → impossible
        if (nb_on == 1 && nb_unk == 0) return(NULL)  # Cul-de-sac figé → impossible
        
        # 2 arêtes ON : le sommet est "complet", toute inconnue supplémentaire est OFF
        if (nb_on == 2 && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
        # 1 arête ON et 1 seule inconnue : l'inconnue DOIT être ON sinon cul-de-sac
        if (nb_on == 1 && nb_unk == 1) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 1); modifie <- TRUE }
          }
        }
        # 0 arête ON et 1 seule inconnue : l'inconnue DOIT être OFF sinon degré 1
        if (nb_on == 0 && nb_unk == 1) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
      }
    }
  }
  return(etat)  # Point fixe atteint : tout ce qui pouvait être déduit l'a été
}

# --- Vérification de connexité ---
# Quand le solveur a assigné toutes les arêtes (plus aucun -1), on doit encore vérifier
# que les arêtes ON forment UNE SEULE boucle et pas deux boucles séparées.
# Méthode : on construit le graphe des sommets reliés par les arêtes ON, puis on lance
# un BFS (parcours en largeur). Si le BFS visite tous les sommets actifs → c'est connecté.
verifier_boucle_unique <- function(etat, nb_lignes, nb_colonnes) {
  # Construction du graphe d'adjacence sous forme de liste
  adj <- list()
  
  # Chaque arête horizontale ON relie deux sommets horizontalement voisins
  for (l in 1:(nb_lignes + 1)) {
    for (cc in 1:nb_colonnes) {
      if (etat$h[l, cc] == 1) {
        # On encode les sommets comme "ligne-colonne" pour pouvoir les mettre dans une liste nommée
        s1 <- paste0(l, "-", cc)       # sommet gauche du trait
        s2 <- paste0(l, "-", cc + 1)   # sommet droit du trait
        adj[[s1]] <- c(adj[[s1]], s2)   # arête bidirectionnelle
        adj[[s2]] <- c(adj[[s2]], s1)
      }
    }
  }
  # Pareil pour les arêtes verticales : elles relient deux sommets verticalement voisins
  for (l in 1:nb_lignes) {
    for (cc in 1:(nb_colonnes + 1)) {
      if (etat$v[l, cc] == 1) {
        s1 <- paste0(l, "-", cc)       # sommet du haut
        s2 <- paste0(l + 1, "-", cc)   # sommet du bas
        adj[[s1]] <- c(adj[[s1]], s2)
        adj[[s2]] <- c(adj[[s2]], s1)
      }
    }
  }
  
  # Aucune arête allumée → pas de boucle du tout → ce n'est pas une solution valide
  if (length(adj) == 0) return(FALSE)
  
  # BFS classique : on part d'un sommet arbitraire et on explore de proche en proche
  sommets <- names(adj)       # Tous les sommets qui participent à au moins une arête ON
  visite <- c(sommets[1])     # Ensemble des sommets déjà visités
  file <- c(sommets[1])       # File d'attente du BFS (FIFO)
  while (length(file) > 0) {
    courant <- file[1]         # On prend le premier de la file
    file <- file[-1]           # On le retire
    for (voisin in adj[[courant]]) {       # Pour chaque voisin du sommet courant
      if (!(voisin %in% visite)) {         # S'il n'a pas déjà été visité
        visite <- c(visite, voisin)        # On le marque comme visité
        file <- c(file, voisin)            # Et on l'ajoute à la file pour explorer ses voisins
      }
    }
  }
  # Si le BFS a touché tous les sommets actifs, le graphe est connexe → une seule boucle.
  # Sinon il y a des sommets isolés dans d'autres composantes → plusieurs boucles → rejeté.
  return(length(visite) == length(sommets))
}

# --- Solveur principal ---
# Objectif : compter combien de solutions valides existent pour un ensemble d'indices donné.
# En pratique on veut juste savoir si c'est 1 (unique → bon puzzle) ou ≥2 (ambigu → mauvais).
# Donc on s'arrête dès qu'on en trouve 2 (paramètre max_solutions).
# L'algo combine propagation (déduction rapide) + backtracking (force brute quand on est bloqué).
compter_solutions <- function(chiffres, nb_lignes, nb_colonnes, max_solutions = 2) {
  # On démarre avec toutes les arêtes à -1 (rien de connu)
  etat_initial <- list(
    h = matrix(-1, nb_lignes + 1, nb_colonnes),   # arêtes horizontales
    v = matrix(-1, nb_lignes, nb_colonnes + 1)     # arêtes verticales
  )
  
  compteur <- 0             # Nombre de solutions trouvées jusqu'ici
  env <- environment()      # Astuce R : pour que la fonction récursive puisse modifier "compteur"
  # (R passe tout par valeur, donc sans ça, chaque appel récursif aurait sa propre copie)
  
  # Fonction récursive : explore l'arbre des possibilités
  backtrack <- function(etat) {
    # D'abord, on déduit tout ce qu'on peut par propagation
    etat <- propager_contraintes(etat, chiffres, nb_lignes, nb_colonnes)
    if (is.null(etat)) return()  # Contradiction → cette branche est morte, on remonte
    
    # On cherche la première arête encore indéterminée (-1)
    # C'est elle qu'on va "deviner" : on essaie les deux possibilités (ON et OFF)
    for (l in 1:(nb_lignes + 1)) {
      for (cc in 1:nb_colonnes) {
        if (etat$h[l, cc] == -1) {
          # Branche "ON" : on suppose que cette arête est allumée
          etat_on <- etat; etat_on$h[l, cc] <- 1
          backtrack(etat_on)
          if (env$compteur >= max_solutions) return()  # Déjà 2+ solutions → inutile de continuer
          # Branche "OFF" : on suppose que cette arête est éteinte
          etat_off <- etat; etat_off$h[l, cc] <- 0
          backtrack(etat_off)
          return()  # Les deux branches sont explorées, on remonte
        }
      }
    }
    # Pareil pour les arêtes verticales (on y arrive seulement si toutes les h sont résolues)
    for (l in 1:nb_lignes) {
      for (cc in 1:(nb_colonnes + 1)) {
        if (etat$v[l, cc] == -1) {
          etat_on <- etat; etat_on$v[l, cc] <- 1
          backtrack(etat_on)
          if (env$compteur >= max_solutions) return()
          etat_off <- etat; etat_off$v[l, cc] <- 0
          backtrack(etat_off)
          return()
        }
      }
    }
    
    # Si on arrive ici, TOUTES les arêtes sont assignées (plus aucun -1).
    # Dernière vérification : est-ce que ça forme bien une seule boucle fermée ?
    # (La propagation garantit degré 0 ou 2 partout, mais pas la connexité)
    if (verifier_boucle_unique(etat, nb_lignes, nb_colonnes)) {
      env$compteur <- env$compteur + 1  # C'est une solution valide !
    }
  }
  
  backtrack(etat_initial)  # On lance la recherche
  return(compteur)         # Résultat : 0 (impossible), 1 (unique), ou 2 (ambigu)
}

# --- Algorithme glouton de retrait d'indices ---
# C'est ici qu'on transforme un puzzle facile (tous les chiffres) en puzzle intéressant.
# Stratégie : on retire les chiffres un par un, et AVANT de confirmer chaque retrait,
# on vérifie avec le solveur que la solution reste unique. Si retirer ce chiffre
# crée une ambiguïté, on le remet en place et on passe au suivant.
# Le paramètre proportion_cible est un OBJECTIF (ex: garder 60% des indices), pas une garantie :
# si le puzzle a besoin de plus d'indices pour rester unique, on en garde plus.
retirer_indices <- function(chiffres_complets, nb_lignes, nb_colonnes, proportion_cible = 0.6) {
  print("--> [RETRAIT] Début du retrait d'indices avec vérification d'unicité...")
  
  chiffres <- chiffres_complets  # On travaille sur une copie qu'on va trouer de NA
  
  nb_total <- nb_lignes * nb_colonnes                     # Nombre total de cases
  nb_cible_visible <- ceiling(nb_total * proportion_cible) # Objectif : combien on veut garder
  nb_actuellement_visible <- nb_total                      # Pour l'instant on a tout
  
  # On mélange l'ordre des cases pour que le retrait soit aléatoire
  # (sinon on retirerait toujours les mêmes cases dans le même ordre)
  positions <- expand.grid(l = 1:nb_lignes, c = 1:nb_colonnes) # Toutes les positions (l,c)
  positions <- positions[sample(nrow(positions)), ]             # Mélange de Fisher-Yates
  
  for (i in 1:nrow(positions)) {
    # Stop si on a atteint notre objectif de retrait
    if (nb_actuellement_visible <= nb_cible_visible) break
    
    l <- positions$l[i]    # Ligne de la case qu'on va tenter de retirer
    cc <- positions$c[i]   # Colonne
    
    valeur_sauvee <- chiffres[l, cc]  # On garde la valeur au cas où il faudrait la remettre
    chiffres[l, cc] <- NA             # Retrait provisoire : la case devient vide
    
    # Le moment clé : on demande au solveur "combien de solutions avec cet indice en moins ?"
    # On lui dit de s'arrêter à 2 : on veut juste savoir si c'est 1 (ok) ou plus (pas ok)
    nb_sol <- compter_solutions(chiffres, nb_lignes, nb_colonnes, max_solutions = 2)
    
    if (nb_sol == 1) {
      # Le puzzle reste à solution unique → on valide le retrait
      nb_actuellement_visible <- nb_actuellement_visible - 1
      print(paste("    Indice retiré en (", l, ",", cc, ") - Restants:", nb_actuellement_visible))
    } else {
      # nb_sol = 0 (impossible, ne devrait pas arriver) ou nb_sol >= 2 (ambigu)
      # → On remet le chiffre, il est indispensable pour l'unicité
      chiffres[l, cc] <- valeur_sauvee
    }
  }
  
  print(paste("--> [RETRAIT] Terminé.", nb_actuellement_visible, "indices sur", nb_total,
              "conservés (", round(100 * nb_actuellement_visible / nb_total), "%)"))
  return(chiffres)  # La matrice avec des NA là où on a retiré des indices
}

# ==============================================================================
# 1. MOTEUR DE GÉNÉRATION (Création du puzzle)
# ==============================================================================
# Cette fonction orchestre les 4 étapes de fabrication d'un puzzle :
#   A) Faire pousser une forme aléatoire (l'intérieur de la boucle)
#   B) En déduire les traits de la solution (la frontière intérieur/extérieur)
#   C) Compter les traits autour de chaque case (les chiffres-indices)
#   D) Retirer intelligemment certains chiffres (via le solveur)
generer_grille_slitherlink <- function(nb_lignes = 5, nb_colonnes = 5, complexite = 0.6, proportion_indices = 0.6) {
  print(paste("--> [GÉNÉRATION] Création d'une nouvelle grille de taille", nb_lignes, "x", nb_colonnes))
  
  # ── ÉTAPE A : Créer une forme aléatoire fermée ──
  # On part d'une matrice vide (que des 0 = "extérieur").
  # On va remplir certaines cases avec des 1 = "intérieur".
  # La frontière entre les 0 et les 1 donnera automatiquement la boucle.
  matrice_interieur_exterieur <- matrix(0, nb_lignes, nb_colonnes)
  
  # Point de départ : le centre de la grille (pour que la forme soit bien centrée)
  ligne_depart <- floor(nb_lignes / 2)
  colonne_depart <- floor(nb_colonnes / 2)
  matrice_interieur_exterieur[ligne_depart, colonne_depart] <- 1  # Première case intérieure
  
  # Les "cases candidates" sont celles à la frontière de la zone :
  # elles ont encore des voisins extérieurs qu'on pourrait absorber
  cases_candidates <- list(c(ligne_depart, colonne_depart))
  print(paste("Cases candidates:", cases_candidates))
  
  # On veut que la zone intérieure fasse environ (complexité × surface totale) cases
  # Ex: complexité 0.6 sur une grille 5×5 → on vise 15 cases intérieures
  taille_cible_zone <- floor(nb_lignes * nb_colonnes * complexite)
  taille_actuelle_zone <- 1  # Pour l'instant on n'a que la case de départ
  
  print(paste("--> [GÉNÉRATION] Agrandissement de la zone jusqu'à", taille_cible_zone, "cases..."))
  
  #browser()  # Point d'arrêt de debug (décommenter pour inspecter en pas-à-pas dans RStudio)
  
  # Boucle de croissance : on agrandit la zone intérieure case par case
  while(taille_actuelle_zone < taille_cible_zone && length(cases_candidates) > 0) {
    # On choisit une case candidate au hasard (pas toujours la même → formes variées)
    index_choisi <- sample(length(cases_candidates), 1)
    case_actuelle <- cases_candidates[[index_choisi]]  # La case tirée au sort
    ligne_act <- case_actuelle[1]     # Sa ligne dans la matrice
    colonne_act <- case_actuelle[2]   # Sa colonne
    
    # On regarde ses 4 voisins directs (pas de diagonale dans Slitherlink)
    voisins <- list(c(ligne_act+1, colonne_act), c(ligne_act-1, colonne_act),
                    c(ligne_act, colonne_act+1), c(ligne_act, colonne_act-1))
    
    zone_agrandie <- FALSE  # Est-ce qu'on a réussi à ajouter au moins un voisin ?
    for(voisin in voisins) {
      ligne_voisin <- voisin[1]
      colonne_voisin <- voisin[2]
      
      # Le voisin doit être dans les limites de la grille ET être encore "extérieur" (0)
      if(ligne_voisin > 0 && ligne_voisin <= nb_lignes &&
         colonne_voisin > 0 && colonne_voisin <= nb_colonnes &&
         matrice_interieur_exterieur[ligne_voisin, colonne_voisin] == 0) {
        
        # On annexe ce voisin : il passe de 0 (extérieur) à 1 (intérieur)
        matrice_interieur_exterieur[ligne_voisin, colonne_voisin] <- 1
        # Il devient lui-même candidat pour agrandir la zone au tour suivant
        cases_candidates <- append(cases_candidates, list(c(ligne_voisin, colonne_voisin)))
        taille_actuelle_zone <- taille_actuelle_zone + 1  # Mise à jour du compteur
        zone_agrandie <- TRUE
        
        # Astuce pour avoir des formes irrégulières : 30% du temps on n'ajoute qu'un seul
        # voisin au lieu de tester les 4. Ça crée des tentacules et des bras.
        if(runif(1) > 0.7) break
      }
    }
    # Si la case n'a plus de voisin libre, OU par hasard (60% du temps),
    # on la retire des candidates. Ça force l'algo à aller chercher ailleurs.
    if(!zone_agrandie || runif(1) > 0.4) cases_candidates[[index_choisi]] <- NULL
  }
  
  # ── ÉTAPE B : Calculer la solution (les traits) ──
  # Règle fondamentale : il y a un trait entre deux cases adjacentes
  # si et seulement si l'une est "intérieure" (1) et l'autre "extérieure" (0).
  # C'est exactement la frontière de la forme qu'on vient de dessiner.
  print("--> [GÉNÉRATION] Calcul des traits solution (Frontière entre intérieur et extérieur)...")
  
  # Matrice des traits horizontaux : (n+1) lignes de traits × m colonnes
  # (il y a n+1 rangées de traits horizontaux pour n rangées de cases)
  solution_horizontale <- matrix(0, nb_lignes + 1, nb_colonnes)
  # Matrice des traits verticaux : n lignes × (m+1) colonnes de traits
  solution_verticale <- matrix(0, nb_lignes, nb_colonnes + 1)
  
  # Traits horizontaux : on compare la case au-dessus et la case en-dessous de chaque trait
  for(colonne in 1:nb_colonnes) {
    for(ligne in 1:(nb_lignes+1)) {
      # La case "au-dessus" du trait. Si on est sur la première rangée, il n'y en a pas → 0
      valeur_haut <- if(ligne == 1) 0 else matrice_interieur_exterieur[ligne-1, colonne]
      # La case "en-dessous". Si on est sur la dernière rangée + 1, il n'y en a pas → 0
      valeur_bas <- if(ligne > nb_lignes) 0 else matrice_interieur_exterieur[ligne, colonne]
      # S'ils sont différents (un dedans, un dehors), il faut un trait = frontière
      if(valeur_haut != valeur_bas) solution_horizontale[ligne, colonne] <- 1
    }
  }
  
  # Même logique pour les traits verticaux : on compare gauche et droite
  for(ligne in 1:nb_lignes) {
    for(colonne in 1:(nb_colonnes+1)) {
      valeur_gauche <- if(colonne == 1) 0 else matrice_interieur_exterieur[ligne, colonne-1]
      valeur_droite <- if(colonne > nb_colonnes) 0 else matrice_interieur_exterieur[ligne, colonne]
      if(valeur_gauche != valeur_droite) solution_verticale[ligne, colonne] <- 1
    }
  }
  
  # ── ÉTAPE C : Calculer les chiffres ──
  # Chaque case reçoit un chiffre = le nombre de traits solution qui l'entourent (0 à 4).
  # C'est un simple comptage, pas d'aléatoire ici.
  print("--> [GÉNÉRATION] Calcul des chiffres pour chaque case...")
  chiffres_indices <- matrix(NA, nb_lignes, nb_colonnes)  # On initialise à NA par convention
  for(ligne in 1:nb_lignes) {
    for(colonne in 1:nb_colonnes) {
      # Somme des 4 arêtes adjacentes dans la solution
      total_traits_autour <- solution_horizontale[ligne, colonne] +    # trait du haut
        solution_horizontale[ligne+1, colonne] +                        # trait du bas
        solution_verticale[ligne, colonne] +                            # trait de gauche
        solution_verticale[ligne, colonne+1]                            # trait de droite
      chiffres_indices[ligne, colonne] <- total_traits_autour
    }
  }
  
  # ── ÉTAPE D : Retirer des indices en garantissant l'unicité ──
  # C'est ici que le solveur entre en jeu : on lui demande de valider chaque retrait.
  print("--> [GÉNÉRATION] Retrait intelligent des indices (vérification d'unicité par solveur)...")
  chiffres_visibles <- retirer_indices(chiffres_indices, nb_lignes, nb_colonnes, proportion_indices)
  
  print("--> [GÉNÉRATION] Terminé avec succès !")
  # On renvoie tout dans un paquet : la grille, la solution, les chiffres complets et visibles
  return(list(
    nb_lignes = nb_lignes,
    nb_colonnes = nb_colonnes,
    solution_h = solution_horizontale,      # La vraie solution (pour la vérification finale)
    solution_v = solution_verticale,
    chiffres = chiffres_indices,            # Tous les chiffres (pour colorier vert/rouge pendant le jeu)
    chiffres_visibles = chiffres_visibles   # Les chiffres que le joueur voit (avec des trous)
  ))
}

# ==============================================================================
# 2. INTERFACE UTILISATEUR (Ce qui s'affiche à l'écran)
# ==============================================================================

# Thème de base : on part de "darkly" (thème sombre Bootstrap) et on le personnalise.
# Orbitron pour les titres (police géométrique futuriste), Rajdhani pour le texte courant.
theme_visuel <- bs_theme(
  version = 5,                              # Bootstrap 5
  bootswatch = "darkly",                    # Socle sombre
  primary = "#00f5d4",                      # Couleur d'accentuation : cyan néon
  base_font = font_google("Rajdhani"),      # Police du corps de texte
  heading_font = font_google("Orbitron")    # Police des titres
)

ui <- fluidPage(
  theme = theme_visuel,
  
  # ── CSS PERSONNALISÉ ──
  # On injecte du CSS directement dans le <head> de la page pour tout le style néon.
  # C'est plus flexible que de se battre avec les classes Bootstrap par défaut.
  tags$head(
    # Import Google Fonts (au cas où bslib ne les charge pas assez tôt)
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@600;900&family=Rajdhani:wght@400;600&display=swap"),
    
    tags$style(HTML("

      /* ── Variables CSS centralisées ── */
      /* Tout le thème de couleurs est défini ici : si on veut changer l'ambiance,
         il suffit de modifier ces 10 lignes au lieu de chercher dans tout le CSS. */
      :root {
        --neon-cyan:  #00f5d4;   /* Couleur principale (traits, titres, poteaux) */
        --neon-amber: #f5a623;   /* Couleur secondaire (bouton vérifier, avertissements) */
        --neon-red:   #ff3b5c;   /* Erreurs et contradictions */
        --bg-deep:    #090e1a;   /* Fond de la page (très sombre, presque noir bleuté) */
        --bg-panel:   #0d1526;   /* Fond de la barre de contrôle */
        --bg-card:    #111d35;   /* Fond de la carte qui contient la grille */
        --border-dim: #1e3060;   /* Bordures discrètes et traits inactifs */
        --text-main:  #cdd9f5;   /* Texte principal (blanc bleuté) */
        --text-dim:   #5a7aaa;   /* Texte secondaire (labels, sous-titres) */
      }

      /* ── Fond de page ── */
      body {
        background-color: var(--bg-deep) !important;  /* !important pour écraser Bootstrap */
        color: var(--text-main) !important;
        font-family: 'Rajdhani', sans-serif;
        font-size: 16px;
        margin: 0;
        padding: 0;
      }
      /* Shiny met un container-fluid avec du padding par défaut — on le vire */
      .container-fluid { padding: 0 !important; }

      /* ── Titre du jeu avec effet néon (text-shadow cyan) ── */
      .titre-jeu {
        font-family: 'Orbitron', sans-serif;
        font-weight: 900;
        font-size: 1.5rem;
        letter-spacing: 0.15em;
        color: var(--neon-cyan);
        /* Le double text-shadow crée l'effet de halo lumineux typique du néon */
        text-shadow: 0 0 8px var(--neon-cyan), 0 0 25px rgba(0,245,212,0.4);
        margin: 0;
        line-height: 1;
      }
      .sous-titre-jeu {
        font-size: 0.7rem;
        letter-spacing: 0.3em;                /* Espacement large = style HUD */
        color: var(--text-dim);
        text-transform: uppercase;
        margin-top: 3px;
        font-family: 'Rajdhani', sans-serif;
      }

      /* ── Barre de contrôle (en haut de la page, sticky) ── */
      /* Elle remplace le classique sidebarPanel de Shiny par un bandeau horizontal */
      .barre-controle {
        background: var(--bg-panel);
        border-bottom: 1px solid var(--border-dim);
        padding: 12px 24px;
        display: flex;                  /* Flexbox horizontal */
        align-items: center;
        gap: 24px;                      /* Espacement entre les blocs */
        flex-wrap: wrap;                /* Retour à la ligne si la fenêtre est étroite */
        box-shadow: 0 4px 20px rgba(0,0,0,0.5);  /* Ombre portée vers le bas */
        position: sticky;              /* Reste collée en haut quand on scrolle */
        top: 0;
        z-index: 50;                   /* Au-dessus du contenu mais sous les modales */
      }

      /* Ligne verticale entre les groupes de contrôles */
      .separateur-barre {
        width: 1px;
        height: 38px;
        background: var(--border-dim);
        flex-shrink: 0;  /* Ne se compresse pas quand la barre manque de place */
      }

      /* Chaque groupe label + widget (slider, checkbox, bouton) */
      .groupe-controle {
        display: flex;
        flex-direction: column;
        gap: 2px;
        min-width: 150px;
      }
      .groupe-controle > label {
        font-family: 'Orbitron', sans-serif;
        font-size: 0.55rem;
        letter-spacing: 0.2em;
        color: var(--text-dim);
        text-transform: uppercase;
        margin-bottom: 0;
      }
      /* On écrase les marges que Shiny ajoute par défaut aux widgets */
      .groupe-controle .form-group,
      .groupe-controle .shiny-input-container { margin-bottom: 0 !important; }

      /* Personnalisation de la bibliothèque ionRangeSlider utilisée par Shiny */
      .groupe-controle .irs--shiny .irs-bar       { background: var(--neon-cyan); border-color: var(--neon-cyan); }
      .groupe-controle .irs--shiny .irs-handle     { background: var(--neon-cyan) !important; border-color: var(--neon-cyan) !important; }
      .groupe-controle .irs--shiny .irs-from,
      .groupe-controle .irs--shiny .irs-to,
      .groupe-controle .irs--shiny .irs-single     { background: var(--neon-cyan); color: #000; font-family: 'Orbitron', sans-serif; font-size: 0.6rem; }
      .groupe-controle .irs--shiny .irs-line       { background: var(--border-dim); border-color: var(--border-dim); }
      .groupe-controle .irs--shiny .irs-grid-text  { color: var(--text-dim); }

      /* Cases à cocher empilées verticalement */
      .groupe-checks { display: flex; flex-direction: column; gap: 4px; }
      .groupe-checks .checkbox { margin: 0 !important; }
      .groupe-checks .checkbox label {
        font-size: 0.8rem;
        color: var(--text-main);
        letter-spacing: 0.04em;
        font-family: 'Rajdhani', sans-serif;
      }

      /* ── Boutons d'action ── */
      /* Style outlined : fond transparent, bordure colorée, remplissage au hover */
      .btn-nouveau {
        background: transparent;
        border: 1.5px solid var(--neon-cyan);
        color: var(--neon-cyan);
        font-family: 'Orbitron', sans-serif;
        font-size: 0.6rem;
        letter-spacing: 0.12em;
        padding: 8px 16px;
        border-radius: 3px;
        transition: all 0.2s ease;   /* Animation douce au survol */
        white-space: nowrap;         /* Pas de retour à la ligne dans le texte du bouton */
        cursor: pointer;
      }
      .btn-nouveau:hover, .btn-nouveau:focus {
        background: var(--neon-cyan);
        color: #000 !important;
        box-shadow: 0 0 16px rgba(0,245,212,0.5);  /* Halo néon au survol */
        outline: none;
      }
      /* Même pattern pour le bouton vérifier, mais en ambre */
      .btn-verifier {
        background: transparent;
        border: 1.5px solid var(--neon-amber);
        color: var(--neon-amber);
        font-family: 'Orbitron', sans-serif;
        font-size: 0.6rem;
        letter-spacing: 0.12em;
        padding: 8px 16px;
        border-radius: 3px;
        transition: all 0.2s ease;
        white-space: nowrap;
        cursor: pointer;
      }
      .btn-verifier:hover, .btn-verifier:focus {
        background: var(--neon-amber);
        color: #000 !important;
        box-shadow: 0 0 16px rgba(245,166,35,0.5);
        outline: none;
      }

      /* ── Zone de jeu (le grand espace central qui contient la grille) ── */
      .zone-jeu {
        display: flex;
        justify-content: center;       /* Centrage horizontal de la grille */
        align-items: flex-start;
        padding: 36px 20px 60px;       /* 60px en bas pour le bandeau fixe */
        min-height: calc(100vh - 90px); /* Remplit la hauteur restante sous la barre */
        background: var(--bg-deep);
        /* Motif de points en fond pour l'ambiance plan technique */
        background-image: radial-gradient(circle, #1a2a4a 1px, transparent 1px);
        background-size: 28px 28px;
      }

      /* ── Carte contenant le plot de la grille ── */
      .carte-grille {
        background: var(--bg-card);
        border: 1px solid var(--border-dim);
        border-radius: 6px;
        padding: 18px;
        /* Ombre portée subtile avec une teinte cyan */
        box-shadow: 0 0 40px rgba(0,245,212,0.06), 0 20px 60px rgba(0,0,0,0.6);
        cursor: crosshair;  /* La souris devient une croix de visée sur la grille */
        position: relative; /* Pour positionner les coins décoratifs */
      }
      /* Coins décoratifs : petits angles cyan en haut-gauche et bas-droite */
      /* C'est purement esthétique — ça donne un look cadre de visée HUD */
      .carte-grille::before {
        content: '';
        position: absolute;
        top: -1px; left: -1px;
        width: 20px; height: 20px;
        border-top: 2px solid var(--neon-cyan);
        border-left: 2px solid var(--neon-cyan);
        border-radius: 6px 0 0 0;
      }
      .carte-grille::after {
        content: '';
        position: absolute;
        bottom: -1px; right: -1px;
        width: 20px; height: 20px;
        border-bottom: 2px solid var(--neon-cyan);
        border-right: 2px solid var(--neon-cyan);
        border-radius: 0 0 6px 0;
      }

      /* ── Bandeau de statut fixe en bas de page ── */
      .bandeau-statut {
        position: fixed;
        bottom: 0; left: 0; right: 0;
        background: var(--bg-panel);
        border-top: 1px solid var(--border-dim);
        padding: 7px 28px;
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 100;  /* Au-dessus de tout le reste */
      }

      /* ── Texte de statut (utilisé dans la barre du haut ET le bandeau du bas) ── */
      .texte-statut {
        font-family: 'Orbitron', sans-serif;
        font-size: 0.65rem;
        letter-spacing: 0.15em;
        color: var(--text-dim);
        text-transform: uppercase;
      }
      .texte-statut.actif  { color: var(--text-main); }   /* En jeu : texte visible */
      .texte-statut.gagne  {
        color: var(--neon-cyan);
        text-shadow: 0 0 10px var(--neon-cyan);
        animation: pulse-glow 1.2s ease-in-out infinite alternate;  /* Pulsation néon */
      }
      .texte-statut.erreur { color: var(--neon-amber); }  /* Erreurs : ambre */

      /* Animation de pulsation pour le message de victoire */
      @keyframes pulse-glow {
        from { text-shadow: 0 0 5px var(--neon-cyan); }
        to   { text-shadow: 0 0 22px var(--neon-cyan), 0 0 45px rgba(0,245,212,0.35); }
      }
    "))
  ),
  
  # ── BARRE DE CONTRÔLE HORIZONTALE (layout Flexbox, remplace le sidebar) ──
  div(class = "barre-controle",
      
      # Bloc titre en haut à gauche de la barre
      div(
        p(class = "titre-jeu",      "SLITHERLINK"),
        p(class = "sous-titre-jeu", "Édition Française")
      ),
      
      div(class = "separateur-barre"),  # Trait vertical de séparation
      
      # Slider taille : contrôle la dimension de la grille (n × n)
      div(class = "groupe-controle",
          tags$label("Taille de grille"),
          sliderInput("entree_taille", label = NULL, min = 4, max = 20, value = 5, width = "155px")
      ),
      
      # Slider indices : pourcentage cible de chiffres visibles
      # Le solveur essaiera d'atteindre ce pourcentage, mais gardera plus si nécessaire
      div(class = "groupe-controle",
          tags$label("Indices affichés (%)"),
          sliderInput("entree_indices", label = NULL, min = 10, max = 100, value = 60, step = 5, width = "155px")
      ),
      
      div(class = "separateur-barre"),
      
      # Options d'aide visuelle
      div(class = "groupe-checks",
          checkboxInput("case_afficher_cibles",  "Cibles grises",    value = TRUE),   # Les petites croix au milieu des traits
          checkboxInput("case_afficher_erreurs", "Erreurs en rouge", value = FALSE)   # Mode triche : traits faux en rouge
      ),
      
      div(class = "separateur-barre"),
      
      # Les deux boutons d'action principaux, côte à côte
      div(style = "display:flex; gap:10px; align-items:center;",
          actionButton("bouton_nouveau",  "▶ NOUVEAU JEU", class = "btn-nouveau"),   # Lance la génération
          actionButton("bouton_verifier", "✔ VÉRIFIER",    class = "btn-verifier")   # Compare avec la solution
      ),
      
      div(class = "separateur-barre"),
      
      # Zone de texte dynamique : le statut actuel de la partie (dans la barre du haut)
      uiOutput("affichage_message_statut")
  ),
  
  # ── ZONE DE JEU ──
  # Le plot Shiny est enveloppé dans deux div pour le centrage et le style.
  div(class = "zone-jeu",
      div(class = "carte-grille",
          # plotOutput crée un <img> interactif. Le paramètre click = "clic_souris"
          # dit à Shiny d'envoyer les coordonnées au serveur à chaque clic.
          plotOutput("dessin_jeu", click = "clic_souris", height = "570px", width = "570px")
      )
  ),
  
  # ── BANDEAU BAS ──
  # Duplique le message de statut en bas de l'écran (utile sur grands écrans
  # où la barre du haut serait loin de la grille)
  div(class = "bandeau-statut",
      uiOutput("affichage_statut_bas")
  )
)

# ==============================================================================
# 3. SERVEUR (Le cerveau de l'application qui réagit à vos clics)
# ==============================================================================
server <- function(input, output, session) {
  
  # ── État réactif de la partie ──
  # reactiveValues est le "state" de Shiny : quand on modifie un de ces champs,
  # tout ce qui en dépend (les outputs) est automatiquement recalculé.
  etat_partie <- reactiveValues(
    donnees_grille    = NULL,     # L'objet renvoyé par generer_grille_slitherlink()
    traits_joueur_h   = NULL,     # Matrice des traits horizontaux posés par le joueur (0 ou 1)
    traits_joueur_v   = NULL,     # Matrice des traits verticaux posés par le joueur (0 ou 1)
    partie_gagnee     = FALSE,    # Flag : passe à TRUE quand 0 erreurs
    message_texte     = "Cliquez sur les traits gris pour les allumer.",
    classe_statut     = "texte-statut actif",   # Classe CSS du message (actif/gagne/erreur)
    chiffres_visibles = NULL      # Matrice des chiffres montrés au joueur (avec des NA)
  )
  
  # ── Nouveau jeu ──
  # Déclenché par le bouton "Nouveau Jeu" ET au chargement initial (ignoreNULL = FALSE)
  observeEvent(input$bouton_nouveau, {
    taille <- input$entree_taille  # Récupère la valeur du slider
    print(paste("=== NOUVELLE PARTIE DEMANDÉE (Taille", taille, ") ==="))
    
    # On fabrique la grille (étapes A→D, y compris le retrait d'indices par le solveur)
    grille <- generer_grille_slitherlink(taille, taille)
    etat_partie$donnees_grille  <- grille                      # On stocke tout dans l'état
    etat_partie$traits_joueur_h <- matrix(0, taille + 1, taille)   # Remise à zéro : aucun trait posé
    etat_partie$traits_joueur_v <- matrix(0, taille, taille + 1)
    etat_partie$partie_gagnee   <- FALSE
    etat_partie$message_texte   <- "Nouvelle partie lancée — à vous de jouer."
    etat_partie$classe_statut   <- "texte-statut actif"
    
    # /!\ ATTENTION : le bloc ci-dessous fait un masquage ALÉATOIRE sans solveur.
    # C'est l'ancienne méthode. Elle écrase les chiffres_visibles calculés par retirer_indices().
    # Pour utiliser le retrait vérifié mathématiquement, il faudrait plutôt faire :
    #   etat_partie$chiffres_visibles <- grille$chiffres_visibles
    # et supprimer tout ce bloc sample().
    chiffres_masques   <- grille$chiffres             # On repart des chiffres complets
    proportion_visible <- input$entree_indices / 100  # Conversion du slider (ex: 60 → 0.6)
    nb_cases_total     <- taille * taille
    nb_cases_a_cacher  <- floor(nb_cases_total * (1 - proportion_visible))  # Nombre à retirer
    if(nb_cases_a_cacher > 0) {
      # sample() choisit nb_cases_a_cacher positions au hasard parmi toutes les cases
      indices_a_cacher <- sample(nb_cases_total, nb_cases_a_cacher)
      chiffres_masques[indices_a_cacher] <- NA  # On les efface (NA = pas d'indice visible)
      # Problème : rien ne garantit que le puzzle reste à solution unique après ça.
    }
    etat_partie$chiffres_visibles <- chiffres_masques
    
  }, ignoreNULL = FALSE)  # FALSE = exécuter aussi au tout premier chargement de la page
  
  # ── Gestion du clic sur la grille ──
  # Shiny envoie input$clic_souris à chaque clic sur le plotOutput.
  # On cherche le trait le plus proche et on le bascule (ON ↔ OFF).
  observeEvent(input$clic_souris, {
    # Gardes : on ne fait rien si la grille n'existe pas ou si la partie est finie
    if(is.null(etat_partie$donnees_grille) || etat_partie$partie_gagnee) {
      print("[CLIC IGNORÉ] La partie est finie ou non commencée.")
      return()
    }
    
    # Coordonnées du clic dans le repère du plot (unités de la grille, pas des pixels)
    clic_x <- input$clic_souris$x
    clic_y <- input$clic_souris$y
    print(paste("--- Clic détecté aux coordonnées X:", round(clic_x, 2), " Y:", round(clic_y, 2), "---"))
    
    nb_lignes   <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # On va chercher le trait (horizontal ou vertical) dont le centre est le plus proche du clic.
    distance_minimale    <- Inf    # On commence à l'infini, tout sera plus proche
    cible_la_plus_proche <- NULL   # Contiendra le type et la position du trait trouvé
    
    # Parcours de tous les traits HORIZONTAUX possibles
    # Un trait horizontal à la position (ligne, colonne) va du poteau (ligne, colonne)
    # au poteau (ligne, colonne+1). Son centre est à (colonne + 0.5, ...).
    for(ligne in 1:(nb_lignes + 1)) {
      for(colonne in 1:nb_colonnes) {
        centre_x <- colonne + 0.5                  # Milieu horizontal du trait
        centre_y <- (nb_lignes + 2) - ligne         # Axe Y inversé (R dessine de bas en haut)
        
        # Distance euclidienne entre le clic et le centre du trait (Pythagore)
        distance <- sqrt((clic_x - centre_x)^2 + (clic_y - centre_y)^2)
        
        if(distance < distance_minimale) {
          distance_minimale    <- distance
          cible_la_plus_proche <- list(type="horizontal", ligne=ligne, colonne=colonne)
        }
      }
    }
    
    # Parcours de tous les traits VERTICAUX possibles
    # Un trait vertical à la position (ligne, colonne) va du poteau (ligne, colonne)
    # au poteau (ligne+1, colonne). Son centre est à (colonne, ligne + 0.5).
    for(ligne in 1:nb_lignes) {
      for(colonne in 1:(nb_colonnes + 1)) {
        centre_x <- colonne
        centre_y <- (nb_lignes + 2) - ligne - 0.5  # Centre vertical du trait (Y inversé)
        
        distance <- sqrt((clic_x - centre_x)^2 + (clic_y - centre_y)^2)
        
        if(distance < distance_minimale) {
          distance_minimale    <- distance
          cible_la_plus_proche <- list(type="vertical", ligne=ligne, colonne=colonne)
        }
      }
    }
    
    # Seuil de distance : si le clic est à plus de 0.45 unités du trait le plus proche,
    # on considère que le joueur n'a pas cliqué sur un trait → on ignore.
    # 0.45 est calibré pour que les zones de clic des traits ne se chevauchent pas.
    if(distance_minimale < 0.45) {
      type <- cible_la_plus_proche$type
      l    <- cible_la_plus_proche$ligne
      c    <- cible_la_plus_proche$colonne
      
      print(paste("Trait sélectionné:", type, "- Ligne:", l, "- Colonne:", c))
      
      # Bascule : 1 - 0 = 1 (allumer), 1 - 1 = 0 (éteindre). Élégant et sans if/else.
      if(type == "horizontal") {
        etat_partie$traits_joueur_h[l, c] <- 1 - etat_partie$traits_joueur_h[l, c]
      } else {
        etat_partie$traits_joueur_v[l, c] <- 1 - etat_partie$traits_joueur_v[l, c]
      }
    } else {
      print("Clic trop éloigné d'un trait, action annulée.")
    }
  })
  
  # ── Vérification de la solution ──
  # Compare terme à terme les matrices du joueur avec les matrices solution.
  observeEvent(input$bouton_verifier, {
    req(etat_partie$donnees_grille)  # Ne rien faire s'il n'y a pas de partie en cours
    print("=== VÉRIFICATION DE LA SOLUTION ===")
    
    # |joueur - solution| donne 1 là où ils diffèrent, 0 là où ils sont d'accord
    erreurs_horizontales <- sum(abs(etat_partie$traits_joueur_h - etat_partie$donnees_grille$solution_h))
    erreurs_verticales   <- sum(abs(etat_partie$traits_joueur_v - etat_partie$donnees_grille$solution_v))
    total_erreurs        <- erreurs_horizontales + erreurs_verticales
    
    print(paste("Nombre d'erreurs détectées :", total_erreurs))
    
    if(total_erreurs == 0) {
      # Victoire : on verrouille la partie et on passe en mode "gagné"
      etat_partie$partie_gagnee <- TRUE
      etat_partie$message_texte <- "★ MAGNIFIQUE — PUZZLE RÉSOLU ★"
      etat_partie$classe_statut <- "texte-statut gagne"   # Déclenche l'animation néon CSS
      showNotification("Victoire !", type = "message")
    } else {
      # Pas encore : on indique combien d'erreurs restent
      etat_partie$message_texte <- paste(total_erreurs, "erreur(s) restante(s).")
      etat_partie$classe_statut <- "texte-statut erreur"  # Texte ambre
      showNotification(paste("Il reste", total_erreurs, "erreurs."), type = "warning")
    }
  })
  
  # ── Rendu du message de statut (barre du haut) ──
  # renderUI recrée le HTML à chaque changement de message_texte ou classe_statut
  output$affichage_message_statut <- renderUI({
    div(class = etat_partie$classe_statut, etat_partie$message_texte)
  })
  
  # ── Rendu du message de statut (bandeau du bas) ── identique, juste positionné ailleurs
  output$affichage_statut_bas <- renderUI({
    div(class = etat_partie$classe_statut, etat_partie$message_texte)
  })
  
  # ── Dessin de la grille ──
  # renderPlot est rappelé à chaque fois qu'un reactive utilisé à l'intérieur change.
  # En pratique : après chaque clic (car traits_joueur_h/v changent) et après un nouveau jeu.
  output$dessin_jeu <- renderPlot({
    req(etat_partie$donnees_grille)  # Pas de données → pas de dessin
    
    nb_lignes   <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # On configure le canvas R : pas de marges, fond assorti au CSS de la carte
    par(mar = c(0, 0, 0, 0), bg = "#111d35")
    # Le plot vide définit le système de coordonnées.
    # xlim et ylim sont choisis pour que les poteaux tombent sur des entiers
    # et que les centres de cases tombent sur des .5 — ça simplifie tout le positionnement.
    plot(0, 0, type = "n",
         xlim = c(0.5, nb_colonnes + 1.5),
         ylim = c(0.5, nb_lignes + 1.5),
         asp = 1,          # Ratio 1:1 pour que les cases soient carrées
         axes = FALSE,     # Pas d'axes gradués
         xlab = "", ylab = "")
    
    # ─── COUCHE 1 : les cibles (petites croix d'aide) ───
    # Elles montrent au joueur où il peut cliquer. Désactivables via la checkbox.
    if(input$case_afficher_cibles) {
      # Croix au centre de chaque emplacement de trait horizontal
      for(l in 1:(nb_lignes+1)) {
        for(c in 1:nb_colonnes) {
          points(c + 0.5, (nb_lignes + 2) - l, pch = 3, col = "#1e3060", cex = 0.5)
          # pch = 3 = symbole "+" ; col = bleu très sombre ; cex = 0.5 = petite taille
        }
      }
      # Croix au centre de chaque emplacement de trait vertical
      for(l in 1:nb_lignes) {
        for(c in 1:(nb_colonnes+1)) {
          points(c, (nb_lignes + 2) - l - 0.5, pch = 3, col = "#1e3060", cex = 0.5)
        }
      }
    }
    
    # ─── COUCHE 2 : les poteaux ───
    # Points ronds cyan à chaque intersection de la grille (les "coins" des cases)
    grille_x <- rep(1:(nb_colonnes+1), each = nb_lignes+1)     # X : 1,1,1,...,2,2,2,...
    grille_y <- rep((nb_lignes+1):1, times = nb_colonnes+1)    # Y : n+1,n,...,1,n+1,n,...
    points(grille_x, grille_y, pch = 19, col = "#00f5d4", cex = 0.9)
    # pch = 19 = disque plein ; cex = 0.9 = un peu moins gros que la valeur par défaut
    
    # ─── COUCHE 3 : les chiffres ───
    # On affiche le chiffre seulement si la case n'est pas masquée (non-NA).
    # La couleur dépend du nombre de traits posés par le joueur autour de cette case.
    for(l in 1:nb_lignes) {
      for(c in 1:nb_colonnes) {
        chiffre_attendu <- etat_partie$donnees_grille$chiffres[l, c]  # Valeur réelle (toujours connue côté serveur)
        chiffre_visible <- etat_partie$chiffres_visibles[l, c]        # Ce que le joueur voit (ou NA)
        
        # Comptage des traits du joueur autour de cette case (même logique que le calcul initial)
        traits_autour_joueur <- etat_partie$traits_joueur_h[l, c] +
          etat_partie$traits_joueur_h[l+1, c] +
          etat_partie$traits_joueur_v[l, c] +
          etat_partie$traits_joueur_v[l, c+1]
        
        # Si l'indice a été retiré (NA), on n'affiche rien — la case reste vide
        if(!is.na(chiffre_visible)) {
          # Code couleur :
          couleur_texte <- "#cdd9f5"                                        # Blanc bleuté : en cours
          if(traits_autour_joueur == chiffre_attendu) couleur_texte <- "#00f5d4"  # Cyan néon : satisfait
          if(traits_autour_joueur >  chiffre_attendu) couleur_texte <- "#ff3b5c"  # Rouge néon : trop de traits
          # Note : on compare avec chiffre_attendu (pas chiffre_visible) car ils ont la même valeur
          # quand la case est visible. La distinction existe pour la cohérence conceptuelle.
          
          # Positionnement : le chiffre va au centre de la case (l, c)
          # qui se trouve à x = c + 0.5, y = (nb_lignes+1) - l + 0.5 dans le repère du plot
          text(c + 0.5, (nb_lignes+1) - l + 0.5, chiffre_visible,
               col = couleur_texte, cex = 1.8, font = 2, family = "sans")
          # cex = 1.8 = assez gros pour être lisible ; font = 2 = gras
        }
      }
    }
    
    # ─── COUCHE 4 : les traits du joueur ───
    # Chaque trait est soit actif (posé par le joueur → épais et coloré)
    # soit inactif (non posé → fin et sombre, quasi invisible).
    epaisseur_trait_actif   <- 5   # Bien visible
    epaisseur_trait_inactif <- 1   # Discret
    
    # Traits horizontaux
    for(l in 1:(nb_lignes+1)) {
      for(c in 1:nb_colonnes) {
        hauteur_y <- (nb_lignes + 2) - l  # Conversion ligne → coordonnée Y (axe inversé)
        
        if(etat_partie$traits_joueur_h[l, c] == 1) {
          # Le joueur a posé ce trait
          couleur_trait <- "#00f5d4"  # Cyan néon par défaut
          # Si le mode triche est activé ET que ce trait n'est pas dans la solution → rouge
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_h[l, c] == 0) {
            couleur_trait <- "#ff3b5c"
          }
          # segments() trace une ligne entre deux points
          segments(c, hauteur_y, c+1, hauteur_y, lwd = epaisseur_trait_actif, col = couleur_trait)
        } else {
          # Trait non posé : ligne fantôme sombre (pour montrer qu'il y a un emplacement)
          segments(c, hauteur_y, c+1, hauteur_y, lwd = epaisseur_trait_inactif, col = "#1e3060", lty = 1)
        }
      }
    }
    
    # Traits verticaux — même logique, orientation différente
    for(l in 1:nb_lignes) {
      for(c in 1:(nb_colonnes+1)) {
        hauteur_y <- (nb_lignes + 2) - l
        
        if(etat_partie$traits_joueur_v[l, c] == 1) {
          couleur_trait <- "#00f5d4"
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_v[l, c] == 0) {
            couleur_trait <- "#ff3b5c"
          }
          # Trait vertical : même x, y varie de hauteur_y à hauteur_y - 1
          segments(c, hauteur_y, c, hauteur_y-1, lwd = epaisseur_trait_actif, col = couleur_trait)
        } else {
          segments(c, hauteur_y, c, hauteur_y-1, lwd = epaisseur_trait_inactif, col = "#1e3060", lty = 1)
        }
      }
    }
  })
}

# Point d'entrée : lance l'application Shiny avec l'UI et le serveur définis ci-dessus
shinyApp(ui, server)