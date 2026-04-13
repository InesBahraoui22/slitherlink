library(shiny)
library(bslib)

# ==============================================================================
# 0. SOLVEUR MATHÉMATIQUE (Garantit l'unicité de la solution)
# ==============================================================================

# --- Fonctions utilitaires pour manipuler les arêtes ---

# Récupère les arêtes adjacentes à un sommet (intersection de la grille)
# Un sommet (l,c) peut avoir jusqu'à 4 arêtes : haut, bas, gauche, droite
obtenir_aretes_sommet <- function(l, c, nb_lignes, nb_colonnes) {
  aretes <- list()
  if (l > 1)            aretes <- append(aretes, list(list(type = "v", l = l - 1, c = c)))  # Haut
  if (l <= nb_lignes)   aretes <- append(aretes, list(list(type = "v", l = l, c = c)))      # Bas
  if (c > 1)            aretes <- append(aretes, list(list(type = "h", l = l, c = c - 1)))  # Gauche
  if (c <= nb_colonnes) aretes <- append(aretes, list(list(type = "h", l = l, c = c)))      # Droite
  return(aretes)
}

# Récupère les 4 arêtes autour d'une cellule (l, c)
obtenir_aretes_cellule <- function(l, c) {
  list(
    list(type = "h", l = l,     c = c),   # trait du haut
    list(type = "h", l = l + 1, c = c),   # trait du bas
    list(type = "v", l = l, c = c),       # trait de gauche
    list(type = "v", l = l, c = c + 1)    # trait de droite
  )
}

# Lit la valeur d'une arête dans l'état du solveur (-1 = inconnu, 0 = éteint, 1 = allumé)
lire_arete <- function(etat, arete) {
  if (arete$type == "h") return(etat$h[arete$l, arete$c])
  else return(etat$v[arete$l, arete$c])
}

# Écrit la valeur d'une arête dans l'état du solveur
ecrire_arete <- function(etat, arete, valeur) {
  if (arete$type == "h") etat$h[arete$l, arete$c] <- valeur
  else etat$v[arete$l, arete$c] <- valeur
  return(etat)
}

# --- Propagation de contraintes ---
# Applique les règles logiques locales en boucle jusqu'à ce que plus rien ne change
# Retourne NULL si on détecte une contradiction, sinon l'état mis à jour
propager_contraintes <- function(etat, chiffres, nb_lignes, nb_colonnes) {
  modifie <- TRUE
  while (modifie) {
    modifie <- FALSE
    
    # Règle 1 : Contraintes de cellule (les chiffres/indices)
    # Pour chaque case avec un indice, la somme des 4 arêtes autour doit valoir l'indice
    for (l in 1:nb_lignes) {
      for (cc in 1:nb_colonnes) {
        if (is.na(chiffres[l, cc])) next  # Pas d'indice ici, on passe
        cible <- chiffres[l, cc]
        aretes <- obtenir_aretes_cellule(l, cc)
        valeurs <- sapply(aretes, function(a) lire_arete(etat, a))
        
        nb_on  <- sum(valeurs == 1)   # Combien sont déjà allumées
        nb_unk <- sum(valeurs == -1)   # Combien sont encore inconnues
        
        # Contradiction : trop d'arêtes allumées, ou pas assez d'inconnues pour atteindre la cible
        if (nb_on > cible || nb_on + nb_unk < cible) return(NULL)
        
        # Si on a déjà le bon nombre d'allumées → éteindre toutes les inconnues
        if (nb_on == cible && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
        # Si le nombre d'allumées + inconnues = cible → allumer toutes les inconnues
        if (nb_on + nb_unk == cible && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 1); modifie <- TRUE }
          }
        }
      }
    }
    
    # Règle 2 : Contraintes de sommet (à chaque intersection : degré 0 ou 2)
    # C'est la règle de la boucle : chaque sommet est traversé par 0 ou 2 arêtes
    for (l in 1:(nb_lignes + 1)) {
      for (cc in 1:(nb_colonnes + 1)) {
        aretes <- obtenir_aretes_sommet(l, cc, nb_lignes, nb_colonnes)
        valeurs <- sapply(aretes, function(a) lire_arete(etat, a))
        
        nb_on  <- sum(valeurs == 1)
        nb_unk <- sum(valeurs == -1)
        
        if (nb_on > 2) return(NULL)                        # Contradiction : degré > 2
        if (nb_on == 1 && nb_unk == 0) return(NULL)        # Cul-de-sac (degré 1 sans issue)
        
        # Degré 2 atteint → toutes les inconnues restantes doivent être éteintes
        if (nb_on == 2 && nb_unk > 0) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
        # 1 arête allumée et 1 seule inconnue → l'inconnue doit être allumée (pour avoir degré 2)
        if (nb_on == 1 && nb_unk == 1) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 1); modifie <- TRUE }
          }
        }
        # 0 arête allumée et 1 seule inconnue → l'inconnue doit être éteinte (degré 1 interdit)
        if (nb_on == 0 && nb_unk == 1) {
          for (a in aretes) {
            if (lire_arete(etat, a) == -1) { etat <- ecrire_arete(etat, a, 0); modifie <- TRUE }
          }
        }
      }
    }
  }
  return(etat)
}

# --- Vérification de connexité ---
# Vérifie que les arêtes allumées forment UNE SEULE boucle (pas plusieurs boucles séparées)
verifier_boucle_unique <- function(etat, nb_lignes, nb_colonnes) {
  # On convertit chaque arête allumée en paire de sommets pour construire un graphe
  adj <- list()
  
  # Arêtes horizontales : relient le sommet (l, c) au sommet (l, c+1)
  for (l in 1:(nb_lignes + 1)) {
    for (cc in 1:nb_colonnes) {
      if (etat$h[l, cc] == 1) {
        s1 <- paste0(l, "-", cc)
        s2 <- paste0(l, "-", cc + 1)
        adj[[s1]] <- c(adj[[s1]], s2)
        adj[[s2]] <- c(adj[[s2]], s1)
      }
    }
  }
  # Arêtes verticales : relient le sommet (l, c) au sommet (l+1, c)
  for (l in 1:nb_lignes) {
    for (cc in 1:(nb_colonnes + 1)) {
      if (etat$v[l, cc] == 1) {
        s1 <- paste0(l, "-", cc)
        s2 <- paste0(l + 1, "-", cc)
        adj[[s1]] <- c(adj[[s1]], s2)
        adj[[s2]] <- c(adj[[s2]], s1)
      }
    }
  }
  
  if (length(adj) == 0) return(FALSE)  # Aucune arête allumée = pas de boucle
  
  # BFS : on part d'un sommet et on visite tout ce qui est connecté
  sommets <- names(adj)
  visite <- c(sommets[1])
  file <- c(sommets[1])
  while (length(file) > 0) {
    courant <- file[1]
    file <- file[-1]
    for (voisin in adj[[courant]]) {
      if (!(voisin %in% visite)) {
        visite <- c(visite, voisin)
        file <- c(file, voisin)
      }
    }
  }
  # Si tous les sommets actifs sont visités → une seule composante → une seule boucle
  return(length(visite) == length(sommets))
}

# --- Solveur principal ---
# Compte le nombre de solutions (s'arrête dès qu'on en trouve max_solutions)
# Utilise : propagation de contraintes + backtracking (essai des 2 valeurs pour chaque arête inconnue)
compter_solutions <- function(chiffres, nb_lignes, nb_colonnes, max_solutions = 2) {
  # État initial : toutes les arêtes sont inconnues (-1)
  etat_initial <- list(
    h = matrix(-1, nb_lignes + 1, nb_colonnes),
    v = matrix(-1, nb_lignes, nb_colonnes + 1)
  )
  
  compteur <- 0
  env <- environment()  # Pour partager le compteur entre les appels récursifs
  
  backtrack <- function(etat) {
    # 1. Propager les contraintes
    etat <- propager_contraintes(etat, chiffres, nb_lignes, nb_colonnes)
    if (is.null(etat)) return()  # Contradiction détectée → on fait marche arrière
    
    # 2. Chercher la première arête encore inconnue (d'abord horizontales, puis verticales)
    for (l in 1:(nb_lignes + 1)) {
      for (cc in 1:nb_colonnes) {
        if (etat$h[l, cc] == -1) {
          etat_on <- etat; etat_on$h[l, cc] <- 1     # Essayer de l'allumer
          backtrack(etat_on)
          if (env$compteur >= max_solutions) return()
          etat_off <- etat; etat_off$h[l, cc] <- 0   # Essayer de l'éteindre
          backtrack(etat_off)
          return()
        }
      }
    }
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
    
    # 3. Plus d'inconnues → on a une assignation complète
    # On vérifie que ça forme bien UNE SEULE boucle fermée
    if (verifier_boucle_unique(etat, nb_lignes, nb_colonnes)) {
      env$compteur <- env$compteur + 1
    }
  }
  
  backtrack(etat_initial)
  return(compteur)
}

# --- Algorithme glouton de retrait d'indices ---
# On retire les indices un par un (dans un ordre aléatoire),
# et on ne confirme le retrait QUE si le solveur certifie que la solution reste unique.
# proportion_cible = pourcentage d'indices qu'on VEUT garder (objectif, pas garanti)
retirer_indices <- function(chiffres_complets, nb_lignes, nb_colonnes, proportion_cible = 0.6) {
  print("--> [RETRAIT] Début du retrait d'indices avec vérification d'unicité...")
  
  chiffres <- chiffres_complets  # Copie de travail (on va mettre des NA dedans)
  
  nb_total <- nb_lignes * nb_colonnes
  nb_cible_visible <- ceiling(nb_total * proportion_cible)  # Nombre d'indices qu'on veut garder
  nb_actuellement_visible <- nb_total
  
  # Liste de toutes les positions, mélangée aléatoirement
  positions <- expand.grid(l = 1:nb_lignes, c = 1:nb_colonnes)
  positions <- positions[sample(nrow(positions)), ]
  
  for (i in 1:nrow(positions)) {
    # Si on a déjà atteint l'objectif de retrait, on arrête
    if (nb_actuellement_visible <= nb_cible_visible) break
    
    l <- positions$l[i]
    cc <- positions$c[i]
    
    valeur_sauvee <- chiffres[l, cc]       # On sauvegarde avant de retirer
    chiffres[l, cc] <- NA                  # On retire provisoirement
    
    # Le solveur cherche au plus 2 solutions : si 1 seule → c'est bon, si 2+ → ambiguïté
    nb_sol <- compter_solutions(chiffres, nb_lignes, nb_colonnes, max_solutions = 2)
    
    if (nb_sol == 1) {
      # Unicité préservée → on confirme le retrait
      nb_actuellement_visible <- nb_actuellement_visible - 1
      print(paste("    Indice retiré en (", l, ",", cc, ") - Restants:", nb_actuellement_visible))
    } else {
      # Ambiguïté → on remet l'indice en place
      chiffres[l, cc] <- valeur_sauvee
    }
  }
  
  print(paste("--> [RETRAIT] Terminé.", nb_actuellement_visible, "indices sur", nb_total,
              "conservés (", round(100 * nb_actuellement_visible / nb_total), "%)"))
  return(chiffres)
}

# ==============================================================================
# 1. MOTEUR DE GÉNÉRATION (Création du puzzle)
# ==============================================================================
generer_grille_slitherlink <- function(nb_lignes = 5, nb_colonnes = 5, complexite = 0.6, proportion_indices = 0.6) {
  print(paste("--> [GÉNÉRATION] Création d'une nouvelle grille de taille", nb_lignes, "x", nb_colonnes))
  
  # ÉTAPE A : Créer une forme aléatoire fermée (la solution du puzzle)
  # On utilise une matrice remplie de 0. Les 1 représenteront "l'intérieur" de la boucle.
  matrice_interieur_exterieur <- matrix(0, nb_lignes, nb_colonnes) # Matrice du plan de remplissage du dessin
  
  # On commence au milieu de la grille
  ligne_depart <- floor(nb_lignes / 2)
  colonne_depart <- floor(nb_colonnes / 2)
  matrice_interieur_exterieur[ligne_depart, colonne_depart] <- 1
  
  # Liste des cases qu'on peut encore transformer en 1
  cases_candidates <- list(c(ligne_depart, colonne_depart)) 
  print(paste("Cases candidates:", cases_candidates))
  
  taille_cible_zone <- floor(nb_lignes * nb_colonnes * complexite)
  taille_actuelle_zone <- 1
  
  print(paste("--> [GÉNÉRATION] Agrandissement de la zone jusqu'à", taille_cible_zone, "cases..."))
  
  #browser()
  
  # Tant que notre zone n'est pas assez grande, on l'agrandit aléatoirement
  while(taille_actuelle_zone < taille_cible_zone && length(cases_candidates) > 0) {
    index_choisi <- sample(length(cases_candidates), 1) # Tirage au sort de l'indice d'une case 
    case_actuelle <- cases_candidates[[index_choisi]] # Récupération de la case
    ligne_act <- case_actuelle[1] # Récupération de la ligne
    colonne_act <- case_actuelle[2] # Récupération de la colonne
    
    # On regarde les cases voisines (Haut, Bas, Droite, Gauche)
    voisins <- list(c(ligne_act+1, colonne_act), c(ligne_act-1, colonne_act), 
                    c(ligne_act, colonne_act+1), c(ligne_act, colonne_act-1)) # liste des coordonnées des cases voisines
    
    zone_agrandie <- FALSE
    for(voisin in voisins) {
      ligne_voisin <- voisin[1] # Récupération de la ligne
      colonne_voisin <- voisin[2] # Récupération de la colonne
      
      # Si le voisin est dans la grille et n'est pas encore à "1"
      if(ligne_voisin > 0 && ligne_voisin <= nb_lignes && 
         colonne_voisin > 0 && colonne_voisin <= nb_colonnes && 
         matrice_interieur_exterieur[ligne_voisin, colonne_voisin] == 0) {
        
        # On l'ajoute à la zone intérieure
        matrice_interieur_exterieur[ligne_voisin, colonne_voisin] <- 1 # la case voisin concernée devient inclus dans le dessin
        cases_candidates <- append(cases_candidates, list(c(ligne_voisin, colonne_voisin))) # ajoute les coordonnées du nouvel élément dans la liste des cases candidates
        taille_actuelle_zone <- taille_actuelle_zone+1 # recalcul de la taille de la zone
        zone_agrandie <- TRUE # devient vrai si on a ajouté au moins un candidat parmis les voisins
        
        # Petit hasard pour donner des formes biscornues
        if(runif(1) > 0.7) break # si la réalisation de la VA uniforme de 0 à 1 est supérieur à 0.7 on ne rajoute pas les voisins
      }
    }
    # Si on est bloqué, on retire cette case des candidates
    if(!zone_agrandie || runif(1) > 0.4) cases_candidates[[index_choisi]] <- NULL 
  }
  
  # ÉTAPE B : Calculer les traits/murs (La Solution)
  # Si une case "1" (intérieur) touche une case "0" (extérieur), il doit y avoir un trait entre les deux.
  print("--> [GÉNÉRATION] Calcul des traits solution (Frontière entre intérieur et extérieur)...")
  solution_horizontale <- matrix(0, nb_lignes + 1, nb_colonnes)
  solution_verticale <- matrix(0, nb_lignes, nb_colonnes + 1)
  
  # Chercher les traits horizontaux
  for(colonne in 1:nb_colonnes) {
    for(ligne in 1:(nb_lignes+1)) {
      valeur_haut <- if(ligne == 1) 0 else matrice_interieur_exterieur[ligne-1, colonne]
      valeur_bas <- if(ligne > nb_lignes) 0 else matrice_interieur_exterieur[ligne, colonne]
      if(valeur_haut != valeur_bas) solution_horizontale[ligne, colonne] <- 1
    }
  }
  
  # Chercher les traits verticaux
  for(ligne in 1:nb_lignes) {
    for(colonne in 1:(nb_colonnes+1)) {
      valeur_gauche <- if(colonne == 1) 0 else matrice_interieur_exterieur[ligne, colonne-1]
      valeur_droite <- if(colonne > nb_colonnes) 0 else matrice_interieur_exterieur[ligne, colonne]
      if(valeur_gauche != valeur_droite) solution_verticale[ligne, colonne] <- 1
    }
  }
  
  # ÉTAPE C : Calculer les chiffres qui seront affichés dans chaque case
  print("--> [GÉNÉRATION] Calcul des chiffres pour chaque case...")
  chiffres_indices <- matrix(NA, nb_lignes, nb_colonnes)
  for(ligne in 1:nb_lignes) {
    for(colonne in 1:nb_colonnes) {
      # On compte le nombre de traits (solution) autour de cette case
      total_traits_autour <- solution_horizontale[ligne, colonne] + 
        solution_horizontale[ligne+1, colonne] + 
        solution_verticale[ligne, colonne] + 
        solution_verticale[ligne, colonne+1]
      chiffres_indices[ligne, colonne] <- total_traits_autour
    }
  }
  
  # ÉTAPE D : Retirer des indices avec VÉRIFICATION MATHÉMATIQUE de l'unicité
  # Au lieu de retirer au hasard, on utilise un solveur qui garantit qu'il n'y a qu'une seule solution
  print("--> [GÉNÉRATION] Retrait intelligent des indices (vérification d'unicité par solveur)...")
  chiffres_visibles <- retirer_indices(chiffres_indices, nb_lignes, nb_colonnes, proportion_indices)
  
  print("--> [GÉNÉRATION] Terminé avec succès !")
  # On renvoie tout ce qu'on a fabriqué
  return(list(
    nb_lignes = nb_lignes, 
    nb_colonnes = nb_colonnes, 
    solution_h = solution_horizontale, 
    solution_v = solution_verticale, 
    chiffres = chiffres_indices,            # Tous les chiffres (solution complète, pour vérif interne)
    chiffres_visibles = chiffres_visibles   # Chiffres après retrait (ce que le joueur voit)
  ))
}

# ==============================================================================
# 2. INTERFACE UTILISATEUR (Ce qui s'affiche à l'écran)
# ==============================================================================

# Thème de base bslib : on utilise "darkly" comme socle
# et on superpose un CSS complet pour le style néon rétro-futuriste
theme_visuel <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#00f5d4",
  base_font = font_google("Rajdhani"),     # Police corps : géométrique et lisible
  heading_font = font_google("Orbitron")  # Police titres : rétro-futuriste
)

ui <- fluidPage(
  theme = theme_visuel,
  
  # --------------------------------------------------------------------------
  # CSS PERSONNALISÉ : thème néon rétro-futuriste
  # --------------------------------------------------------------------------
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@600;900&family=Rajdhani:wght@400;600&display=swap"),
    
    tags$style(HTML("

      /* ── Variables globales de couleur ── */
      :root {
        --neon-cyan:  #00f5d4;
        --neon-amber: #f5a623;
        --neon-red:   #ff3b5c;
        --bg-deep:    #090e1a;
        --bg-panel:   #0d1526;
        --bg-card:    #111d35;
        --border-dim: #1e3060;
        --text-main:  #cdd9f5;
        --text-dim:   #5a7aaa;
      }

      /* ── Fond général ── */
      body {
        background-color: var(--bg-deep) !important;
        color: var(--text-main) !important;
        font-family: 'Rajdhani', sans-serif;
        font-size: 16px;
        margin: 0;
        padding: 0;
      }
      .container-fluid { padding: 0 !important; }

      /* ── Titre principal avec effet néon ── */
      .titre-jeu {
        font-family: 'Orbitron', sans-serif;
        font-weight: 900;
        font-size: 1.5rem;
        letter-spacing: 0.15em;
        color: var(--neon-cyan);
        text-shadow: 0 0 8px var(--neon-cyan), 0 0 25px rgba(0,245,212,0.4);
        margin: 0;
        line-height: 1;
      }
      .sous-titre-jeu {
        font-size: 0.7rem;
        letter-spacing: 0.3em;
        color: var(--text-dim);
        text-transform: uppercase;
        margin-top: 3px;
        font-family: 'Rajdhani', sans-serif;
      }

      /* ── Barre de contrôle horizontale en haut (remplace le sidebarPanel) ── */
      .barre-controle {
        background: var(--bg-panel);
        border-bottom: 1px solid var(--border-dim);
        padding: 12px 24px;
        display: flex;
        align-items: center;
        gap: 24px;
        flex-wrap: wrap;
        box-shadow: 0 4px 20px rgba(0,0,0,0.5);
        position: sticky;
        top: 0;
        z-index: 50;
      }

      /* ── Séparateur vertical dans la barre ── */
      .separateur-barre {
        width: 1px;
        height: 38px;
        background: var(--border-dim);
        flex-shrink: 0;
      }

      /* ── Groupe label + contrôle dans la barre ── */
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
      /* Suppression des marges internes des widgets Shiny dans la barre */
      .groupe-controle .form-group,
      .groupe-controle .shiny-input-container { margin-bottom: 0 !important; }

      /* Sliders : couleur cyan néon */
      .groupe-controle .irs--shiny .irs-bar       { background: var(--neon-cyan); border-color: var(--neon-cyan); }
      .groupe-controle .irs--shiny .irs-handle     { background: var(--neon-cyan) !important; border-color: var(--neon-cyan) !important; }
      .groupe-controle .irs--shiny .irs-from,
      .groupe-controle .irs--shiny .irs-to,
      .groupe-controle .irs--shiny .irs-single     { background: var(--neon-cyan); color: #000; font-family: 'Orbitron', sans-serif; font-size: 0.6rem; }
      .groupe-controle .irs--shiny .irs-line       { background: var(--border-dim); border-color: var(--border-dim); }
      .groupe-controle .irs--shiny .irs-grid-text  { color: var(--text-dim); }

      /* ── Cases à cocher ── */
      .groupe-checks { display: flex; flex-direction: column; gap: 4px; }
      .groupe-checks .checkbox { margin: 0 !important; }
      .groupe-checks .checkbox label {
        font-size: 0.8rem;
        color: var(--text-main);
        letter-spacing: 0.04em;
        font-family: 'Rajdhani', sans-serif;
      }

      /* ── Boutons d'action ── */
      .btn-nouveau {
        background: transparent;
        border: 1.5px solid var(--neon-cyan);
        color: var(--neon-cyan);
        font-family: 'Orbitron', sans-serif;
        font-size: 0.6rem;
        letter-spacing: 0.12em;
        padding: 8px 16px;
        border-radius: 3px;
        transition: all 0.2s ease;
        white-space: nowrap;
        cursor: pointer;
      }
      .btn-nouveau:hover, .btn-nouveau:focus {
        background: var(--neon-cyan);
        color: #000 !important;
        box-shadow: 0 0 16px rgba(0,245,212,0.5);
        outline: none;
      }
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

      /* ── Zone de jeu principale (grille centrée, pleine largeur) ── */
      .zone-jeu {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        padding: 36px 20px 60px;       /* 60px en bas pour laisser place au bandeau fixe */
        min-height: calc(100vh - 90px);
        background: var(--bg-deep);
        /* Grille de points en arrière-plan pour l'ambiance */
        background-image: radial-gradient(circle, #1a2a4a 1px, transparent 1px);
        background-size: 28px 28px;
      }

      /* ── Carte conteneur de la grille ── */
      .carte-grille {
        background: var(--bg-card);
        border: 1px solid var(--border-dim);
        border-radius: 6px;
        padding: 18px;
        box-shadow: 0 0 40px rgba(0,245,212,0.06), 0 20px 60px rgba(0,0,0,0.6);
        cursor: crosshair;
        position: relative;
      }
      /* Coins décoratifs néon */
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
        z-index: 100;
      }

      /* ── Texte de statut (barre haute et bandeau bas) ── */
      .texte-statut {
        font-family: 'Orbitron', sans-serif;
        font-size: 0.65rem;
        letter-spacing: 0.15em;
        color: var(--text-dim);
        text-transform: uppercase;
      }
      .texte-statut.actif  { color: var(--text-main); }
      .texte-statut.gagne  {
        color: var(--neon-cyan);
        text-shadow: 0 0 10px var(--neon-cyan);
        animation: pulse-glow 1.2s ease-in-out infinite alternate;
      }
      .texte-statut.erreur { color: var(--neon-amber); }

      @keyframes pulse-glow {
        from { text-shadow: 0 0 5px var(--neon-cyan); }
        to   { text-shadow: 0 0 22px var(--neon-cyan), 0 0 45px rgba(0,245,212,0.35); }
      }
    "))
  ),
  
  # --------------------------------------------------------------------------
  # BARRE DE CONTRÔLE HORIZONTALE (remplace le sidebarPanel d'origine)
  # --------------------------------------------------------------------------
  div(class = "barre-controle",
      
      # Bloc titre
      div(
        p(class = "titre-jeu",      "SLITHERLINK"),
        p(class = "sous-titre-jeu", "Édition Française")
      ),
      
      div(class = "separateur-barre"),
      
      # Slider : taille de la grille
      div(class = "groupe-controle",
          tags$label("Taille de grille"),
          sliderInput("entree_taille", label = NULL, min = 4, max = 20, value = 5, width = "155px")
      ),
      
      # Slider : densité des indices (% de chiffres visibles)
      div(class = "groupe-controle",
          tags$label("Indices affichés (%)"),
          sliderInput("entree_indices", label = NULL, min = 10, max = 100, value = 60, step = 5, width = "155px")
      ),
      
      div(class = "separateur-barre"),
      
      # Cases à cocher : aides visuelles
      div(class = "groupe-checks",
          checkboxInput("case_afficher_cibles",  "Cibles grises",    value = TRUE),
          checkboxInput("case_afficher_erreurs", "Erreurs en rouge", value = FALSE)
      ),
      
      div(class = "separateur-barre"),
      
      # Boutons d'action
      div(style = "display:flex; gap:10px; align-items:center;",
          actionButton("bouton_nouveau",  "▶ NOUVEAU JEU", class = "btn-nouveau"),
          actionButton("bouton_verifier", "✔ VÉRIFIER",    class = "btn-verifier")
      ),
      
      div(class = "separateur-barre"),
      
      # Statut court visible dans la barre elle-même
      uiOutput("affichage_message_statut")
  ),
  
  # --------------------------------------------------------------------------
  # ZONE DE JEU PRINCIPALE (grille centrée, pleine largeur)
  # --------------------------------------------------------------------------
  div(class = "zone-jeu",
      div(class = "carte-grille",
          # C'est ici qu'on capte les clics ("clic_souris")
          plotOutput("dessin_jeu", click = "clic_souris", height = "570px", width = "570px")
      )
  ),
  
  # --------------------------------------------------------------------------
  # BANDEAU DE STATUT FIXE EN BAS DE PAGE
  # --------------------------------------------------------------------------
  div(class = "bandeau-statut",
      uiOutput("affichage_statut_bas")
  )
)

# ==============================================================================
# 3. SERVEUR (Le cerveau de l'application qui réagit à vos clics)
# ==============================================================================
server <- function(input, output, session) {
  
  # Mémoire de l'application : ce qui doit être mis à jour à l'écran
  etat_partie <- reactiveValues(
    donnees_grille    = NULL,
    traits_joueur_h   = NULL,
    traits_joueur_v   = NULL,
    partie_gagnee     = FALSE,
    message_texte     = "Cliquez sur les traits gris pour les allumer.",
    classe_statut     = "texte-statut actif",
    # Matrice des chiffres visibles par le joueur (NA = case masquée, le chiffre reste connu en interne)
    chiffres_visibles = NULL
  )
  
  # ACTION : Quand on clique sur le bouton "Nouveau Jeu"
  observeEvent(input$bouton_nouveau, {
    taille <- input$entree_taille
    print(paste("=== NOUVELLE PARTIE DEMANDÉE (Taille", taille, ") ==="))
    
    # On génère la grille et on remet le plateau à zéro
    grille <- generer_grille_slitherlink(taille, taille)
    etat_partie$donnees_grille  <- grille
    etat_partie$traits_joueur_h <- matrix(0, taille + 1, taille)
    etat_partie$traits_joueur_v <- matrix(0, taille, taille + 1)
    etat_partie$partie_gagnee   <- FALSE
    etat_partie$message_texte   <- "Nouvelle partie lancée — à vous de jouer."
    etat_partie$classe_statut   <- "texte-statut actif"
    
    # Masquage aléatoire des chiffres selon le pourcentage d'indices choisi par le joueur
    # On part de la grille complète et on efface (NA) les cases non sélectionnées
    chiffres_masques   <- grille$chiffres
    proportion_visible <- input$entree_indices / 100
    nb_cases_total     <- taille * taille
    nb_cases_a_cacher  <- floor(nb_cases_total * (1 - proportion_visible))
    if(nb_cases_a_cacher > 0) {
      indices_a_cacher <- sample(nb_cases_total, nb_cases_a_cacher)
      chiffres_masques[indices_a_cacher] <- NA # NA signifie "case sans indice affiché"
    }
    etat_partie$chiffres_visibles <- chiffres_masques
    
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE permet de lancer ça dès l'ouverture de la page
  
  # ACTION : Quand le joueur clique sur le dessin
  observeEvent(input$clic_souris, {
    # Si le jeu n'est pas chargé ou est déjà gagné, on ignore le clic
    if(is.null(etat_partie$donnees_grille) || etat_partie$partie_gagnee) {
      print("[CLIC IGNORÉ] La partie est finie ou non commencée.")
      return()
    }
    
    # On récupère les coordonnées X et Y de la souris
    clic_x <- input$clic_souris$x
    clic_y <- input$clic_souris$y
    print(paste("--- Clic détecté aux coordonnées X:", round(clic_x, 2), " Y:", round(clic_y, 2), "---"))
    
    nb_lignes   <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # Variables pour trouver la croix (cible) la plus proche du clic
    distance_minimale    <- Inf
    cible_la_plus_proche <- NULL
    
    # 1. Vérifier la distance avec tous les traits HORIZONTAUX
    for(ligne in 1:(nb_lignes + 1)) {
      for(colonne in 1:nb_colonnes) {
        centre_x <- colonne + 0.5
        centre_y <- (nb_lignes + 2) - ligne # Astuce: On inverse l'axe Y pour correspondre au dessin
        
        # Théorème de Pythagore pour calculer la distance entre la souris et le centre du trait
        distance <- sqrt((clic_x - centre_x)^2 + (clic_y - centre_y)^2)
        
        if(distance < distance_minimale) {
          distance_minimale    <- distance
          cible_la_plus_proche <- list(type="horizontal", ligne=ligne, colonne=colonne)
        }
      }
    }
    
    # 2. Vérifier la distance avec tous les traits VERTICAUX
    for(ligne in 1:nb_lignes) {
      for(colonne in 1:(nb_colonnes + 1)) {
        centre_x <- colonne
        centre_y <- (nb_lignes + 2) - ligne - 0.5
        
        distance <- sqrt((clic_x - centre_x)^2 + (clic_y - centre_y)^2)
        
        if(distance < distance_minimale) {
          distance_minimale    <- distance
          cible_la_plus_proche <- list(type="vertical", ligne=ligne, colonne=colonne)
        }
      }
    }
    
    # 3. Action finale : Si on a cliqué assez près d'une cible (distance < 0.45)
    if(distance_minimale < 0.45) {
      type <- cible_la_plus_proche$type
      l    <- cible_la_plus_proche$ligne
      c    <- cible_la_plus_proche$colonne
      
      print(paste("Trait sélectionné:", type, "- Ligne:", l, "- Colonne:", c))
      
      # On inverse l'état du trait (si c'est 0 ça devient 1, si c'est 1 ça devient 0)
      if(type == "horizontal") {
        etat_partie$traits_joueur_h[l, c] <- 1 - etat_partie$traits_joueur_h[l, c]
      } else {
        etat_partie$traits_joueur_v[l, c] <- 1 - etat_partie$traits_joueur_v[l, c]
      }
    } else {
      print("Clic trop éloigné d'un trait, action annulée.")
    }
  })
  
  # ACTION : Quand on clique sur "Vérifier ma solution"
  observeEvent(input$bouton_verifier, {
    req(etat_partie$donnees_grille)
    print("=== VÉRIFICATION DE LA SOLUTION ===")
    
    # On compare les traits du joueur avec les traits de la solution
    erreurs_horizontales <- sum(abs(etat_partie$traits_joueur_h - etat_partie$donnees_grille$solution_h))
    erreurs_verticales   <- sum(abs(etat_partie$traits_joueur_v - etat_partie$donnees_grille$solution_v))
    total_erreurs        <- erreurs_horizontales + erreurs_verticales
    
    print(paste("Nombre d'erreurs détectées :", total_erreurs))
    
    if(total_erreurs == 0) {
      etat_partie$partie_gagnee <- TRUE
      etat_partie$message_texte <- "★ MAGNIFIQUE — PUZZLE RÉSOLU ★"
      etat_partie$classe_statut <- "texte-statut gagne"
      showNotification("Victoire !", type = "message")
    } else {
      etat_partie$message_texte <- paste(total_erreurs, "erreur(s) restante(s).")
      etat_partie$classe_statut <- "texte-statut erreur"
      showNotification(paste("Il reste", total_erreurs, "erreurs."), type = "warning")
    }
  })
  
  # AFFICHAGE : Texte de statut dans la barre de contrôle (haut)
  output$affichage_message_statut <- renderUI({
    div(class = etat_partie$classe_statut, etat_partie$message_texte)
  })
  
  # AFFICHAGE : Texte de statut dans le bandeau fixe du bas (même info, autre emplacement)
  output$affichage_statut_bas <- renderUI({
    div(class = etat_partie$classe_statut, etat_partie$message_texte)
  })
  
  # DESSIN : La fonction qui dessine tout le plateau (appelée à chaque clic)
  output$dessin_jeu <- renderPlot({
    req(etat_partie$donnees_grille) # Vérifie qu'il y a des données à dessiner
    
    nb_lignes   <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # Création du fond vide — aligné avec la couleur "--bg-card" du thème CSS
    par(mar = c(0, 0, 0, 0), bg = "#111d35")
    plot(0, 0, type = "n",
         xlim = c(0.5, nb_colonnes + 1.5),
         ylim = c(0.5, nb_lignes + 1.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "")
    
    # COUCHE 1 : Les cibles (Aide visuelle - les petites croix grises)
    if(input$case_afficher_cibles) {
      # Horizontales
      for(l in 1:(nb_lignes+1)) {
        for(c in 1:nb_colonnes) {
          points(c + 0.5, (nb_lignes + 2) - l, pch = 3, col = "#1e3060", cex = 0.5)
        }
      }
      # Verticales
      for(l in 1:nb_lignes) {
        for(c in 1:(nb_colonnes+1)) {
          points(c, (nb_lignes + 2) - l - 0.5, pch = 3, col = "#1e3060", cex = 0.5)
        }
      }
    }
    
    # COUCHE 2 : Les "poteaux" (points de coin) de la grille — cyan néon discret
    grille_x <- rep(1:(nb_colonnes+1), each = nb_lignes+1)
    grille_y <- rep((nb_lignes+1):1, times = nb_colonnes+1)
    points(grille_x, grille_y, pch = 19, col = "#00f5d4", cex = 0.9)
    
    # COUCHE 3 : Les Chiffres au centre des cases
    # On utilise chiffres_visibles (qui peut contenir des NA) pour l'affichage,
    # mais chiffres (solution complète) pour vérifier si le compte est bon.
    for(l in 1:nb_lignes) {
      for(c in 1:nb_colonnes) {
        chiffre_attendu <- etat_partie$donnees_grille$chiffres[l, c]  # Valeur réelle (toujours connue)
        chiffre_visible <- etat_partie$chiffres_visibles[l, c]        # Valeur affichée (peut être NA)
        
        # Combien de traits le joueur a-t-il dessiné autour de cette case précise ?
        traits_autour_joueur <- etat_partie$traits_joueur_h[l, c] +
          etat_partie$traits_joueur_h[l+1, c] +
          etat_partie$traits_joueur_v[l, c] +
          etat_partie$traits_joueur_v[l, c+1]
        
        # On n'affiche le chiffre que si la case n'a pas été masquée (non-NA)
        if(!is.na(chiffre_visible)) {
          # Le chiffre change de couleur selon qu'on a le bon compte ou non
          couleur_texte <- "#cdd9f5"                                       # Blanc bleuté par défaut
          if(traits_autour_joueur == chiffre_attendu) couleur_texte <- "#00f5d4" # Cyan néon (OK !)
          if(traits_autour_joueur >  chiffre_attendu) couleur_texte <- "#ff3b5c" # Rouge néon (trop !)
          
          text(c + 0.5, (nb_lignes+1) - l + 0.5, chiffre_visible,
               col = couleur_texte, cex = 1.8, font = 2, family = "sans")
        }
      }
    }
    
    # COUCHE 4 : Les traits dessinés par le joueur
    epaisseur_trait_actif   <- 5
    epaisseur_trait_inactif <- 1
    
    # Dessin des traits horizontaux
    for(l in 1:(nb_lignes+1)) {
      for(c in 1:nb_colonnes) {
        hauteur_y <- (nb_lignes + 2) - l
        
        # Si le joueur a cliqué ici (valeur = 1)
        if(etat_partie$traits_joueur_h[l, c] == 1) {
          couleur_trait <- "#00f5d4" # Cyan néon
          # Si le mode "afficher les erreurs" est actif et que ce trait est faux
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_h[l, c] == 0) {
            couleur_trait <- "#ff3b5c" # Rouge néon
          }
          segments(c, hauteur_y, c+1, hauteur_y, lwd = epaisseur_trait_actif, col = couleur_trait)
        } else {
          # Trait inactif (fantôme sombre)
          segments(c, hauteur_y, c+1, hauteur_y, lwd = epaisseur_trait_inactif, col = "#1e3060", lty = 1)
        }
      }
    }
    
    # Dessin des traits verticaux
    for(l in 1:nb_lignes) {
      for(c in 1:(nb_colonnes+1)) {
        hauteur_y <- (nb_lignes + 2) - l
        
        if(etat_partie$traits_joueur_v[l, c] == 1) {
          couleur_trait <- "#00f5d4" # Cyan néon
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_v[l, c] == 0) {
            couleur_trait <- "#ff3b5c" # Rouge néon
          }
          segments(c, hauteur_y, c, hauteur_y-1, lwd = epaisseur_trait_actif, col = couleur_trait)
        } else {
          segments(c, hauteur_y, c, hauteur_y-1, lwd = epaisseur_trait_inactif, col = "#1e3060", lty = 1)
        }
      }
    }
  })
}

shinyApp(ui, server)