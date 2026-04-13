library(shiny)
library(bslib)

# ==============================================================================
# 1. MOTEUR DE GÉNÉRATION (Création du puzzle)
# ==============================================================================
generer_grille_slitherlink <- function(nb_lignes = 5, nb_colonnes = 5, complexite = 0.6) {
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
  
  print("--> [GÉNÉRATION] Terminé avec succès !")
  # On renvoie tout ce qu'on a fabriqué
  return(list(
    nb_lignes = nb_lignes, 
    nb_colonnes = nb_colonnes, 
    solution_h = solution_horizontale, 
    solution_v = solution_verticale, 
    chiffres = chiffres_indices
  ))
}

# ==============================================================================
# 2. INTERFACE UTILISATEUR (Ce qui s'affiche à l'écran)
# ==============================================================================
theme_visuel <- bs_theme(
  version = 5,
  bootswatch = "darkly", 
  primary = "#e74c3c", # Rouge
  base_font = font_google("Roboto")
)

ui <- fluidPage(
  theme = theme_visuel,
  
  # Un peu de CSS pour embellir les boîtes et la souris
  tags$head(
    tags$style(HTML("
      .curseur-cible { cursor: crosshair; } /* La souris devient une croix de visée */
      .boite-graphique { background-color: #2b2b2b; border: 1px solid #444; border-radius: 10px; padding: 15px; }
    "))
  ),
  
  titlePanel("Slitherlink : Édition Française"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Commandes"),
      sliderInput("entree_taille", "Taille de la grille", min = 4, max = 20, value = 5),
      # Curseur pour choisir la proportion d'indices affichés (100% = tous les chiffres, mode facile)
      sliderInput("entree_indices", "Indices affichés (%)", min = 10, max = 100, value = 60, step = 5),
      actionButton("bouton_nouveau", "Nouveau Jeu", icon = icon("sync"), class = "btn-primary w-100"),
      hr(),
      h4("Aides Visuelles"),
      checkboxInput("case_afficher_cibles", "Afficher les cibles (croix grises)", value = TRUE),
      checkboxInput("case_afficher_erreurs", "Surligner les erreurs en rouge", value = FALSE),
      hr(),
      actionButton("bouton_verifier", "Vérifier ma solution", class = "btn-success w-100"),
      br(), br(),
      uiOutput("affichage_message_statut")
    ),
    
    mainPanel(
      width = 9,
      div(class = "boite-graphique curseur-cible",
          # C'est ici qu'on capte les clics ("clic_souris")
          plotOutput("dessin_jeu", click = "clic_souris", height = "600px")
      )
    )
  )
)

# ==============================================================================
# 3. SERVEUR (Le cerveau de l'application qui réagit à vos clics)
# ==============================================================================
server <- function(input, output, session) {
  
  # Mémoire de l'application : ce qui doit être mis à jour à l'écran
  etat_partie <- reactiveValues(
    donnees_grille = NULL,
    traits_joueur_h = NULL, 
    traits_joueur_v = NULL,
    partie_gagnee = FALSE,
    message_texte = "Cliquez sur les traits gris pour les allumer.",
    # Matrice des chiffres visibles par le joueur (NA = case masquée, le chiffre reste connu en interne)
    chiffres_visibles = NULL
  )
  
  # ACTION : Quand on clique sur le bouton "Nouveau Jeu"
  observeEvent(input$bouton_nouveau, {
    taille <- input$entree_taille
    print(paste("=== NOUVELLE PARTIE DEMANDÉE (Taille", taille, ") ==="))
    
    # On génère la grille et on remet le plateau à zéro
    grille <- generer_grille_slitherlink(taille, taille)
    etat_partie$donnees_grille <- grille
    etat_partie$traits_joueur_h <- matrix(0, taille + 1, taille)
    etat_partie$traits_joueur_v <- matrix(0, taille, taille + 1)
    etat_partie$partie_gagnee <- FALSE
    etat_partie$message_texte <- "Nouvelle partie lancée ! À vous de jouer."
    
    # Masquage aléatoire des chiffres selon le pourcentage d'indices choisi par le joueur
    # On part de la grille complète et on efface (NA) les cases non sélectionnées
    chiffres_masques <- grille$chiffres
    proportion_visible <- input$entree_indices / 100
    nb_cases_total <- taille * taille
    nb_cases_a_cacher <- floor(nb_cases_total * (1 - proportion_visible))
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
    
    nb_lignes <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # Variables pour trouver la croix (cible) la plus proche du clic
    distance_minimale <- Inf
    cible_la_plus_proche <- NULL 
    
    # 1. Vérifier la distance avec tous les traits HORIZONTAUX
    for(ligne in 1:(nb_lignes + 1)) {
      for(colonne in 1:nb_colonnes) {
        centre_x <- colonne + 0.5
        centre_y <- (nb_lignes + 2) - ligne # Astuce: On inverse l'axe Y pour correspondre au dessin
        
        # Théorème de Pythagore pour calculer la distance entre la souris et le centre du trait
        distance <- sqrt((clic_x - centre_x)^2 + (clic_y - centre_y)^2)
        
        if(distance < distance_minimale) {
          distance_minimale <- distance
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
          distance_minimale <- distance
          cible_la_plus_proche <- list(type="vertical", ligne=ligne, colonne=colonne)
        }
      }
    }
    
    # 3. Action finale : Si on a cliqué assez près d'une cible (distance < 0.45)
    if(distance_minimale < 0.45) {
      type <- cible_la_plus_proche$type
      l <- cible_la_plus_proche$ligne
      c <- cible_la_plus_proche$colonne
      
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
    erreurs_verticales <- sum(abs(etat_partie$traits_joueur_v - etat_partie$donnees_grille$solution_v))
    total_erreurs <- erreurs_horizontales + erreurs_verticales
    
    print(paste("Nombre d'erreurs détectées :", total_erreurs))
    
    if(total_erreurs == 0) {
      etat_partie$partie_gagnee <- TRUE
      etat_partie$message_texte <- "🏆 MAGNIFIQUE ! Puzzle Résolu ! 🏆"
      showNotification("Victoire !", type="message")
    } else {
      etat_partie$message_texte <- paste(total_erreurs, "erreurs restantes.")
      showNotification(paste("Il reste", total_erreurs, "erreurs."), type="warning")
    }
  })
  
  # AFFICHAGE : Le texte de statut en bas à gauche
  output$affichage_message_statut <- renderUI({
    h3(etat_partie$message_texte, style="color: #00bc8c; text-align: center;")
  })
  
  # DESSIN : La fonction qui dessine tout le plateau (appelée à chaque clic)
  output$dessin_jeu <- renderPlot({
    req(etat_partie$donnees_grille) # Vérifie qu'il y a des données à dessiner
    
    nb_lignes <- etat_partie$donnees_grille$nb_lignes
    nb_colonnes <- etat_partie$donnees_grille$nb_colonnes
    
    # Création du fond vide
    par(mar=c(0,0,0,0), bg="#2b2b2b")
    plot(0,0, type="n", xlim=c(0.5, nb_colonnes+1.5), ylim=c(0.5, nb_lignes+1.5), asp=1, axes=FALSE, xlab="", ylab="")
    
    # COUCHE 1 : Les cibles (Aide visuelle - les petites croix grises)
    if(input$case_afficher_cibles) {
      # Horizontales
      for(l in 1:(nb_lignes+1)) {
        for(c in 1:nb_colonnes) {
          points(c + 0.5, (nb_lignes + 2) - l, pch=3, col="#444444", cex=0.5)
        }
      }
      # Verticales
      for(l in 1:nb_lignes) {
        for(c in 1:(nb_colonnes+1)) {
          points(c, (nb_lignes + 2) - l - 0.5, pch=3, col="#444444", cex=0.5)
        }
      }
    }
    
    # COUCHE 2 : Les "poteaux" blancs de la grille
    grille_x <- rep(1:(nb_colonnes+1), each=nb_lignes+1)
    grille_y <- rep((nb_lignes+1):1, times=nb_colonnes+1)
    points(grille_x, grille_y, pch=19, col="#bdc3c7", cex=1.2)
    
    # COUCHE 3 : Les Chiffres au centre des cases
    # On utilise chiffres_visibles (qui peut contenir des NA) pour l'affichage,
    # mais chiffres (solution complète) pour vérifier si le compte est bon.
    for(l in 1:nb_lignes) {
      for(c in 1:nb_colonnes) {
        chiffre_attendu <- etat_partie$donnees_grille$chiffres[l,c]   # Valeur réelle (toujours connue)
        chiffre_visible <- etat_partie$chiffres_visibles[l,c]         # Valeur affichée (peut être NA)
        
        # Combien de traits le joueur a-t-il dessiné autour de cette case précise ?
        traits_autour_joueur <- etat_partie$traits_joueur_h[l,c] + 
          etat_partie$traits_joueur_h[l+1,c] + 
          etat_partie$traits_joueur_v[l,c] + 
          etat_partie$traits_joueur_v[l,c+1]
        
        # On n'affiche le chiffre que si la case n'a pas été masquée (non-NA)
        if(!is.na(chiffre_visible)) {
          # Le chiffre change de couleur selon qu'on a le bon compte ou non
          couleur_texte <- "#ecf0f1" # Blanc par défaut
          if(traits_autour_joueur == chiffre_attendu) couleur_texte <- "#2ecc71" # Vert (OK)
          if(traits_autour_joueur > chiffre_attendu) couleur_texte <- "#e74c3c"  # Rouge (Trop de traits !)
          
          text(c+0.5, (nb_lignes+1)-l+0.5, chiffre_visible, col=couleur_texte, cex=2, font=2)
        }
      }
    }
    
    # COUCHE 4 : Les traits dessinés par le joueur
    epaisseur_trait_actif <- 5
    epaisseur_trait_inactif <- 1
    
    # Dessin des traits horizontaux
    for(l in 1:(nb_lignes+1)) {
      for(c in 1:nb_colonnes) {
        hauteur_y <- (nb_lignes+2)-l
        
        # Si le joueur a cliqué ici (valeur = 1)
        if(etat_partie$traits_joueur_h[l,c] == 1) {
          couleur_trait <- "#3498db" # Bleu
          # Si le mode triche "afficher les erreurs" est actif et que ce trait est faux
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_h[l,c] == 0) {
            couleur_trait <- "#e74c3c" # Rouge
          }
          segments(c, hauteur_y, c+1, hauteur_y, lwd=epaisseur_trait_actif, col=couleur_trait)
        } else {
          # Trait inactif (fantôme gris)
          segments(c, hauteur_y, c+1, hauteur_y, lwd=epaisseur_trait_inactif, col="#444444", lty=1) 
        }
      }
    }
    
    # Dessin des traits verticaux
    for(l in 1:nb_lignes) {
      for(c in 1:(nb_colonnes+1)) {
        hauteur_y <- (nb_lignes+2)-l
        
        if(etat_partie$traits_joueur_v[l,c] == 1) {
          couleur_trait <- "#3498db" # Bleu
          if(input$case_afficher_erreurs && etat_partie$donnees_grille$solution_v[l,c] == 0) {
            couleur_trait <- "#e74c3c" # Rouge
          }
          segments(c, hauteur_y, c, hauteur_y-1, lwd=epaisseur_trait_actif, col=couleur_trait)
        } else {
          segments(c, hauteur_y, c, hauteur_y-1, lwd=epaisseur_trait_inactif, col="#444444", lty=1)
        }
      }
    }
  })
}

shinyApp(ui, server)