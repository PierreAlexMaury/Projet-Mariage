package graphic is

   subtype Nbr_Couple is Integer range 1..6;
   type T_Voeux is array (Integer range <>) of Character;--tableau qui permet de rangé les voeux de chaque indi
   Image_Manquante:exception;
  
   procedure Image_Blanche;--base du programme pour y insérer tout l'aspect graphique par dessus

   procedure Intro;--texte introductif

   procedure Indication(I:in positive);--donne à l'utili des indications sur ce qu'il doit faire pour utiliser le programme

   procedure Enleve_Indication;

   procedure Aurevoir;--animation 

   procedure Afficher_Etat(B1,B2: in Boolean);--permet de savoir si les hommes choisissent ou si les femmes rejetent

   procedure Bouton_Direct(Appuye, off : Boolean);                    --   

   procedure Bouton_PaP(Appuye, off : Boolean);    
                   --  
   procedure Bouton_Fleche(Appuye : Boolean);                         --affichage des différents boutons

   procedure Bouton_Recommencer(Appuye, off : Boolean);               --

   procedure Bouton_Quitter(Appuye : Boolean);                        --

   procedure Bouton_Num(Appuye, off : Boolean; Lequel : Nbr_Couple);  --

   procedure Dim_im;--dimensionne la fenêtre graphique

   procedure Afficher_Deux_Ronds(X,Y: in Nbr_Couple);

   procedure Afficher_Couple (X: in Nbr_Couple);--affiche le nombre de future couples voulus

   procedure Afficher_Voeux (Voeux: in T_Voeux; Individu: in Character);--affiche les voeux de chaque indiv 

   procedure Trace_Trait (X,Y: in Nbr_Couple);--relit deux indivs entre-eux

   procedure Trace_Trait_anim (X,Y: in Nbr_Couple);--avec un fondu ici

   procedure Enlever_trait(X,Y: in Nbr_Couple);

   procedure Afficher_Erreur;--si il manque une image une fenêtre d'erreur apparaitra

end Graphic;
