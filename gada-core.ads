with GLib ;

package GAda.Core is

   -- Affiche le message sans passer à la ligne
   procedure Put (Item : in String) ;

   -- Affiche le message et passe à la ligne
   procedure Put_Line (Item : in String) ;

   -- Passe à la ligne
   procedure New_Line ;

   -- Lit et retourne une chaîne
   function FGet return String ;

   -- Lit une chaîne (version originale)
   procedure Get (Item : out String) ;

   -- Pour les caractères
   procedure Put (Car : in Character) ;
   procedure Put_Line(Car : in Character) ;

   -- Pour les entiers
   procedure IPut (Item : in Integer) ;
   function FIGet return Integer ;

   -- Pour les réels
   procedure FPut (Item : in Float) ;
   function FFGet return Float ;

   -- Pour les erreurs
   procedure Put_Err (Item : in String) ;

   procedure Replace (Item : in String) ;

   -- GAda.Core est thread-safe


   --
   -- Procédures de synchronisation
   -- (pour les utilisateurs avertis)
   --
   procedure Enter ;
   procedure Leave ;

   function Convert (Arg : String) return Glib.UTF8_String ;
   function InvConvert (Arg : Glib.UTF8_String) return String ;

   procedure Abort_Gtk_Loop ;


   Boucle_Infinie : exception ;
   Fenetre_Fermee : exception ;

end GAda.Core ;

