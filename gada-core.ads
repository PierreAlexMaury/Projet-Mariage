with GLib ;

package GAda.Core is

   -- Affiche le message sans passer � la ligne
   procedure Put (Item : in String) ;

   -- Affiche le message et passe � la ligne
   procedure Put_Line (Item : in String) ;

   -- Passe � la ligne
   procedure New_Line ;

   -- Lit et retourne une cha�ne
   function FGet return String ;

   -- Lit une cha�ne (version originale)
   procedure Get (Item : out String) ;

   -- Pour les caract�res
   procedure Put (Car : in Character) ;
   procedure Put_Line(Car : in Character) ;

   -- Pour les entiers
   procedure IPut (Item : in Integer) ;
   function FIGet return Integer ;

   -- Pour les r�els
   procedure FPut (Item : in Float) ;
   function FFGet return Float ;

   -- Pour les erreurs
   procedure Put_Err (Item : in String) ;

   procedure Replace (Item : in String) ;

   -- GAda.Core est thread-safe


   --
   -- Proc�dures de synchronisation
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

