with Graphic; use Graphic;
with Ada.Calendar; use Ada.Calendar;

generic

   Nb_Couple : Positive;
   Initial : Integer;

package Population is

   Init : Integer := Integer(Seconds(Clock));

   Numero_Inconnu : exception;

   subtype Voeu is Positive range 1..Nb_Couple;

   type T_Population is limited private;

   --type T_Voeux is array(1..Nb_Couple) of Character;

    -- initialise la matrice Population avec des valeurs aléatoires répondant aux conditions de Gale et Shapley et en initialisant tous les lies à true et les autres à false.
   Procedure initialise_population(Couples : in out T_Population);

   -- retourne les vœux de Homme sous forme d’un tableau qui contient les lettres des femmes par ordre de préférence de 1 à nb_Couples.
   function Get_Voeux_Homme(Population : in T_Population ; Homme : in voeu) Return T_Voeux;


    -- retourne les vœux de Femme sous forme d’un tableau qui contient les lettres des hommes par ordre de préférence de 1 à nb_Couples.
   function Get_Voeux_Femme(Population : in T_Population ; Femme : in voeu) return T_Voeux;


   -- Donne le classement de la femme "Femme" pour l'homme "Homme"
   function Get_Ponderation_Homme(Population : in T_Population ; Homme, Femme : in voeu) return Voeu;


   -- Donne le classement de l'homme "Homme pour la femme "Femme"
   function Get_Ponderation_Femme(Population : in T_Population ; Homme, Femme : in voeu) return Voeu;


   -- Donne le nombre de couple
   function Get_Nb_Couple return Voeu;


   -- Indique si l'homme "Homme" et la femme "Femme" sont lies
   function Get_Lies(Population : in T_Population ; Homme, Femme : in voeu) return Boolean;


   -- Indique si l'homme "Homme a été rejeté par la femme "Femme"
   function Get_Rejete(Population : in T_Population ; Homme, Femme : in voeu) return Boolean;


   -- Indique si l'homme "Homme" et la femme "Femme" sont temporairement lies
   function Get_Temp(Population : in T_Population ; Homme, Femme : in voeu) return Boolean;


   -- Indique si l'homme "Homme" et la femme "Femme" étaient liés à l'étape précédente
   function Get_Ex(Population : in T_Population ; Homme, Femme : in voeu) return Boolean;


   -- Lie ou délie les deux individus "Homme" et "Femme"
   procedure Set_Lies(Population : in out T_Population ; Homme, Femme : in Voeu; Valeur : boolean);


   -- Rejete l'homme "Homme" pour la femme "Femme"
   procedure Set_Rejete(Population : in out T_Population ; Homme, Femme : in voeu; Valeur : boolean);


   -- Lie ou délie temporairement les deux individus "Homme" et "Femme"
   procedure Set_Temp(Population : in out T_Population ; Homme, Femme : in voeu; Valeur : boolean);


   -- Lie ou délie les deux individus "Homme" et "Femme" pour l'étape suivante
   procedure Set_Ex(Population : in out T_Population ; Homme, Femme : in voeu; Valeur : boolean);

   -- Affiche la Matrice des préférences.
   procedure To_String(Couples : in T_Population);


   -- Donne la lettre de l'homme associé au nombre donné
   function Nombre_Personne_Homme(Personne : Integer) return Character;


   -- Donne la lettre de la femme associé au nombre donné
   function Nombre_Personne_Femme(Personne : Integer) return Character;

private

   type T_Individu is record
      Voeu_Homme : Voeu;
      Voeu_Femme : Voeu;
      Lies : Boolean;
      Rejete : Boolean;
      Temp : Boolean;
      Ex : Boolean;
   end record;

   type T_Population is array (1..Nb_Couple,1..Nb_Couple) of T_Individu;

end Population;
