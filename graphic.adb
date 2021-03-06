with GAda.Advanced_Graphics, GAda.Graphics, Ada.Text_io;
use GAda.Advanced_Graphics, GAda.Graphics, Ada.Text_io;

package body Graphic is

   procedure Dim_Im is
   begin
      Resize(1000,600);
      Avec_Marge(False);
   end Dim_Im;

   procedure Afficher_Erreur is
   begin
      Resize(500,300);
      Avec_Marge(False);
      Disque(250,150,100,(250,0,0));
      ColorRectangle(180,140,140,30,(250,250,250));
      BlackPoint(-1,-1);--on repasse le noir en coul par default
      Put_Text(7,30,"Le programme va s'arreter car une ou plusieurs images manquent",11);
   end Afficher_Erreur;

   procedure Intro is
   begin
      Put_Text(15,580,"Bonjour,",16);
      Put_Text(15,530,"Ce programme vous permet de realiser des couples a partir d'une",16);
      Put_Text(15,500,"population d'hommes et de femmes donnee. Les mariages sont effectues",16);
      Put_Text(15,470,"grace a la methode de Gale et Shapley.",16);
      Put_Text(15,420,"Cette population presente un nombre de femmes et d'hommes egal, ou",16);
      Put_Text(15,390,"chaque individu definit une liste de preferences, contenant tous les",16);
      Put_Text(15,360,"individus du sexe oppose.",16);
      Put_Text(15,310,"Vous pouvez a tout moment quitter ou recommencer le programme en",16);
      Put_Text(15,280,"appuyant sur le bouton correspondant en bas a droite.",16);
      Put_Text(15,230,"<< Le mariage est une loterie >>",16);
      Put_Text(15,200,"Ben Jonson (1572,1637), Dramaturge anglais de la Renaissance.",16);
      Put_Text(15,25,"M.Prevost Remi, M.Maury Pierre-Alexandre. INSA de Toulouse, 2013.",15);
   end Intro;

   procedure Indication(I:in positive)is   --suivant le nombre choisi l'indication est différente(géré dans le client)
   begin
      if I=1 then
         Put_Text(817,310,"Choisissez",13);
         Put_Text(847,290,"le nombre",13);
         Put_Text(877,270,"de couples",13);
         Put_Text(907,250,"a former",13);
      elsif I=2 then
         Put_Text(817,310,"Bouton 'Direct':",12);
         Put_Text(847,290,"donne le resultat",12);
         Put_Text(817,260,"Bouton 'Pas a Pas':",12);
         Put_Text(847,240,"affiche les etapes",12);
      else
         Put_Text(817,310,"Appuyez sur la",13);
         Put_Text(847,290,"fleche pour",13);
         Put_Text(877,270,"voir l'etape",13);
         Put_Text(907,250,"suivante",13);
      end if;
   end Indication;

   procedure Enleve_Indication is
   begin
      ColorRectangle(817,151,173,189,(255,255,255));
      ColorRectangle(-817,-500,173,189,(0,0,0));
   end Enleve_Indication;

   procedure Afficher_Etat(B1,B2: in Boolean) is
   begin
      if B1 then
         Put_Text(255,580,"Les hommes font leur choix",15);
      else
        Put_Text(165,580,"Les femmes choisissent la meilleure proposition",15);
      end if;
      if B2 then
         ColorRectangle(165,558,465,22,(255,255,255)); 
      end if;
   end Afficher_Etat;

   procedure Image_Blanche is
   begin
      Put_Image(0,600,Load_Image("images/image blanche.png"));
      exception when Program_Error => raise Image_Manquante;  --exception levée si image manquante
   end Image_Blanche;

   procedure Aurevoir is
   begin
       for I in 1..4 loop
         Put_Image(0,600,Load_Image("images/aurevoir1.jpg"));
         delay 0.5;
         Put_Image(0,600,Load_Image("images/aurevoir2.jpg"));
         delay 0.5;
       end loop;
   exception when Program_Error => raise Image_Manquante;
   end Aurevoir;

   procedure Bouton_Direct(Appuye, off : Boolean) is
   begin
      if Off then Put_Image(817,570,Load_Image("boutons/Bouton_Direct_off.png"));
      elsif Appuye then Put_Image(817,570,Load_Image("boutons/Bouton_Direct_in.png"));
      else Put_Image(817,570,Load_Image("boutons/Bouton_Direct_out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;
   end Bouton_Direct;

   procedure Bouton_PaP(Appuye, off : Boolean) is
   begin
      if Off then Put_Image(817,505,Load_Image("boutons/Bouton_PaP_off.png"));
      elsif Appuye then Put_Image(817,505,Load_Image("boutons/Bouton_PaP_in.png"));
      else Put_Image(817,505,Load_Image("boutons/Bouton_PaP_out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;
   end Bouton_PaP;

   procedure Bouton_Fleche(Appuye : Boolean) is
   begin
      if Appuye then Put_Image(817,505,Load_Image("boutons/Bouton_Fleche_in.png"));
      else Put_Image(817,505,Load_Image("boutons/Bouton_Fleche_out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;
   end Bouton_Fleche;

   procedure Bouton_Recommencer(Appuye, off : Boolean) is
   begin
      if Off then Put_Image(817,150,Load_Image("boutons/Bouton_recommencer_off.png"));
      elsif Appuye then Put_Image(817,150,Load_Image("boutons/Bouton_recommencer_in.png"));
      else Put_Image(817,150,Load_Image("boutons/Bouton_recommencer_out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;
   end Bouton_Recommencer;

   procedure Bouton_Quitter(Appuye : Boolean) is
   begin
      if Appuye then Put_Image(817,85,Load_Image("boutons/Bouton_Quitter_In.png"));
      else Put_Image(817,85,Load_Image("boutons/Bouton_Quitter_Out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;
   end Bouton_Quitter;

   procedure Bouton_Num(Appuye, off : Boolean; Lequel : Nbr_couple) is
   begin
      if Off then
         Put_Image(817,440,Load_Image("boutons/Bouton_1_off.png"));
         Put_Image(882,440,Load_Image("boutons/Bouton_2_off.png"));
         Put_Image(947,440,Load_Image("boutons/Bouton_3_off.png"));
         Put_Image(817,385,Load_Image("boutons/Bouton_4_off.png"));
         Put_Image(882,385,Load_Image("boutons/Bouton_5_off.png"));
         Put_Image(947,385,Load_Image("boutons/Bouton_6_off.png"));
      elsif Appuye then
         case Lequel is
            when 1 => Put_Image(817,440,Load_Image("boutons/Bouton_1_in.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
            when 2 => Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_in.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
            when 3 => Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_in.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
            when 4 => Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_in.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
            when 5 => Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_in.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
            when 6 => Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
               Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
               Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
               Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
               Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
               Put_Image(947,385,Load_Image("boutons/Bouton_6_in.png"));
         end case;
      else
         Put_Image(817,440,Load_Image("boutons/Bouton_1_out.png"));
         Put_Image(882,440,Load_Image("boutons/Bouton_2_out.png"));
         Put_Image(947,440,Load_Image("boutons/Bouton_3_out.png"));
         Put_Image(817,385,Load_Image("boutons/Bouton_4_out.png"));
         Put_Image(882,385,Load_Image("boutons/Bouton_5_out.png"));
         Put_Image(947,385,Load_Image("boutons/Bouton_6_out.png"));
      end if;
   exception when Program_Error => raise Image_Manquante;

   end Bouton_Num;

   procedure Afficher_Deux_Ronds(X,Y: in Nbr_Couple) is
   begin
      case X is
            when 1 => Disque(110,525,6,(0,0,200));
            when 2 => Disque(110,435,6,(0,0,200));
            when 3 => Disque(110,345,6,(0,0,200));
            when 4 => Disque(110,255,6,(0,0,200));
            when 5 => Disque(110,165,6,(0,0,200));
            when 6 => Disque(110,75,6,(0,0,200));
      end case;

      case Y is
            when 1 => Disque(690,525,6,(255,0,200));
            when 2 => Disque(690,435,6,(255,0,200));
            when 3 => Disque(690,345,6,(255,0,200));
            when 4 => Disque(690,255,6,(255,0,200));
            when 5 => Disque(690,165,6,(255,0,200));
            when 6 => Disque(690,75,6,(255,0,200));
         end case;
   end Afficher_Deux_Ronds;

   procedure Afficher_Couple(X:Nbr_Couple) is
      I:Nbr_couple:=1;
   begin
      loop
         case I is
            when 1 => Put_Image(50,550,Load_Image("images/homme.jpg"));
               Disque(110,525,6,(0,0,200));
               Put_Text(50,535,"A",15);--lettre homme
               Put_Image(700,550,Load_Image("images/femme.jpg"));
               Disque(690,525,6,(255,0,200));
               Put_Text(739,540,"a",15);--lettre femme


            when 2 => Put_Image(50,460,Load_Image("images/homme.jpg"));
               Disque(110,435,6,(0,0,200));
               Put_Text(50,445,"B",15);
               Put_Image(700,460,Load_Image("images/femme.jpg"));
               Disque(690,435,6,(255,0,200));
               Put_Text(739,450,"b",15);


            when 3 => Put_Image(50,370,Load_Image("images/homme.jpg"));
               Disque(110,345,6,(0,0,200));
               Put_Text(50,355,"C",15);
               Put_Image(700,370,Load_Image("images/femme.jpg"));
               Disque(690,345,6,(255,0,200));
               Put_Text(739,360,"c",15);


            when 4 => Put_Image(50,280,Load_Image("images/homme.jpg"));
               Disque(110,255,6,(0,0,200));
               Put_Text(50,265,"D",15);
               Put_Image(700,280,Load_Image("images/femme.jpg"));
               Disque(690,255,6,(255,0,200));
               Put_Text(739,270,"d",15);


            when 5 => Put_Image(50,190,Load_Image("images/homme.jpg"));
               Disque(110,165,6,(0,0,200));
               Put_Text(50,175,"E",15);
               Put_Image(700,190,Load_Image("images/femme.jpg"));
               Disque(690,165,6,(255,0,200));
               Put_Text(739,180,"e",15);


            when 6 => Put_Image(50,100,Load_Image("images/homme.jpg"));
               Disque(110,75,6,(0,0,200));
               Put_Text(50,85,"F",15);
               Put_Image(700,100,Load_Image("images/femme.jpg"));
               Disque(690,75,6,(255,0,200));
               Put_Text(739,90,"f",15);
         end case;
         exit when i=X;
         I:=I+1;
      end loop;
   exception when Program_Error => raise Image_Manquante;
   end Afficher_Couple;

   procedure Trace_Trait (X,Y: in Nbr_Couple) is
      Depx,Depy,Finx,finy:Integer;
   begin
      Depx:=110;
      FinX:=690;
      case X is
         when 1 => Depy:=525;
         when 2 => Depy:=435;
         when 3 => Depy:=345;
         when 4 => Depy:=255;
         when 5 => Depy:=165;
         when 6 => Depy:=75;
      end case;

      case Y is
         when 1 => Finy:=525;
         when 2 => Finy:=435;
         when 3 => Finy:=345;
         when 4 => Finy:=255;
         when 5 => Finy:=165;
         when 6 => Finy:=75;
      end case;

      colorLine(depx,depy,finx,Finy,(0,0,0));

      case X is
         when 1 => Depy:=524;
         when 2 => Depy:=434;
         when 3 => Depy:=344;
         when 4 => Depy:=254;
         when 5 => Depy:=164;
         when 6 => Depy:=74;
      end case;

      case Y is
         when 1 => Finy:=524;
         when 2 => Finy:=434;
         when 3 => Finy:=344;
         when 4 => Finy:=254;
         when 5 => Finy:=164;
         when 6 => Finy:=74;
      end case;

      colorLine(depx,depy,finx,Finy,(0,0,0));

   end Trace_Trait;

   procedure Trace_Trait_anim (X,Y: in Nbr_Couple) is
      Depx,Depy,Finx,finy:Integer;
   begin
      Depx:=110;
      FinX:=690;
      case X is
         when 1 => Depy:=525;
         when 2 => Depy:=435;
         when 3 => Depy:=345;
         when 4 => Depy:=255;
         when 5 => Depy:=165;
         when 6 => Depy:=75;
      end case;

      case Y is
         when 1 => Finy:=525;
         when 2 => Finy:=435;
         when 3 => Finy:=345;
         when 4 => Finy:=255;
         when 5 => Finy:=165;
         when 6 => Finy:=75;
      end case;

      for I in 0..255 loop
         delay 0.001;
         colorLine(depx,depy,finx,Finy,(255-I,255-I,255-I));
      end loop;

      case X is
         when 1 => Depy:=524;
         when 2 => Depy:=434;
         when 3 => Depy:=344;
         when 4 => Depy:=254;
         when 5 => Depy:=164;
         when 6 => Depy:=74;
      end case;

      case Y is
         when 1 => Finy:=524;
         when 2 => Finy:=434;
         when 3 => Finy:=344;
         when 4 => Finy:=254;
         when 5 => Finy:=164;
         when 6 => Finy:=74;
      end case;

      for I in 0..255 loop
         delay 0.001;
         colorLine(depx,depy,finx,Finy,(255-I,255-I,255-I));
      end loop;

   end Trace_Trait_anim;

   procedure Enlever_trait(X,Y: in Nbr_Couple) is
      Depx,Depy,Finx,finy:Integer;
   begin
      Depx:=110;
      FinX:=690;
      case X is
         when 1 => Depy:=525;
         when 2 => Depy:=435;
         when 3 => Depy:=345;
         when 4 => Depy:=255;
         when 5 => Depy:=165;
         when 6 => Depy:=75;
      end case;

      case Y is
         when 1 => Finy:=525;
         when 2 => Finy:=435;
         when 3 => Finy:=345;
         when 4 => Finy:=255;
         when 5 => Finy:=165;
         when 6 => Finy:=75;
      end case;

      for I in 0..255 loop
         delay 0.001;
         colorLine(depx,depy,finx,Finy,(I,I,I));
         Afficher_Deux_Ronds(X,Y);
      end loop;

      case X is
         when 1 => Depy:=524;
         when 2 => Depy:=434;
         when 3 => Depy:=344;
         when 4 => Depy:=254;
         when 5 => Depy:=164;
         when 6 => Depy:=74;
      end case;

      case Y is
         when 1 => Finy:=524;
         when 2 => Finy:=434;
         when 3 => Finy:=344;
         when 4 => Finy:=254;
         when 5 => Finy:=164;
         when 6 => Finy:=74;
      end case;

      for I in 0..255 loop
         delay 0.001;
         colorLine(depx,depy,finx,Finy,(I,I,I));
         Afficher_Deux_Ronds(X,Y);
      end loop;
   end Enlever_Trait;

   procedure Afficher_Voeux (voeux: in T_Voeux; Individu:in character) is
   begin
      BlackPoint(-1,-1);
      if Individu = 'A' then
         Put_Text(10,580,"(",15);
         for I in Voeux'Range loop
            case i is
               when 1 => Put_Text(15,580,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,580,")",15); end if;
               when 2 => Put_Text(35,580,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,580,")",15);end if;
               when 3 => Put_Text(55,580,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,580,")",15);end if;
               when 4 => Put_Text(75,580,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,580,")",15);end if;
               when 5 => Put_Text(95,580,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,580,")",15);end if;
               when 6 => Put_Text(115,580,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'a' then
         Put_Text(665,580,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(670,580,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,580,")",15); end if;
               when 2 => Put_Text(690,580,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,580,")",15); end if;
               when 3 => Put_Text(710,580,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,580,")",15); end if;
               when 4 => Put_Text(730,580,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,580,")",15); end if;
               when 5 => Put_Text(750,580,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,580,")",15); end if;
               when 6 => Put_Text(770,580,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'B' then
         Put_Text(10,490,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(15,490,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,490,")",15); end if;
               when 2 => Put_Text(35,490,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,490,")",15);end if;
               when 3 => Put_Text(55,490,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,490,")",15);end if;
               when 4 => Put_Text(75,490,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,490,")",15);end if;
               when 5 => Put_Text(95,490,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,490,")",15);end if;
               when 6 => Put_Text(115,490,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'b' then
         Put_Text(665,490,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(670,490,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,490,")",15); end if;
               when 2 => Put_Text(690,490,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,490,")",15); end if;
               when 3 => Put_Text(710,490,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,490,")",15); end if;
               when 4 => Put_Text(730,490,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,490,")",15); end if;
               when 5 => Put_Text(750,490,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,490,")",15); end if;
               when 6 => Put_Text(770,490,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'C' then
         Put_Text(10,400,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(15,400,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,400,")",15); end if;
               when 2 => Put_Text(35,400,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,400,")",15);end if;
               when 3 => Put_Text(55,400,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,400,")",15);end if;
               when 4 => Put_Text(75,400,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,400,")",15);end if;
               when 5 => Put_Text(95,400,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,400,")",15);end if;
               when 6 => Put_Text(115,400,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'c' then
         Put_Text(665,400,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(670,400,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,400,")",15); end if;
               when 2 => Put_Text(690,400,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,400,")",15); end if;
               when 3 => Put_Text(710,400,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,400,")",15); end if;
               when 4 => Put_Text(730,400,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,400,")",15); end if;
               when 5 => Put_Text(750,400,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,400,")",15); end if;
               when 6 => Put_Text(770,400,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'D' then
         Put_Text(10,310,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(15,310,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,310,")",15); end if;
               when 2 => Put_Text(35,310,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,310,")",15);end if;
               when 3 => Put_Text(55,310,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,310,")",15);end if;
               when 4 => Put_Text(75,310,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,310,")",15);end if;
               when 5 => Put_Text(95,310,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,310,")",15);end if;
               when 6 => Put_Text(115,310,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'd' then
         Put_Text(665,310,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => put_Text(670,310,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,310,")",15); end if;
               when 2 => Put_Text(690,310,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,310,")",15); end if;
               when 3 => Put_Text(710,310,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,310,")",15); end if;
               when 4 => Put_Text(730,310,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,310,")",15); end if;
               when 5 => Put_Text(750,310,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,310,")",15); end if;
               when 6 => Put_Text(770,310,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'E' then
         Put_Text(10,220,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(15,220,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,220,")",15); end if;
               when 2 => Put_Text(35,220,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,220,")",15);end if;
               when 3 => Put_Text(55,220,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,220,")",15);end if;
               when 4 => Put_Text(75,220,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,220,")",15);end if;
               when 5 => Put_Text(95,220,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,220,")",15);end if;
               when 6 => Put_Text(115,220,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'e' then
         Put_Text(665,220,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(670,220,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,220,")",15); end if;
               when 2 => Put_Text(690,220,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,220,")",15); end if;
               when 3 => Put_Text(710,220,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,220,")",15); end if;
               when 4 => Put_Text(730,220,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,220,")",15); end if;
               when 5 => Put_Text(750,220,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,220,")",15); end if;
               when 6 => Put_Text(770,220,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'F' then
         Put_Text(10,130,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(15,130,Voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(26,130,")",15); end if;
               when 2 => Put_Text(35,130,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(46,130,")",15);end if;
               when 3 => Put_Text(55,130,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(66,130,")",15);end if;
               when 4 => Put_Text(75,130,voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(86,130,")",15);end if;
               when 5 => Put_Text(95,130,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(106,130,")",15);end if;
               when 6 => Put_Text(115,130,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;

      if Individu = 'f' then
         Put_Text(665,130,"(",15);
         for i in voeux'Range loop
            case i is
               when 1 => Put_Text(670,130,voeux(1)&",",15);
                  if 1 = voeux'Last then Put_Text(683,130,")",15); end if;
               when 2 => Put_Text(690,130,voeux(2)&",",15);
                  if 2 = voeux'Last then Put_Text(703,130,")",15); end if;
               when 3 => Put_Text(710,130,voeux(3)&",",15);
                  if 3 = voeux'Last then Put_Text(723,130,")",15); end if;
               when 4 => Put_Text(730,130,Voeux(4)&",",15);
                  if 4 = voeux'Last then Put_Text(743,130,")",15); end if;
               when 5 => Put_Text(750,130,voeux(5)&",",15);
                  if 5 = voeux'Last then Put_Text(763,130,")",15); end if;
               when 6 => Put_Text(770,130,voeux(6)&")",15);
               when others => null;
            end case;
         end loop;
      end if;
   end Afficher_Voeux;

end Graphic;
