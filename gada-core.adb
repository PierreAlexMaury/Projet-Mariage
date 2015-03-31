with Uncaught_Exception ;

pragma Warnings (Off);
with Ada.Interrupts.Signal ;
with Ada.Interrupts.Names ;
pragma Warnings (On);
--
-- with System.Tasking;
--
-- Self := System.Tasking.Self ;
-- Parent := Self.Common.Parent ;

with System ;
with Ada.Task_Identification ;
with Ada.Command_Line ;
with Ada.Exceptions ;
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO ;
with Ada.Strings.Fixed ;
with Ada.Calendar ; use Ada.Calendar ;
with Glib ; use Glib ;
with Glib.Convert ;
with Glib.Properties ;
with Glib.Error, Glib.Values, Glib.Unicode ;
with Gdk.Threads ;
with Gdk.Color ;
with Gtk.Enums, Pango.Enums ; use Gtk.Enums, Pango.Enums ;
with Gtk.Main, Gtk.Widget, Gtk.Handlers  ;
with Gtk.Window, Gtk.Frame, Gtk.Box , Gtk.Scrolled_Window, Gtk.Label, Gtk.Button ;
with Gtk.Text_View, Gtk.Text_Buffer, Gtk.Text_Iter, Gtk.Text_Mark, Gtk.Text_Tag_Table, Gtk.Text_Tag ;
with Gtk.Toolbar, Gtk.Stock ;
with Gtk.Clipboard ;
with GtkAda.Dialogs ;
with Pango.Font ;
with Ada.Command_Line ;

package body GAda.Core is

   -- Exception raised when GET is not possible
   GetForbidden : exception ;

   -- Configuration
   Console_Encoding : String := "latin-9" ; -- xterm encoding, not necessarily the locale encoding
   Emacs_Encoding   : String := "latin-9" ; -- emacs encoding, not necessarily the locale encoding

   -- We check the status of the main program frequently (check for exception or termination)
   Program_Check_Interval : Guint32 := 500 ; -- Milliseconds


   -- Get ID of main task (used to check if this task has finished).
   Main_Task_Id : Ada.Task_Identification.Task_ID := Ada.Task_Identification.Current_Task ;
   Main_Task_Finished : Boolean ; -- Main application is known to be finished

   type Reason is (None, Closed, Return_Get) ;
   Reason_For_Exit : Reason ;

   Exit_Gtk_When_Finished : Boolean := False ;

   ---------------------------------------------------------------------
   --                CONFIGURATION of the GUI                         --
   ---------------------------------------------------------------------

   --
   -- All the widgets and more
   --
   Main_Window     : Gtk.Window.Gtk_Window;
   Main_Vbox       : Gtk.Box.Gtk_Box;

   -- Text Buffer
   Buffer          : Gtk.Text_Buffer.Gtk_Text_Buffer ;
   IterEnd         : Gtk.Text_Iter.Gtk_Text_Iter ; -- Points in the buffer..
   IterStart       : Gtk.Text_Iter.Gtk_Text_Iter ; --   ..used for different purposes
   EndMark         : Gtk.Text_Mark.Gtk_Text_Mark ; -- Mark at the end of buffer
   GetMark         : Gtk.Text_Mark.Gtk_Text_Mark ; -- Mark at the beginning of a GET

   -- Tags in the text buffer
   Tags            : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table ;

   EditableTag     : Gtk.Text_Tag.Gtk_Text_Tag ;
   EditableColor   : Gdk.Color.Gdk_Color;
   Editable_Font   : Pango.Font.Pango_Font_Description ;

   ErrorTag        : Gtk.Text_Tag.Gtk_Text_Tag ;
   ErrorColor      : Gdk.Color.Gdk_Color;
   Error_Font      : Pango.Font.Pango_Font_Description ;

   NotEditableTag  : Gtk.Text_Tag.Gtk_Text_Tag ;


   -- Text Buffer & View
   TextView        : Gtk.Text_View.Gtk_Text_View ;
   Scrolled        : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   Text_Font       : Pango.Font.Pango_Font_Description ;

   -- Frame
   Frame           : Gtk.Frame.Gtk_Frame ;
   Frame_Label1    : Gtk.Label.Gtk_Label ;
   Frame_Label2    : Gtk.Label.Gtk_Label ;
   Frame_Title_HBox: Gtk.Box.Gtk_Box;
   Label1_Font     : Pango.Font.Pango_Font_Description ;
   Label2_Font     : Pango.Font.Pango_Font_Description ;

   Toolbar         : Gtk.Toolbar.Gtk_Toolbar ;
   Clipboard       : Gtk.Clipboard.Gtk_Clipboard ;

   B_Close         : Gtk.Button.Gtk_Button ;
   B_Copy          : Gtk.Button.Gtk_Button ;
   B_Paste         : Gtk.Button.Gtk_Button ;
   B_Help          : Gtk.Button.Gtk_Button ;

   function Get_Application_Name(LongName : String) return String is
      I_Start : Integer := LongName'First ;
      I_End   : Integer := LongName'Last ;
      I_Search : Integer := I_End ;
   begin
      while (I_Search >= I_Start) and then (LongName(I_Search) /= '/') loop
         I_Search := I_Search - 1 ;
      end loop ;

      if (I_Search >= I_Start) then
         I_Start := I_Search + 1 ;
      end if ;

      return LongName(I_Start..I_End) ;
   end Get_Application_Name ;

   AppName         : String := Glib.Convert.Locale_To_UTF8(Get_Application_Name(Ada.Command_Line.Command_Name)) ;
   Title           : String := " is running." ;
   TitleFinished   : String := " is finished." ;

   Same_Encodings   : Boolean := Console_Encoding = Emacs_Encoding ;

   function Convert (Arg : String) return Glib.UTF8_String is
   begin
      return Glib.Convert.Convert(Arg, "UTF-8", Emacs_Encoding) ;
   end Convert ;

   function InvConvert (Arg : Glib.UTF8_String) return String is
   begin
      return Glib.Convert.Convert(Arg, Emacs_Encoding, "UTF-8") ;
   end InvConvert ;

   -- Copie la chaîne src vers tgt sans erreur
   procedure Copy_String (Src : in UTF8_String ; Tgt : out String) is
      Converted_Src : String := Glib.Convert.Convert(Src, Emacs_Encoding, "UTF-8") ;
   begin
      Ada.Strings.Fixed.Move(Source => Converted_Src,
                             Target => Tgt,
                             Drop => Ada.Strings.Right,
                             Justify => Ada.Strings.Left) ;
   end Copy_String ;

   ---------------------------------------------------------------------
   --                       HANDLERS                                  --
   ---------------------------------------------------------------------
   package Handlers is new Gtk.Handlers.Callback(Gtk.Widget.Gtk_Widget_Record);
   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      Return_Type => Boolean);

   -- Bouton "Close"
   function HF_Close(From : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean is
   begin
      Reason_For_Exit := Closed ;
      Gtk.Main.Main_Quit ;
      return False ;
   end HF_Close ;

   -- Menu "Close"
   procedure H_Close(From : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Reason_For_Exit := Closed ;
      Gtk.Window.Destroy (Main_Window) ;
      Gtk.Main.Main_Quit ;
   end H_Close ;

   -- Menu "Copy"
   procedure H_Copy(From : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Gtk.Text_Buffer.Copy_Clipboard (Buffer, Clipboard);
   end H_Copy ;

   -- Menu "Paste"
   procedure H_Paste(From : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Gtk.Text_Buffer.Paste_Clipboard (Buffer, Clipboard);
   end H_Paste ;

   -- Menu "Info"
   Info_Message : String :=
     "GADA (Graphical Ada Console)" & ASCII.LF & ASCII.LF &
     "GTK-based input and output for Ada applications" & ASCII.LF &
     "INSA Toulouse, 2008-2012" ;

   procedure H_Help(From : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Ret_Value : GtkAda.Dialogs.Message_Dialog_Buttons ;
   begin
      Ret_Value := GtkAda.Dialogs.Message_Dialog (Info_Message,
                                                  GtkAda.Dialogs.Information,
                                                  GtkAda.Dialogs.Button_OK, GtkAda.Dialogs.Button_OK,
                                                  "",
                                                  "Info") ;
   end H_Help ;

   Result : Boolean ;

   procedure Set_All_Text_Not_Editable is
   begin
      Gtk.Text_Buffer.Get_Start_Iter(Buffer, IterStart) ;
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;
      Gtk.Text_Buffer.Apply_Tag (Buffer, NotEditableTag, IterStart, IterEnd) ;
   end Set_All_Text_Not_Editable ;

   procedure Build_GUI is
   begin
      -- Will we have to quit when application finishes?
      for Argnum in 1..Ada.Command_Line.Argument_Count loop
         if Ada.Command_Line.Argument(Argnum) = "-exitgtk" then
            Exit_Gtk_When_Finished := True ;
         end if ;
      end loop ;

      -- Main Window
      --Gtk.Window.Gtk_New(Main_Window, Gtk.Enums.Window_Toplevel) ;
      --Gtk.Window.Set_Title(Main_Window, AppName & " (running in Ada Console)") ;
      --Gtk.Window.Set_Position(Main_Window, Win_Pos_Center) ;
      --Gtk.Window.Set_Default_Size(Main_Window, 800, 600) ;
      --Return_Handlers.Connect (Main_Window, "delete_event", Return_Handlers.To_Marshaller(HF_Close'Access));

      Gtk.Box.Gtk_New_Vbox(Main_Vbox);

      -- The text view
      Gtk.Text_View.Gtk_New(TextView) ;
      Gtk.Text_View.Set_Editable(TextView, False) ;
      Gtk.Text_View.Set_Cursor_Visible(TextView, False) ;
      Gtk.Text_View.Set_Left_Margin(TextView, 14) ;
      Gtk.Text_View.Set_Right_Margin(TextView, 14) ;
      Text_Font := Pango.Font.To_Font_Description(Size => 11,
                                                  Family_Name => "Monospace") ;
      Gtk.Text_View.Modify_Font(TextView, Text_Font) ;

      -- The text buffer
      Buffer := Gtk.Text_View.Get_Buffer(TextView) ;
      Gtk.Text_Buffer.Get_End_Iter (Buffer, IterEnd);
      EndMark := Gtk.Text_Buffer.Create_Mark (Buffer, "", IterEnd, False) ;
      Gtk.Text_Buffer.Insert(Buffer, IterEnd, "" & ASCII.LF) ;

      -- TAGS
      Tags := Gtk.Text_Buffer.Get_Tag_Table(Buffer) ;

      -- Tag 'editable'
      Gtk.Text_Tag.Gtk_New(EditableTag) ;
      EditableColor := Gdk.Color.Parse ("blue");
      Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, EditableColor);
      Gdk.Color.Set_Property (EditableTag, Gtk.Text_Tag.Foreground_Gdk_Property, EditableColor);

      Editable_Font := Pango.Font.To_Font_Description(Size => 12,
                                                      Style => Pango_Style_Italic,
                                                      Weight => Pango_Weight_Bold) ;
      Pango.Font.Set_Property (EditableTag, Gtk.Text_Tag.Font_Desc_Property, Editable_Font);

      Glib.Properties.Set_Property (EditableTag, Gtk.Text_Tag.Editable_Property, true);
      Gtk.Text_Tag_Table.Add(Tags, EditableTag) ;

      -- Tag 'error'
      Gtk.Text_Tag.Gtk_New(ErrorTag) ;
      ErrorColor := Gdk.Color.Parse ("red");
      Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, ErrorColor);
      Gdk.Color.Set_Property (ErrorTag, Gtk.Text_Tag.Foreground_Gdk_Property, ErrorColor);

      Error_Font := Pango.Font.To_Font_Description(Size => 12,
--                                                   Style => Pango_Style_Italic,
                                                   Weight => Pango_Weight_Bold) ;
      Pango.Font.Set_Property (ErrorTag, Gtk.Text_Tag.Font_Desc_Property, Error_Font);

      Glib.Properties.Set_Property (ErrorTag, Gtk.Text_Tag.Editable_Property, false);
      Gtk.Text_Tag_Table.Add(Tags, ErrorTag) ;

      -- Tag 'not editable'
      Gtk.Text_Tag.Gtk_New(NotEditableTag) ;
      Glib.Properties.Set_Property (NotEditableTag, Gtk.Text_Tag.Editable_Property, false);
      Gtk.Text_Tag_Table.Add(Tags, NotEditableTag) ; -- Must be added after EditableTag to get higher priority.

      -- The frame and scrolled window
      Gtk.Scrolled_Window.Gtk_New(Scrolled) ;
      Gtk.Scrolled_Window.Add (Scrolled, TextView);
      Gtk.Scrolled_Window.Set_Policy (Scrolled, Policy_Automatic,Policy_Automatic);

      Gtk.Frame.Gtk_New(Frame) ;
      Gtk.Frame.Set_Border_Width(Frame, 10) ;

      -- Frame Title : 2 labels
      Gtk.Box.Gtk_New_Hbox(Frame_Title_Hbox);

      -- Frame Label2
      Gtk.Label.Gtk_New(Frame_Label2, AppName) ;
      Gtk.Label.Set_Padding(Frame_Label2, 15, 5) ;
      Label2_Font := Pango.Font.To_Font_Description(Size => 14,
                                                    Weight => Pango_Weight_Bold) ;
      Gtk.Label.Modify_Font(Frame_Label2, Label2_Font) ;
      Gtk.Box.Pack_Start(Frame_Title_HBox, Frame_Label2) ;

      Gtk.Frame.Set_Label_Widget(Frame, Frame_Title_HBox) ;
      Gtk.Frame.Add(Frame, Scrolled);

      -- Frame Label 1
      Gtk.Label.Gtk_New(Frame_Label1, Title) ;
      Gtk.Label.Set_Padding(Frame_Label1, 5, 5) ;
      Label1_Font := Pango.Font.To_Font_Description(Size => 10,
                                                    Weight => Pango_Weight_Normal) ;
      Gtk.Label.Modify_Font(Frame_Label1, Label1_Font) ;
      Gtk.Box.Pack_Start(Frame_Title_HBox, Frame_Label1) ;

      -- The toolbar
      Gtk.Toolbar.Gtk_New (Toolbar, Orientation_Horizontal, Toolbar_Both);
      Gtk.Box.Pack_Start(Main_Vbox, Toolbar, False);
      Gtk.Toolbar.Set_Tooltips (Toolbar, True);

      Clipboard := Gtk.Clipboard.Get ;

      -- Deprecated ! --
      B_Close := Gtk.Toolbar.Insert_Stock(Toolbar, Gtk.Stock.Stock_Close, "Close the window") ;
      Handlers.Connect (B_Close, "clicked", Handlers.To_Marshaller(H_Close'Access));

      Gtk.Toolbar.Append_Space(Toolbar) ;

      B_Copy := Gtk.Toolbar.Insert_Stock(Toolbar, Gtk.Stock.Stock_Copy, "Copy selected text to clipboard") ;
      Handlers.Connect (B_Copy, "clicked", Handlers.To_Marshaller(H_Copy'Access));

      B_Paste := Gtk.Toolbar.Insert_Stock(Toolbar, Gtk.Stock.Stock_Paste, "Paste clipboard") ;
      Handlers.Connect (B_Paste, "clicked", Handlers.To_Marshaller(H_Paste'Access));
      Gtk.Button.Set_Sensitive(B_Paste, False) ;

      Gtk.Toolbar.Append_Space(Toolbar) ;

      B_Help := Gtk.Toolbar.Insert_Stock(Toolbar, Gtk.Stock.Stock_Dialog_Info, "Technical information") ;
      Handlers.Connect (B_Help, "clicked", Handlers.To_Marshaller(H_Help'Access));
      ------------------

      --Gtk.Window.Add(Main_Window, Main_Vbox);
      --Gtk.Box.Pack_Start(Main_Vbox, Frame);

      --Gtk.Window.Show_All(Main_Window);
   end Build_GUI ;


   ---------------------------------------------------------------------
   --      GTK VERSIONS of TEXT_IO PROCEDURES  (not thread-safe)      --
   ---------------------------------------------------------------------

   procedure Append(M : String) is
   begin
      -- Gtk.Window.Present(Main_Window) ;
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;

      Gtk.Text_Buffer.Insert(Buffer, IterEnd, Convert(M)) ;
      Gtk.Text_View.Scroll_To_Mark (TextView, EndMark) ;
   end Append ;

   procedure Gtk_Start_Of_Line is
   begin
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;
      Gtk.Text_Buffer.Get_Iter_At_Line(Buffer, IterStart, Gtk.Text_Buffer.Get_Line_Count(Buffer)) ;
      Gtk.Text_Buffer.Delete(Buffer, IterStart, IterEnd) ;
   end Gtk_Start_Of_Line ;

   procedure Gtk_Put(Message : String) is
   begin
      if Same_Encodings then Ada.Text_IO.Put(Message) ;
      else Ada.Text_IO.Put(Glib.Convert.Convert(Message, Console_Encoding, Emacs_Encoding)) ;
      end if ;
      Append(Message) ;
   end Gtk_Put ;

   procedure Gtk_Put_Line(Message : String) is
   begin
      if Same_Encodings then Ada.Text_IO.Put_Line(Message) ;
      else Ada.Text_IO.Put_Line(Glib.Convert.Convert(Message, Console_Encoding, Emacs_Encoding)) ;
      end if ;
      Append(Message & ASCII.LF) ;
   end Gtk_Put_Line ;

   procedure Gtk_Exception(Message : String) is
   begin
      if Same_Encodings then Ada.Text_IO.Put(Message) ;
      else Ada.Text_IO.Put(Glib.Convert.Convert(Message, Console_Encoding, Emacs_Encoding)) ;
      end if ;
      Append(Message) ;

      -- fixme : it would probably be better to use marks.
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterStart) ;
      Gtk.Text_Iter.Backward_Chars(IterStart, Message'Length, Result);
      Gtk.Text_Buffer.Apply_Tag (Buffer, ErrorTag, IterStart, IterEnd);
   end Gtk_Exception ;

   New_Line_String : String := (1 => ASCII.LF) ;

   procedure Gtk_New_Line is
   begin
      Ada.Text_IO.New_Line ;
      Append(New_Line_String) ;
   end Gtk_New_Line ;

   --         --
   --   GET   --
   --         --
   Nb_Lines        : Glib.Gint ; -- Number of lines when GET started
   H_Insert_ID : Gtk.Handlers.Handler_Id ;

   procedure Finish_Get is
   begin
      Gtk.Handlers.Disconnect(Buffer, H_Insert_Id) ;
      Gtk.Text_View.Set_Editable(TextView, False) ;
      Gtk.Button.Set_Sensitive(B_Paste, False) ;
      Gtk.Text_View.Set_Cursor_Visible(TextView, False) ;
      Set_All_Text_Not_Editable ;

      Gtk.Text_Buffer.Get_Iter_At_Mark(Buffer, IterStart, GetMark) ;
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;

      Reason_For_Exit := Return_Get ;
      Gtk.Main.Main_Quit ;
   end Finish_Get ;

   procedure H_Insert(From : access Gtk.Widget.Gtk_Widget_Record'Class ; Params : Glib.Values.GValues) is
      IterOrig  : Gtk.Text_Iter.Gtk_Text_Iter ;
      IterEnd   : Gtk.Text_Iter.Gtk_Text_Iter ;
      IterStart : Gtk.Text_Iter.Gtk_Text_Iter ;
      Text      : Glib.UTF8_String := Glib.Values.Get_String(Glib.Values.Nth(Params, 2)) ;
      -- Length : Glib.Gint := Gint(Glib.Unicode.UTF8_Strlen(Text)) ;
      Lines     : Glib.Gint ;
   begin
      Gtk.Text_Iter.Get_Text_Iter(Glib.Values.Nth(Params, 1), IterOrig) ;
      Gtk.Text_Iter.Copy(IterOrig, IterEnd) ;

      --
      -- The following two lines are not satisfying because H_insert is sometimes called with text that was
      -- inserted (with a Gtk_Put) before the handler was connected. (Probably because of bufferisation)
      --
      -- Gtk.Text_Iter.Copy(IterOrig, IterStart) ;
      -- Gtk.Text_Iter.Backward_Chars(IterStart, Length, Result) ;
      --
      -- So, we use GetMark instead.
      Gtk.Text_Buffer.Get_Iter_At_Mark(Buffer, IterStart, GetMark) ;

      -- Apply Tag
      Ada.Text_IO.Put(Glib.Convert.Convert(Text, Console_Encoding, "UTF-8")) ;
      Gtk.Text_Buffer.Remove_Tag (Buffer, NotEditableTag, IterStart, IterEnd) ;
      Gtk.Text_Buffer.Apply_Tag (Buffer, EditableTag, IterStart, IterEnd) ;

      -- Detect New Lines
      Lines := Gtk.Text_Buffer.Get_Line_Count(Buffer) ;
      if Lines > Nb_Lines then Finish_Get ; end if ;
   end H_Insert ;

   Accept_Requests : Boolean := False ; -- GAda does not accept requests while GETting and while initializing

   procedure Gtk_Get is
   begin
      Accept_Requests := False ;

      -- The text that is already in the buffer is set not editable
      Set_All_Text_Not_Editable ;

      -- Count the lines
      Nb_Lines := Gtk.Text_Buffer.Get_Line_Count(Buffer) ;

      -- Get a mark at the beginning of GET
      Gtk.Text_Buffer.Get_End_Iter(Buffer, IterEnd) ;
      GetMark := Gtk.Text_Buffer.Create_Mark (Buffer, "", IterEnd, True) ;

      -- Set the rest editable
      H_Insert_Id := Handlers.Object_Connect (Buffer, "insert_text", H_Insert'Access, TextView, true) ;
      Gtk.Text_View.Set_Editable(TextView, True) ;
      Gtk.Text_Buffer.Place_Cursor(Buffer, IterEnd) ;

      Gtk.Text_View.Set_Cursor_Visible(TextView, True) ;
      Gtk.Text_View.Grab_Focus(TextView) ;
      Gtk.Button.Set_Sensitive(B_Paste, True) ;

      Ada.Text_IO.Put("[[-- Input expected in gtk window only --]]") ;
   end Gtk_Get ;

   ---------------------------------------------------------------------
   --         GTK TASK and standard TEXT_IO INTERFACE                 --
   ---------------------------------------------------------------------

   function Check_Program return Boolean is
   begin
   Gdk.Threads.Enter;

        -- Check if main task has finished.
   if (not Main_Task_Finished) and (not Ada.Task_Identification.Is_Callable(Main_Task_Id)) then
   	Main_Task_Finished := True ;

           -- Check for an uncaught exception
   		if Uncaught_Exception.Is_Present then
   	           Gtk_Exception("Exception " & Uncaught_Exception.Get_Uncaught_Exception_Name & ":" & ASCII.LF) ;
   	           Gtk_Exception(Uncaught_Exception.Get_Uncaught_Exception_Message & ASCII.LF) ;
   	        end if ;

   	        Gtk_Exception(ASCII.LF & AppName & " is finished." & ASCII.LF & ASCII.LF) ;

   	        Gtk.Label.Set_Text(Frame_Label1, TitleFinished) ;
  	        Gtk.Window.Set_Title(Main_Window, AppName & " is finished") ;

   	        if Exit_Gtk_When_Finished then
   	           Gtk.Main.Main_Quit ;
   	        end if ;
   	     end if ;

   	     Gdk.Threads.Leave;
   	     return (not Main_Task_Finished) ;
   	  end Check_Program ;

   	 --Kill main program
   	  procedure Kill_Program is
   	     Res : Boolean ;
   	  begin
   	     -- Temporisation pour permettre au programme principal de terminer proprement s'il le souhaite.
   	     delay 0.75 ;

   	     Res := Check_Program ; -- <-- AJOUTÉ RECEMMENT. FIXME TODO : en cas de bug, chercher là en priorité.

   	     if not Main_Task_Finished then

   	        -- SIGKILL semble la seule manière efficace
  	        Ada.Text_IO.Put_Line("[[-- Envoi du signal SIGKILL a l'application. --]]") ;
   	        Ada.Interrupts.Signal.Generate_Interrupt(Ada.Interrupts.Names.SIGKILL) ;

   	        -- Fonctionne de manière aléatoire
   	        --Ada.Interrupts.Signal.Generate_Interrupt(Ada.Interrupts.Names.SIGQUIT) ;
   	        --Ada.Task_Identification.Abort_Task(Main_Task_Id) ;
   	     end if ;
   	  end;


   	  task Gtk_Main_Loop is
   	     entry Return_From_Get ;
   	  end Gtk_Main_Loop ;

   Finished : Boolean := False ; -- indicates if we should exit from the main loop

   	  task body Gtk_Main_Loop is
   	     Tm_Handler : Gtk.Main.Timeout_Handler_Id ;
   	     -- B_Ignored : Boolean ;
   	  begin
   	     Gdk.Threads.G_Init;
   	     Gdk.Threads.Init;
   	     Gtk.Main.Set_Locale;
   	     Gtk.Main.Init;

   	     Build_GUI ;
   	     Tm_Handler := Gtk.Main.Timeout_Add (Program_Check_Interval, Check_Program'access) ;

   	     Finished := False ;
   	     while not Finished loop
   	        Gdk.Threads.Enter;
   	        Accept_Requests := True ;

   	        Reason_For_Exit := None ;
   	        Gtk.Main.Main;
   	        -- Do what remains to be done (necessary?)
   	        -- while Gtk.Main.Events_Pending loop B_ignored := Gtk.Main.Main_Iteration(False); end loop;

   	        Accept_Requests := False ;
   	        Gdk.Threads.Leave;

   	        -- Why have we exited from the loop?
   	        case Reason_For_Exit is

   	           -- The window has been closed
   	           when Closed => Finished := True ;

   	           -- Get is finished
   	           when Return_Get =>
   	              Finished := False ;

   	              accept Return_From_Get do
   	                 null ;
   	              end Return_From_Get ;

   	           -- No reason, we exit
   	           when None => Finished := True ;

   	        end case ;

   	     end loop ;

   	     Kill_Program ;

   	     Ada.Text_IO.New_Line ;
   	     Ada.Text_IO.Put_Line("[[-- Exiting gtk loop --]]") ;

   	  exception
  	     when Any : others =>
   	        Gtk_Exception("An exception has occured in Gtk-Thread: " & Ada.Exceptions.Exception_Name(Any)
   	                      & ASCII.LF & Ada.Exceptions.Exception_Message(Any)) ;

   	  end Gtk_Main_Loop ;

   procedure Enter is
   begin
      while not (Finished or Accept_Requests) loop
         delay 0.5 ;
      end loop ;

      if Finished then
         raise Fenetre_Fermee ;
      else
         Gdk.Threads.Enter;
      end if ;
   end ;

   procedure Leave is
   begin
      Gdk.Threads.Leave;
   end Leave ;

   procedure Put(Item : String) is
   begin
      Enter ;
      Gtk_Put(Item) ;
      Leave ;
   end Put ;

   procedure Put_Err(Item : String) is
   begin
      Enter ;
      Gtk_Exception(Item) ;
      Gtk_New_Line ;
      Leave ;
   end Put_Err ;

   procedure IPut(Item : Integer) is
   begin
      Enter;
      Gtk_Put(Ada.Strings.Fixed.Trim(Integer'Image(Item), Ada.Strings.Left)) ;
      Leave;
   end IPut ;

   procedure Put (Car : in Character) is
   begin
      Enter ;
      Gtk_Put((1 => Car)) ;
      Leave ;
   end Put ;

   procedure FPut(Item : Float) is
   begin
      Enter;
      Gtk_Put(Ada.Strings.Fixed.Trim(Float'Image(Item), Ada.Strings.Left)) ;
      Leave;
   end FPut ;

   procedure Put_Line(Item : String) is
   begin
      Enter;
      Gtk_Put_Line(Item) ;
      Leave;
   end Put_Line ;

   procedure Put_Line(Car : in Character) is
   begin
      Enter;
      Gtk_Put_Line((1 => Car)) ;
      Leave;
   end Put_Line ;

   procedure Replace (Item : in String) is
   begin
      Enter ;
      Gtk_Start_Of_Line ;
      Gtk_Put(Item) ;
      Leave ;
   end Replace ;

   procedure New_Line is
   begin
      Enter;
      Gtk_New_Line ;
      Leave;
   end New_Line ;

   function Core_FGet return String is
   begin
      -- If we are in "non-blocking" mode, forbid GETs.
      if Exit_Gtk_When_Finished then
         raise GetForbidden ;
      end if ;

      Enter ;
      Gtk_Get ;
      Leave ;
      Gtk_Main_Loop.Return_From_Get ;
      Enter ;
      declare
         Result : String := Gtk.Text_Buffer.Get_Text(Buffer, IterStart, IterEnd) ;
      begin
         Leave ;
         if Result(Result'Last) = ASCII.LF then return Result(Result'First .. Result'Last - 1) ;
         end if ;
         return Result ;
      end ;
   end Core_FGet ;

   function FGet return String is
   begin
      return Core_FGet ;
   exception when GetForbidden => return "" ;
   end FGet ;

   procedure Get(Item : out String) is
   begin
      Copy_String(FGet, Item) ;
   end Get ;

   function FIGet return Integer is
      Result, Last : Integer ;
   begin
      declare
         Tape : constant String := Core_FGet ;
      begin
         if Tape'Length = 0 then return FIGet ;
         else
            Ada.Integer_Text_IO.Get(Tape, Result, Last) ;
            return Result ;
         end if ;
      exception when others =>
         Put_Err ("Erreur, ce n'est pas un entier : " & Tape) ;

         -- Pour brouiller les pistes et virer le warning.
         if 1 = 1 then return FIGet ;
         else return FIGet ;
         end if ;
      end ;

   exception when GetForbidden => return 0 ;
   end FIGet ;

   function FFGet return Float is
      Result : Float ;
      Last : Integer ;
   begin

      declare
         Tape : constant String := Core_FGet ;
      begin
         if Tape'Length = 0 then return FFGet ;
         else
            Ada.Float_Text_IO.Get(Tape, Result, Last) ;
            return Result ;
         end if ;
      exception when others =>
         Put_Err ("Erreur, ce n'est pas un réel : " & Tape) ;

         -- Pour brouiller les pistes et virer le warning.
         if 1 = 1 then return FFGet ;
         else return FFGet ;
         end if ;
      end ;

   exception when GetForbidden => return 0.0 ;
   end FFGet ;

   procedure Abort_Gtk_Loop is
   begin
      Ada.Text_IO.Put_Line("MODIFIED BY REM0503");
      Ada.Text_IO.Put_Line("[[-- Gada-core: Abort signal --]]") ;
      Gtk.Main.Main_Quit ;
      abort Gtk_Main_Loop ;
   end Abort_Gtk_Loop ;

end GAda.Core ;
