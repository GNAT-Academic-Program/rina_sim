with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gdk.Event;
with Gdk.RGBA;
with Glib; use Glib;
with Glib.Object;
with Gtk.Button;
with Gtk.Drawing_Area;
with Gtk.Grid;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;
with Cairo;
with Machine_Model; use Machine_Model;
with Gtk.Handlers;

package body Machine_Simulator is

   package Button_Callback is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   package Drawing_Area_Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record, Boolean);

   package Window_Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record, Boolean);

   Window        : Gtk.Window.Gtk_Window;
   Grid          : Gtk.Grid.Gtk_Grid;
   Drawing_Area  : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Create_Button : Gtk.Button.Gtk_Button;

   Manager : Machine_Manager;

   function Random_Color return Gdk.RGBA.Gdk_RGBA;

   procedure Create_Machine;

   function Draw_Callback
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean;
      
   function Button_Press_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button) return Boolean;

   procedure Create_Machine_Cb
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Random_Color return Gdk.RGBA.Gdk_RGBA is
      package Random_Color is new Ada.Numerics.Discrete_Random (Guint16);
      Gen : Random_Color.Generator;
      function To_GDouble (Value : Guint16) return Gdouble is
      begin
         return Gdouble (Value) / Gdouble (Guint16'Last);
      end To_GDouble;
   begin
      Random_Color.Reset (Gen);
      return (Red   => To_GDouble (Random_Color.Random (Gen)),
              Green => To_GDouble (Random_Color.Random (Gen)),
              Blue  => To_GDouble (Random_Color.Random (Gen)),
              Alpha => 1.0);
   end Random_Color;

   procedure Create_Machine is
      X : constant Integer :=
        (Integer (Manager.Get_Machines.Length) mod 7) * 120 + 10;
      Y : constant Integer :=
        (Integer (Manager.Get_Machines.Length) / 7) * 120 + 10;
   begin
      Manager.Create_Machine (X, Y);
      Gtk.Widget.Queue_Draw (Gtk.Widget.Gtk_Widget (Drawing_Area));
   end Create_Machine;

   function Draw_Callback
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean
   is
      Machines : constant Machine_Vectors.Vector := Manager.Get_Machines;
   begin
      for Machine of Machines loop
         declare
            Color : constant Gdk.RGBA.Gdk_RGBA := Random_Color;
         begin
            Cairo.Set_Source_Rgba (Cr, Color.Red, Color.Green,
                                   Color.Blue, Color.Alpha);
            Cairo.Rectangle (Cr, Gdouble (Machine.X), Gdouble (Machine.Y),
                             100.0, 100.0);
            Cairo.Fill (Cr);

            Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Cairo.Select_Font_Face (Cr, "Sans",
                                    Cairo.Cairo_Font_Slant_Normal,
                                    Cairo.Cairo_Font_Weight_Bold);
            Cairo.Set_Font_Size (Cr, 12.0);
            Cairo.Move_To (Cr, Gdouble (Machine.X + 5),
                           Gdouble (Machine.Y + 20));
            Cairo.Show_Text (Cr, To_String (Machine.Name));

            -- Draw the "X" button
            Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
            Cairo.Rectangle (Cr, Gdouble (Machine.X + 80),
                             Gdouble (Machine.Y), 20.0, 20.0);
            Cairo.Fill (Cr);
            Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            Cairo.Select_Font_Face (Cr, "Sans",
                                    Cairo.Cairo_Font_Slant_Normal,
                                    Cairo.Cairo_Font_Weight_Bold);
            Cairo.Set_Font_Size (Cr, 16.0);
            Cairo.Move_To (Cr, Gdouble (Machine.X + 85),
                           Gdouble (Machine.Y + 15));
            Cairo.Show_Text (Cr, "X");
         end;
      end loop;
      return True;
   end Draw_Callback;

   function Button_Press_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      use type Gdk.Event.Gdk_Event_Type;
   begin
      if Event.The_Type = Gdk.Event.Button_Press then
         if Event.Button = 1 then
            declare
               X : constant Gdouble := Event.X;
               Y : constant Gdouble := Event.Y;
               Machines : constant Machine_Vectors.Vector :=
                 Manager.Get_Machines;
            begin
               for Machine of Machines loop
                  if X >= Gdouble (Machine.X + 80)
                    and then X <= Gdouble (Machine.X + 100)
                    and then Y >= Gdouble (Machine.Y)
                    and then Y <= Gdouble (Machine.Y + 20)
                  then
                     -- "X" button clicked, remove the machine
                     Manager.Destroy_Machine (Machine.ID);
                     Gtk.Widget.Queue_Draw
                       (Gtk.Widget.Gtk_Widget (Drawing_Area));
                     return True;
                  end if;
               end loop;
            end;
         end if;
      end if;
      return False;
   end Button_Press_Event;

   procedure Create_Machine_Cb
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Create_Machine;
   end Create_Machine_Cb;

   procedure Run is
   begin
      Gtk.Main.Init;
      Gtk.Window.Gtk_New (Window);
      Window.Set_Default_Size (800, 600);
      Window.Set_Title ("Machine Simulator");

      Gtk.Grid.Gtk_New (Grid);
      Window.Add (Grid);

      Gtk.Drawing_Area.Gtk_New (Drawing_Area);
      Drawing_Area.Set_Size_Request (800, 550);
      Grid.Attach (Drawing_Area, 0, 0, 1, 1);

      Gtk.Button.Gtk_New (Create_Button, "Create Machine");
      Grid.Attach (Create_Button, 0, 1, 1, 1);

      Button_Callback.Connect
        (Create_Button, "clicked",
         Button_Callback.To_Marshaller (Create_Machine_Cb'Access));

      Drawing_Area_Return_Callback.Connect
        (Drawing_Area, "draw",
         Drawing_Area_Return_Callback.To_Marshaller (Draw_Callback'Access));

      Window.On_Button_Press_Event (Button_Press_Event'Access, Window);

      Window.Show_All;
      Gtk.Main.Main;
   end Run;

end Machine_Simulator;