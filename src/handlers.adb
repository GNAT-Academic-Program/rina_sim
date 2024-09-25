with Network;
with Gdk.Event;
with Gtk.Widget;
with Gtk.Drawing_Area;
with Gtk.Button;
with Glib; use Glib;
with Ada.Text_IO;
with Ada.Exceptions;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Main;

package body Handlers is
   Network_Topology : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Builder : Gtkada.Builder.Gtkada_Builder;

   procedure Window_Destroy
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end Window_Destroy;

   procedure Set_Network_Topology
     (Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area) is
   begin
      Network_Topology := Drawing_Area;
   end Set_Network_Topology;

   procedure Set_Builder (B : Gtkada.Builder.Gtkada_Builder) is
   begin
      Builder := B;
   end Set_Builder;

   procedure Spawn_Machine_Handler
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Ada.Text_IO.Put_Line ("Spawn_Machine_Handler called");
      Network.Add_Machine;
      Network_Topology.Queue_Draw;
      Ada.Text_IO.Put_Line ("Spawn_Machine_Handler finished");
   end Spawn_Machine_Handler;

   procedure Despawn_Machine_Handler
    (Button : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Button);
   begin 
      Ada.Text_IO.Put_Line ("Despawn_Machine_Handler called");
      declare
         Removed_Count : Natural;
      begin
         Network.Remove_Selected_Machines (Removed_Count);
         if Removed_Count > 0 then
            Ada.Text_IO.Put_Line ("Removed " & Removed_Count'Image & " machine(s)");
            Network_Topology.Queue_Draw;
         else
            Ada.Text_IO.Put_Line ("No machines selected for removal");
         end if;
      end;
      Ada.Text_IO.Put_Line ("Despawn_Machine_Handler finished");
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Despawn_Machine_Handler: " &
                               Ada.Exceptions.Exception_Information (E));
   end Despawn_Machine_Handler;

   procedure Create_Link_Handler
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Ada.Text_IO.Put_Line ("Create_Link_Handler called");
      Network.Create_Link_Between_Selected;
      Network_Topology.Queue_Draw;
      Ada.Text_IO.Put_Line ("Create_Link_Handler finished");
   end Create_Link_Handler;

   function Network_Topology_Button_Press_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Widget);
      use type Gdk.Event.Gdk_Event_Type;
      Button : Guint;
      X, Y : Gdouble;
   begin
      Ada.Text_IO.Put_Line ("Network_Topology_Button_Press_Handler called");
      Ada.Text_IO.Put_Line ("Event type: " & Gdk.Event.Get_Event_Type (Event)'Image);

      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press then
         Button := Gdk.Event.Get_Button (Event);
         Gdk.Event.Get_Coords (Event, X, Y);
         Ada.Text_IO.Put_Line ("Button pressed: " & Button'Image);
         Ada.Text_IO.Put_Line ("Coordinates: X=" & X'Image & ", Y=" & Y'Image);

         if Button = 1 then -- Left mouse button
            Ada.Text_IO.Put_Line ("Left mouse button pressed at X:" & X'Image & ", Y:" & Y'Image);
            Network.Select_Machine (Float (X), Float (Y));
            Network_Topology.Queue_Draw;
            return True;
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Network_Topology_Button_Press_Handler: " &
                               Ada.Exceptions.Exception_Information (E));
         return False;
   end Network_Topology_Button_Press_Handler;

end Handlers;