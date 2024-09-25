with Gtk.Main;
with Gtk.Window;
with Gtkada.Builder;
with Ada.Text_IO;
with Glib.Error;
with Glib; use Glib;
with Gtk.Drawing_Area;
with Gtk.Button;
with Network;
with Drawing_Callbacks;
with Drawing_Handlers;
with Handlers;
with Gdk.Event;
with Event_Handlers;
with Gtk.GEntry;

procedure Rina_Simulator is
   Builder : Gtkada.Builder.Gtkada_Builder;
   Window  : Gtk.Window.Gtk_Window;
   Error   : aliased Glib.Error.GError;

   Network_Topology : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Spawn_Button     : Gtk.Button.Gtk_Button;
   Despawn_Button   : Gtk.Button.Gtk_Button;
   Create_Link_Button : Gtk.Button.Gtk_Button;
   Delete_Link_Button : Gtk.Button.Gtk_Button;
   Machine_Entry    : Gtk.GEntry.Gtk_Entry;

begin
   Gtk.Main.Init;
   Gtkada.Builder.Gtk_New (Builder);

   if Gtkada.Builder.Add_From_File
       (Builder, "src/rina_sim.glade", Error => Error'Access) = 0
   then
      Ada.Text_IO.Put_Line
        ("Error loading Glade file: " & Glib.Error.Get_Message (Error));
      return;
   end if;

   Window :=
     Gtk.Window.Gtk_Window
       (Gtkada.Builder.Get_Object (Builder, "main_window"));

   Window.On_Destroy (Handlers.Window_Destroy'Access);

   Network_Topology :=
     Gtk.Drawing_Area.Gtk_Drawing_Area
       (Builder.Get_Object ("network_topology"));

   Network_Topology.Set_Hexpand (True);
   Network_Topology.Set_Vexpand (True);
   Network_Topology.Set_Can_Focus (True);
   Network_Topology.Add_Events (Gdk.Event.Button_Press_Mask);

   Handlers.Set_Network_Topology (Network_Topology);
   Network.Set_Drawing_Area (Network_Topology);

   Drawing_Handlers.Drawing_Cb.Connect
     (Network_Topology, "draw",
      Drawing_Handlers.Drawing_Cb.To_Marshaller
        (Drawing_Callbacks.Draw_Callback'Access));

   Event_Handlers.Event_Cb.Connect
     (Network_Topology, "button-press-event",
      Event_Handlers.Event_Cb.To_Marshaller
        (Event_Handlers.On_Button_Press'Access));

   Spawn_Button := Gtk.Button.Gtk_Button (Builder.Get_Object ("spawn_button"));
   Spawn_Button.On_Clicked (Handlers.Spawn_Machine_Handler'Access);

   Despawn_Button := Gtk.Button.Gtk_Button
     (Builder.Get_Object ("despawn_button"));
   Despawn_Button.On_Clicked (Handlers.Despawn_Machine_Handler'Access);

   Create_Link_Button := Gtk.Button.Gtk_Button
     (Builder.Get_Object ("create_link_button"));
   Create_Link_Button.On_Clicked (Handlers.Create_Link_Handler'Access);

   Delete_Link_Button := Gtk.Button.Gtk_Button
     (Builder.Get_Object ("delete_link_button"));
   -- Uncomment the following line when Delete_Link_Handler is implemented
   -- Delete_Link_Button.On_Clicked (Handlers.Delete_Link_Handler'Access);

   Machine_Entry := Gtk.GEntry.Gtk_Entry
     (Builder.Get_Object ("machine_entry"));

   Handlers.Set_Builder (Builder);

   Window.Show_All;

   Gtk.Main.Main;
end Rina_Simulator;
