with Gtk.Button;
with Gtk.Drawing_Area;
with Gdk.Event;
with Gtk.Widget;
with Gtkada.Builder;

package Handlers is
   procedure Window_Destroy
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Spawn_Machine_Handler
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Despawn_Machine_Handler
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Create_Link_Handler
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Set_Network_Topology
     (Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area);

   procedure Set_Builder (B : Gtkada.Builder.Gtkada_Builder);

   function Network_Topology_Button_Press_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;

end Handlers;