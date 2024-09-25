with Handlers;

package body Event_Handlers is
   function On_Button_Press
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
   begin
      return Handlers.Network_Topology_Button_Press_Handler (Widget, Event);
   end On_Button_Press;
end Event_Handlers;