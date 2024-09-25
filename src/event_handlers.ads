with Gtk.Widget;
with Gdk.Event;
with Gtk.Handlers;

package Event_Handlers is
   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);

   function On_Button_Press
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
end Event_Handlers;