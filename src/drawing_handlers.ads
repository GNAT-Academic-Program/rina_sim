with Gtk.Drawing_Area;
with Gtk.Handlers;

package Drawing_Handlers is
   package Drawing_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record, Boolean);
end Drawing_Handlers;