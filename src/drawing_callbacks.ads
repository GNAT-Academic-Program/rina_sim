with Gtk.Drawing_Area;
with Cairo;

package Drawing_Callbacks is
   function Draw_Callback
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean;
end Drawing_Callbacks;