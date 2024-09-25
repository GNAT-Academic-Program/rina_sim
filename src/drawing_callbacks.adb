with Network;
with Ada.Text_IO;
with Glib; use Glib;
with Cairo;

package body Drawing_Callbacks is
   function Draw_Callback
     (Widget : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean
   is
      Width, Height : Gint;
   begin
      Width := Widget.Get_Allocated_Width;
      Height := Widget.Get_Allocated_Height;

      Ada.Text_IO.Put_Line
        ("Drawing area size:" & Width'Image & " x" & Height'Image);

      -- Update the network drawing area size
      Network.Update_Drawing_Area_Size (Float (Width), Float (Height));

      Network.Draw_Network (Widget, Cr);
      return True;
   end Draw_Callback;
end Drawing_Callbacks;