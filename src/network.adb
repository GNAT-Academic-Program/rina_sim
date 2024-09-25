with Ada.Numerics;

with Machine; use Machine;
with Glib; use Glib;
with Ada.Text_IO;
with Gtk.Widget;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers; use Ada.Containers;
with Cairo;
with Ada.Exceptions;

package body Network is
   Gen : Ada.Numerics.Float_Random.Generator;
   Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Next_Machine_Id : Positive := 1;

   procedure Redistribute_Machines;
   procedure Refresh_Drawing_Area;
   procedure Initialize_Selected_Machines;

   function Generate_Unique_Machine_Name return String is
      Name : String := "Machine " & Positive'Image (Next_Machine_Id);
   begin
      while Machines.Contains (Name) loop
         Next_Machine_Id := Next_Machine_Id + 1;
         Name := "Machine " & Positive'Image (Next_Machine_Id);
      end loop;
      return Name;
   end Generate_Unique_Machine_Name;

   procedure Add_Machine is
      New_Machine : Machine.Machine;
      Name : constant String := Generate_Unique_Machine_Name;
   begin
      New_Machine.Id := Next_Machine_Id;
      New_Machine.Name := To_Unbounded_String (Name);
      Machines.Insert (Name, New_Machine);
      Ada.Text_IO.Put_Line ("Added new machine: " & Name);
      Next_Machine_Id := Next_Machine_Id + 1;
      Redistribute_Machines;
      Refresh_Drawing_Area;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Add_Machine: " & Ada.Exceptions.Exception_Message (E));
   end Add_Machine;

   procedure Remove_Machine (Name : String) is
   begin
      if Machines.Contains (Name) then
         Machines.Delete (Name);
         Ada.Text_IO.Put_Line ("Removed machine: " & Name);
         
         -- Clear selection if the removed machine was selected
         for I in Selected_Machines'Range loop
            if Selected_Machines (I).Name = To_Unbounded_String (Name) then
               Selected_Machines (I) := (X => 0.0, Y => 0.0, Id => Positive'First, Name => To_Unbounded_String (""));
               if Selected_Count > 0 then
                  Selected_Count := Selected_Count - 1;
               end if;
            end if;
         end loop;

         Redistribute_Machines;
         Refresh_Drawing_Area;
      else
         Ada.Text_IO.Put_Line ("Machine not found: " & Name);
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Remove_Machine: " & Ada.Exceptions.Exception_Message (E));
   end Remove_Machine;

   procedure Remove_Selected_Machines (Removed_Count : out Natural) is
   begin
      Removed_Count := 0;
      for I in 1 .. Selected_Count loop
         declare
            Name : constant String := To_String (Selected_Machines (I).Name);
         begin
            if Machines.Contains (Name) then
               Machines.Delete (Name);
               Removed_Count := Removed_Count + 1;
               Ada.Text_IO.Put_Line ("Removed machine: " & Name);
            end if;
         end;
      end loop;

      -- Clear all selections
      Selected_Count := 0;
      Initialize_Selected_Machines;

      if Removed_Count > 0 then
         Redistribute_Machines;
         Refresh_Drawing_Area;
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Remove_Selected_Machines: " & Ada.Exceptions.Exception_Message (E));
   end Remove_Selected_Machines;

   procedure Draw_Network
     (Area : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr : Cairo.Cairo_Context) is
      pragma Unreferenced (Area);
   begin
      -- Clear the drawing area
      Cairo.Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);  -- White background
      Cairo.Paint (Cr);

      -- Draw links
      for L in Links.Iterate loop
         declare
            Link : constant Machine.Link := Links (Link_Maps.Key (L));
            From, To : Machine.Machine;
            From_Key, To_Key : Unbounded_String;
         begin
            Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);  -- Black line
            -- Find the machines for this link
            for M in Machines.Iterate loop
               if Machines (M).Name = Link.From then
                  Cairo.Move_To (Cr, Gdouble (Machines (M).X), Gdouble (Machines (M).Y));
               elsif Machines (M).Name = Link.To then
                  Cairo.Line_To (Cr, Gdouble (Machines (M).X), Gdouble (Machines (M).Y));
                  Cairo.Stroke (Cr);
               end if;
            end loop;
         end;
      end loop;

      -- Draw machines
      for C in Machines.Iterate loop
         declare
            M : constant Machine.Machine := Machines (C);
         begin
            -- Set color for machine (blue for normal, red for selected)
            if (Selected_Count > 0 and then Selected_Machines (1) = M) or
               (Selected_Count > 1 and then Selected_Machines (2) = M) then
               Cairo.Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);  -- Red for selected
            else
               Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);  -- Blue for normal
            end if;
            
            -- Draw circle for machine
            Cairo.Arc (Cr, Gdouble (M.X), Gdouble (M.Y), 20.0, 0.0, 2.0 * Ada.Numerics.Pi);
            Cairo.Fill (Cr);

            -- Draw machine name
            Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);  -- Black text
            Cairo.Select_Font_Face (Cr, "Sans", Cairo.Cairo_Font_Slant_Normal, Cairo.Cairo_Font_Weight_Normal);
            Cairo.Set_Font_Size (Cr, 12.0);
            Cairo.Move_To (Cr, Gdouble (M.X - 10.0), Gdouble (M.Y + 30.0));
            Cairo.Show_Text (Cr, To_String (M.Name));
         end;
      end loop;
   end Draw_Network;

   procedure Update_Drawing_Area_Size (Width, Height : Float) is
   begin
      Drawing_Area_Width := Width;
      Drawing_Area_Height := Height;
      Redistribute_Machines;
   end Update_Drawing_Area_Size;

   procedure Select_Machine (X, Y : Float) is
      Closest_Machine : Machine.Machine;
      Closest_Distance : Float := Float'Last;
   begin
      for C in Machines.Iterate loop
         declare
            M : constant Machine.Machine := Machines (C);
            Distance : Float;
            use Ada.Numerics.Elementary_Functions;
         begin
            Distance := Sqrt ((M.X - X)**2 + (M.Y - Y)**2);
            if Distance < Closest_Distance then
               Closest_Distance := Distance;
               Closest_Machine := M;
            end if;
         end;
      end loop;

      if Closest_Distance <= 20.0 then  -- Within the circle radius
         if Selected_Count = 0 then
            Selected_Machines (1) := Closest_Machine;
            Selected_Count := 1;
         elsif Selected_Count = 1 and then Selected_Machines (1) /= Closest_Machine then
            Selected_Machines (2) := Closest_Machine;
            Selected_Count := 2;
         elsif Selected_Count = 2 then
            if Selected_Machines (1) = Closest_Machine then
               Selected_Machines (1) := Selected_Machines (2);
               Selected_Count := 1;
            elsif Selected_Machines (2) = Closest_Machine then
               Selected_Count := 1;
            else
               Selected_Machines (1) := Selected_Machines (2);
               Selected_Machines (2) := Closest_Machine;
            end if;
         end if;
         Refresh_Drawing_Area;
      end if;
   end Select_Machine;

   procedure Create_Link_Between_Selected is
   begin
      if Selected_Count = 2 then
         declare
            New_Link : Machine.Link;
            Link_Key_Forward : constant String := To_String(Selected_Machines (1).Name) & To_String(Selected_Machines (2).Name);
            Link_Key_Backward : constant String := To_String(Selected_Machines (2).Name) & To_String(Selected_Machines (1).Name);
         begin
            New_Link.From := Selected_Machines (1).Name;
            New_Link.To := Selected_Machines (2).Name;

            if Links.Contains (Link_Key_Forward) or Links.Contains (Link_Key_Backward) then
               Ada.Text_IO.Put_Line ("Link already exists between " & To_String (Selected_Machines (1).Name) & " and " & To_String (Selected_Machines (2).Name));
            else
               Links.Insert (Link_Key_Forward, New_Link);
               Ada.Text_IO.Put_Line ("Created link between " & To_String (Selected_Machines (1).Name) & " and " & To_String (Selected_Machines (2).Name));
            end if;

            Selected_Count := 0;  -- Clear selection after creating link
            Refresh_Drawing_Area;
         end;
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Create_Link_Between_Selected: " & Ada.Exceptions.Exception_Message (E));
   end Create_Link_Between_Selected;

   procedure Create_Link is
   begin
      Create_Link_Between_Selected;
   end Create_Link;

   function Get_Selected_Count return Natural is
   begin
      return Selected_Count;
   end Get_Selected_Count;

   procedure Set_Drawing_Area (Area : Gtk.Drawing_Area.Gtk_Drawing_Area) is
   begin
      Drawing_Area := Area;
   end Set_Drawing_Area;

   procedure Redistribute_Machines is
      use Ada.Numerics.Elementary_Functions;
      Num_Machines : constant Natural := Natural (Machines.Length);
   begin
      if Num_Machines = 0 then
         return;
      end if;

      declare
         Rows : constant Positive :=
           Positive (Float'Ceiling (Sqrt (Float (Num_Machines))));
         Cols : constant Positive :=
           Positive (Float'Ceiling (Float (Num_Machines) / Float (Rows)));
         Row_Height : constant Float := Drawing_Area_Height / Float (Rows);
         Col_Width : constant Float := Drawing_Area_Width / Float (Cols);
         I : Natural := 0;
      begin
         for C in Machines.Iterate loop
            declare
               Row : constant Natural := I / Cols;
               Col : constant Natural := I mod Cols;
               X : constant Float := Col_Width * (Float (Col) + 0.5);
               Y : constant Float := Row_Height * (Float (Row) + 0.5);
               Key : constant String := Machine_Maps.Key (C);
               M : Machine.Machine := Machines (C);
            begin
               M.X := X;
               M.Y := Y;
               Machine_Maps.Replace (Machines, Key, M);
               Ada.Text_IO.Put_Line ("Redistributed machine: " & Key & " to (" & X'Image & "," & Y'Image & ")");
            end;
            I := I + 1;
         end loop;
      end;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error in Redistribute_Machines: " & Ada.Exceptions.Exception_Message (E));
   end Redistribute_Machines;

   procedure Refresh_Drawing_Area is
   begin
      if Drawing_Area /= null then
         Gtk.Widget.Queue_Draw (Gtk.Widget.Gtk_Widget (Drawing_Area));
      else
         Ada.Text_IO.Put_Line ("Error: Drawing_Area is null");
      end if;
   end Refresh_Drawing_Area;

   procedure Initialize_Selected_Machines is
   begin
      for I in Selected_Machines'Range loop
         Selected_Machines (I) := (X => 0.0, Y => 0.0, Id => Positive'First, Name => To_Unbounded_String (""));
      end loop;
   end Initialize_Selected_Machines;

begin
   Ada.Numerics.Float_Random.Reset (Gen);
   Initialize_Selected_Machines;
end Network;
