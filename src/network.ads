with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Machine;
with Gtk.Drawing_Area;
with Cairo;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Network is
   procedure Add_Machine;
   procedure Draw_Network
     (Area : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Cr   : Cairo.Cairo_Context);
   procedure Update_Drawing_Area_Size (Width, Height : Float);
   procedure Select_Machine (X, Y : Float);
   procedure Create_Link_Between_Selected;
   procedure Create_Link;
   function Get_Selected_Count return Natural;
   procedure Set_Drawing_Area (Area : Gtk.Drawing_Area.Gtk_Drawing_Area);
   procedure Remove_Machine (Name : String);
   procedure Remove_Selected_Machines (Removed_Count : out Natural);

private

   function "=" (Left, Right : Machine.Machine) return Boolean is
     (Left.Id = Right.Id and then Left.Name = Right.Name and then
      Left.X = Right.X and then Left.Y = Right.Y);

   package Machine_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Machine.Machine,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   Machines : Machine_Maps.Map;

   function "=" (Left, Right : Machine.Link) return Boolean is
     (Left.From = Right.From and then Left.To = Right.To);

   package Link_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Machine.Link,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   Links : Link_Maps.Map;

   Drawing_Area_Width  : Float := 800.0;
   Drawing_Area_Height : Float := 600.0;

   type Selected_Machines_Array is array (1 .. 2) of Machine.Machine;
   Selected_Machines : Selected_Machines_Array;
   Selected_Count : Natural := 0;
end Network;