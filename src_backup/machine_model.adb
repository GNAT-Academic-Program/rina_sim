with Ada.Strings.Fixed;

package body Machine_Model is

   procedure Create_Machine (Self : in out Machine_Manager; X, Y : Integer) is
      New_Machine : constant Machine_Access := new Machine'(
         ID   => Self.Next_ID,
         Name => To_Unbounded_String ("Machine" & Ada.Strings.Fixed.Trim (Machine_ID'Image (Self.Next_ID), Ada.Strings.Left)),
         X    => X,
         Y    => Y);
   begin
      Self.Machines.Append (New_Machine);
      Self.Next_ID := Self.Next_ID + 1;
   end Create_Machine;

   procedure Destroy_Machine (Self : in out Machine_Manager; ID : Machine_ID) is
      use Machine_Vectors;
   begin
      for I in Self.Machines.First_Index .. Self.Machines.Last_Index loop
         if Self.Machines (I).ID = ID then
            Self.Machines.Delete (I);
            return;
         end if;
      end loop;
   end Destroy_Machine;

   function Get_Machines (Self : Machine_Manager) return Machine_Vectors.Vector is
   begin
      return Self.Machines;
   end Get_Machines;

end Machine_Model;