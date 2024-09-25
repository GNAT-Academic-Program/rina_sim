with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Machine_Model is
   use Ada.Strings.Unbounded;

   type Machine_ID is new Positive;

   type Machine is record
      ID   : Machine_ID;
      Name : Unbounded_String;
      X, Y : Integer;
   end record;

   type Machine_Access is access all Machine;

   package Machine_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Machine_Access);

   type Machine_Manager is tagged private;

   procedure Create_Machine (Self : in out Machine_Manager; X, Y : Integer);
   procedure Destroy_Machine (Self : in out Machine_Manager; ID : Machine_ID);
   function Get_Machines (Self : Machine_Manager) return Machine_Vectors.Vector;

private
   type Machine_Manager is tagged record
      Machines : Machine_Vectors.Vector;
      Next_ID  : Machine_ID := 1;
   end record;

end Machine_Model;