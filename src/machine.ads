with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Machine is

   type Machine is record
      X, Y : Float;
      Id   : Positive;
      Name : Unbounded_String;
   end record;

   type Link is record
      From, To : Unbounded_String;
   end record;

end Machine;
