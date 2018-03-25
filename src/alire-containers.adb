package body Alire.Containers is

   ---------------
   -- Excluding --
   ---------------

   function Excluding (Map : Release_Map; Name : Alire.Project) return Release_Map is
   begin
      return Filtered : Release_Map := Map do
         Filtered.Exclude (Name);
      end return;
   end Excluding;

   ------------
   -- To_Map --
   ------------

   function To_Map (R : Releases.Release) return Release_Map is
   begin
      return M : Release_Map do
         M.Include (R.Project, R);
      end return;
   end To_Map;

end Alire.Containers;
