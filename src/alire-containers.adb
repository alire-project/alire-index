package body Alire.Containers is

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
