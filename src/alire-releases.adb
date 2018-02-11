with GNAT.OS_Lib;

package body Alire.Releases is

   --------------
   -- Checkout --
   --------------

   procedure Checkout (R : Release; Parent_Folder : String) is
      use GNAT.OS_Lib;
      Folder : constant String := Parent_Folder & Directory_Separator & R.Unique_Folder;
   begin
      if Is_Directory (Folder) then
         raise File_Error with "Destination of checkout already exists.";
      else
         R.Repository.Constant_Reference.Checkout (R.Id, Folder);
      end if;
   end Checkout;

end Alire.Releases;
