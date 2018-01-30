with Ada.Directories;

with Alire.OS_Lib;

package body Alire.Repositories.Git is

   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String) is
   begin
      OS_Lib.Spawn ("git", "clone -n -q " & R.Image & " " & Folder);

      declare
         use Ada.Directories;
         Parent : constant String := Current_Directory;
      begin
         Set_Directory (Folder);
         OS_Lib.Spawn ("git", "reset --hard -q " & Id);
         Set_Directory (Parent);
      end;
   end Checkout;

end Alire.Repositories.Git;
