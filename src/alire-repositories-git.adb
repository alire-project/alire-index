with Ada.Directories;

with Alire.OS_Lib;

package body Alire.Repositories.Git is

   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String) is
   begin
      Log ("Checking out: " & R.Image);
      OS_Lib.Spawn ("git", "clone -n -q --progress " & R.Image & " " & Folder);

      declare
         use Ada.Directories;
         Parent : constant String := Current_Directory;
      begin
         Set_Directory (Folder);
         OS_Lib.Spawn ("git", "reset --hard -q " & Id);
         Set_Directory (Parent);
      end;
   exception
      when others =>
         Trace.Error ("Checkout of " & Id & " from " & R.Image & " to " & Folder & " failed");
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         raise;
   end Checkout;

end Alire.Repositories.Git;
