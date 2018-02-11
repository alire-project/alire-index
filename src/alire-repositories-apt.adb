with Alire.OS_Lib;

package body Alire.Repositories.Apt is

   --------------
   -- Checkout --
   --------------

   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String) is
      pragma Unreferenced (R, Folder);
   begin
      OS_Lib.Spawn ("sudo", "apt install " & Id);
   end Checkout;

end Alire.Repositories.Apt;
