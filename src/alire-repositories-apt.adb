with Alire.OS_Lib;

package body Alire.Repositories.Apt is

   --------------
   -- Checkout --
   --------------

   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String) is
      pragma Unreferenced (R, Folder);
   begin
      Trace.Always ("sudo needed to install platform package " & Id);
      OS_Lib.Spawn_Bypass ("sudo", "apt-get install -q -q -y " & Id);
   exception
      when others =>
         Trace.Error ("Installation of native package " & Id & " failed");
   end Checkout;

end Alire.Repositories.Apt;
