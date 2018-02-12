with Alire.Compilers;
with Alire.Operating_Systems;

package Alire.Properties.Platform with Preelaborate is

   package Compilers is new Values (Alire.Compilers.Compilers);
   package Operating_Systems is new Values (Alire.Operating_Systems.Operating_Systems);

   function Current return Properties.Vector;

private

   function System_Is (V : Alire.Operating_Systems.Operating_Systems) return Property'Class is
     (Operating_Systems.New_Property (V));

   function Compiler_Is (C : Alire.Compilers.Compilers)     return Property'Class is
     (Compilers.New_Property (C));

   function Current return Properties.Vector is
     (Compiler_Is (Alire.Compilers.Compiler) and
        System_Is (Alire.Operating_Systems.Current));

end Alire.Properties.Platform;
