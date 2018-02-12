with Alire.Platform;

package Alire.Properties.Platform with Preelaborate is

   package Compilers is new Values (Alire.Platform.Compilers);
   package Operating_Systems is new Values (Alire.Platform.Operating_Systems);

   function Available_On (V : Alire.Platform.Operating_Systems) return Property'Class;

   function Compiles_With (C : Alire.Platform.Compilers)     return Property'Class;

private

   function Available_On (V : Alire.Platform.Operating_Systems) return Property'Class is
     (Operating_Systems.New_Property (V));

   function Compiles_With (C : Alire.Platform.Compilers)     return Property'Class is
      (Compilers.New_Property (C));

end Alire.Properties.Platform;
