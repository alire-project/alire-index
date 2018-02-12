with Alire.Platform;
with Alire.Properties.Platform;

package Alire.Requisites.Platform with Preelaborate is

   function Available_On (V : Alire.Platform.Operating_Systems) return Requisites.Requisite'Class;

private

   --  Preparation for OS requisites mimicking OS properties

   use all type Alire.Platform.Operating_Systems;

   package Props renames Alire.Properties.Platform;

   package Operating_Systems is new Typed_Requisites (Props.Operating_Systems.Property'Class);

   type OS_Requisite is new Operating_Systems.Requisite with record
      Value : Alire.Platform.Operating_Systems;
   end record;

   overriding function Is_Satisfied (R : OS_Requisite;
                                     P : Props.Operating_Systems.Property'Class) return Boolean is
     (R.Value = P.Element);


   ------------------
   -- Available_On --
   ------------------

   function Available_On (V : Alire.Platform.Operating_Systems) return Requisites.Requisite'Class is
      (OS_Requisite'(Value => V));

end Alire.Requisites.Platform;
