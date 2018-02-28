with Alire.Platforms;
with Alire.Properties.Platform;

package Alire.Requisites.Platform with Preelaborate is

   function Compiler_Is_At_Least (V : Platforms.Compilers) return Requisites.Requisite'Class;

   function System_Is (V : Platforms.Operating_Systems) return Requisites.Requisite'Class;

private

   --  Preparation for OS requisites mimicking OS properties

   use all type Platforms.Compilers;
   use all type Platforms.Operating_Systems;

   package Props renames Alire.Properties.Platform;

   package System_Requisites is new Typed_Requisites (Props.Operating_Systems.Property'Class);

   type OS_Requisite is new System_Requisites.Requisite with record
      Value : Platforms.Operating_Systems;
   end record;

   overriding function Is_Satisfied (R : OS_Requisite;
                                     P : Props.Operating_Systems.Property'Class) return Boolean is
     (R.Value = P.Element);


   package Compiler_Requisites is new Typed_Requisites (Props.Compilers.Property'Class);

   type Compiler_Requisite is new Compiler_Requisites.Requisite with record
      Value : Platforms.Compilers;
   end record;

   overriding function Is_Satisfied (R : Compiler_Requisite;
                                     P : Props.Compilers.Property'Class) return Boolean is
     (R.Value <= P.Element);

   --------------------------
   -- Compiler_Is_At_Least --
   --------------------------

   function Compiler_Is_At_Least (V : Platforms.Compilers) return Requisites.Requisite'Class is
      (Compiler_Requisite'(Value => V));

   ---------------
   -- System_Is --
   ---------------

   function System_Is (V : Platforms.Operating_Systems) return Requisites.Requisite'Class is
      (OS_Requisite'(Value => V));

end Alire.Requisites.Platform;
