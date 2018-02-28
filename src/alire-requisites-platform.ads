with Alire.Platforms;
with Alire.Properties.Platform;
with Alire.Utils;

package Alire.Requisites.Platform with Preelaborate is

   function Compiler_Is_At_Least (V : Platforms.Compilers) return Requisites.Tree;

   function Distribution_Is (V : Platforms.Distributions) return Requisites.Tree;

   function System_Is (V : Platforms.Operating_Systems) return Requisites.Tree;

private

   --  Preparation for OS requisites mimicking OS properties

   use all type Platforms.Compilers;
   use all type Platforms.Distributions;
   use all type Platforms.Operating_Systems;

   package Props renames Alire.Properties.Platform;

   package System_Requisites is new Typed_Requisites (Props.Operating_Systems.Property'Class);

   function Mix (S : String) return String renames Utils.To_Mixed_Case;

   type OS_Requisite is new System_Requisites.Requisite with record
      Value : Platforms.Operating_Systems;
   end record;

   overriding function Image (R : OS_Requisite) return String is
      ("OS is " & Mix (R.Value'Image));

   overriding function Is_Satisfied (R : OS_Requisite;
                                     P : Props.Operating_Systems.Property'Class) return Boolean is
     (R.Value = P.Element);


   package Compiler_Requisites is new Typed_Requisites (Props.Compilers.Property'Class);

   type Compiler_Requisite is new Compiler_Requisites.Requisite with record
      Value : Platforms.Compilers;
   end record;

   overriding function Image (R : Compiler_Requisite) return String is
     ("Compiler >= " & Mix (R.Value'Image));

   overriding function Is_Satisfied (R : Compiler_Requisite;
                                     P : Props.Compilers.Property'Class) return Boolean is
     (R.Value <= P.Element);


   package Distro_Requisites is new Typed_Requisites (Props.Distributions.Property'Class);

   type Distro_Requisite is new Distro_Requisites.Requisite with record
      Value : Platforms.Distributions;
   end record;

   overriding function Image (R : Distro_Requisite) return String is
     ("Distribution is " & Mix (R.Value'Image));

   overriding function Is_Satisfied (R : Distro_Requisite;
                                     P : Props.Distributions.Property'Class) return Boolean is
     (R.Value = P.Element);

   use all type Tree;

   --------------------------
   -- Compiler_Is_At_Least --
   --------------------------

   function Compiler_Is_At_Least (V : Platforms.Compilers) return Requisites.Tree is
     (+Compiler_Requisite'(Value => V));

   ---------------------
   -- Distribution_Is --
   ---------------------

   function Distribution_Is (V : Platforms.Distributions) return Requisites.Tree is
      (+Distro_Requisite'(Value => V));

   ---------------
   -- System_Is --
   ---------------

   function System_Is (V : Platforms.Operating_Systems) return Requisites.Tree is
      (+OS_Requisite'(Value => V));

end Alire.Requisites.Platform;
