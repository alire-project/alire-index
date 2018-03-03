with Alire.Platforms;
with Alire.Properties.Platform;

package Alire.Requisites.Platform with Preelaborate is

   package Plat renames Properties.Platform;

   package Compilers is new Typed_Value_Requisites (Plat.Compilers, "Compiler");
   function Compiler_Is (V : Platforms.Compilers) return Tree renames Compilers.New_Equality;

   package Distributions is new Typed_Value_Requisites (Plat.Distributions, "Distribution");
   function Distribution_Is (V : Platforms.Distributions) return Tree renames Distributions.New_Equality;

   package Systems is new Typed_Value_Requisites (Plat.Operating_Systems, "OS");
   function System_Is (V : Platforms.Operating_Systems) return Tree renames Systems.New_Equality;

   package Versions is new Typed_Value_Requisites (Plat.Versions, "Version");
   function Version_Is (V : Platforms.Versions) return Tree renames Versions.New_Equality;

   package Word_Sizes is new Typed_Value_Requisites (Plat.Word_Sizes, "Arquitecture width");
   function Word_Size_Is (V : Platforms.Word_Sizes) return Tree renames Word_Sizes.New_Equality;

end Alire.Requisites.Platform;
