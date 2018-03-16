with Alire.Platforms;
with Alire.Properties.Platform;

with Alire.Requisites.Comparables;

package Alire.Requisites.Platform with Preelaborate is

   package Ps   renames Platforms;
   package PrPl renames Properties.Platform;

   use all type Ps.Compilers;
   use all type Tree;

   package Op_Systems is new Comparables
     (Ps.Operating_Systems, Ps."<", Ps.Operating_Systems'Image,
      PrPl.Operating_Systems.Property,
      PrPl.Operating_Systems.Element,
      "OS");

   package Compilers is new Comparables
     (Ps.Compilers, Ps."<", Ps.Compilers'Image,
      PrPl.Compilers.Property,
      PrPl.Compilers.Element,
      "Compiler");

   use all type Compilers.Comparable;
   function Compiler is new Compilers.Factory;

   function Compiler_Is_Native return Tree is
     (Compiler < GNAT_GPL_2017 and Compiler /= GNAT_Unknown);

   package Distributions is new Comparables
     (Ps.Distributions, Ps."<", Ps.Distributions'Image,
      PrPl.Distributions.Property,
      PrPl.Distributions.Element,
      "Distribution");

   package Versions is new Comparables
     (Ps.Versions, Ps."<", Ps.Versions'Image,
      PrPl.Versions.Property,
      PrPl.Versions.Element,
      "Version");

   package Word_Sizes is new Comparables
     (Ps.Word_Sizes, Ps."<", Ps.Word_Sizes'Image,
      PrPl.Word_Sizes.Property,
      PrPl.Word_Sizes.Element,
      "Word_Size");

end Alire.Requisites.Platform;
