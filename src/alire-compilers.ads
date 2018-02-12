private with Ada.Strings.Fixed;
private with GNAT.Compiler_Version;

package Alire.Compilers with Preelaborate is

   --  Known compilers
   type Compilers is (GNAT_Any,
                      GNAT_FSF_7_2,
                      GNAT_GPL_2017);

   function Compiler return Compilers;

private

   use Ada.Strings.Fixed;

   package GNAT_Version is new GNAT.Compiler_Version;

   function Compiler return Compilers is
     (if Index (GNAT_Version.Version, "2017") > 0   then GNAT_GPL_2017
      elsif Index (GNAT_Version.Version, "7.2") > 0 then GNAT_FSF_7_2
      else GNAT_Any);

end Alire.Compilers;
