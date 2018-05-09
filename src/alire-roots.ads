with Alire.Conditional;
with Alire.Containers;
with Alire.OS_Lib;
with Alire.Releases;
with Alire.Utils;

package Alire.Roots is

   --  Type used to encapsulate the information about the root release/working copy

   type Root (<>) is tagged private;

   function New_Root (R : Releases.Release) return Root;

   function New_Root (Name         : Alire.Project;
                      Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                      return         Root;

   function Default_Executable (R : Root) return String;
   function Dependencies (R : Root) return Conditional.Dependencies;
   function Is_Released  (R : Root) return Boolean;
   function Project      (R : Root) return Alire.Project;
   function Project_Base (R : Root) return String; -- see Release.Project_Base
   function Release      (R : Root) return Releases.Release with Pre => R.Is_Released;

private

   type Root (Name_Len : Natural; Project_Release : Boolean) is tagged record
      case Project_Release is
         when False =>
            Project      : Alire.Project (1 .. Name_Len);
            Dependencies : Conditional.Dependencies;
         when True =>
            Release      : Containers.Release_H;
      end case;
   end record;

   function New_Root (R : Releases.Release) return Root is
      (0, True, Containers.Release_Holders.To_Holder (R));

   function New_Root (Name         : Alire.Project;
                      Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                      return         Root is
      (Name'Length, False, Name, Dependencies);

   function Default_Executable (R : Root) return String is
     (if R.Project_Release
      then R.Release.Constant_Reference.Default_Executable
      else +R.Project & OS_Lib.Exe_Suffix);

   function Dependencies (R : Root) return Conditional.Dependencies is
     (if R.Project_Release
      then R.Release.Constant_Reference.Depends
      else R.Dependencies);

   function Is_Released  (R : Root) return Boolean is (R.Project_Release);

   function Project (R : Root) return Alire.Project is
     (if R.Project_Release
      then R.Release.Constant_Reference.Project
      else R.Project);

   function Project_Base (R : Root) return String is
      (Utils.Head (+R.Project, Extension_Separator));

   function Release      (R : Root) return Releases.Release is (R.Release.Element);

end Alire.Roots;
