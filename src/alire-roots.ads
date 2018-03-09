with Alire.Conditional;
with Alire.Containers;
with Alire.OS_Lib;
with Alire.Releases;

package Alire.Roots is

   --  Type used to encapsulate the information about the root release/working copy

   type Root (<>) is tagged private;

   function New_Root (R : Releases.Release) return Root;

   function New_Root (Name         : Name_String;
                      Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                      return         Root;

   function Default_Executable (R : Root) return String;
   function Dependencies (R : Root) return Conditional.Dependencies;
   function Is_Released  (R : Root) return Boolean;
   function Name         (R : Root) return Name_String;
   function Release      (R : Root) return Releases.Release with Pre => R.Is_Released;

private

   type Root (Name_Len : Natural; Released : Boolean) is tagged record
      case Released is
         when False =>
            Name         : String (1 .. Name_Len);
            Dependencies : Conditional.Dependencies;
         when True =>
            Release      : Containers.Release_H;
      end case;
   end record;

   function New_Root (R : Releases.Release) return Root is
      (0, True, Containers.Release_Holders.To_Holder (R));

   function New_Root (Name         : Name_String;
                      Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                      return         Root is
      (Name'Length, False, Name, Dependencies);

   function Default_Executable (R : Root) return String is
     (if R.Released
      then R.Release.Constant_Reference.Default_Executable
      else R.Name & OS_Lib.Exe_Suffix);

   function Dependencies (R : Root) return Conditional.Dependencies is
     (if R.Released
      then R.Release.Constant_Reference.Depends
      else R.Dependencies);

   function Is_Released  (R : Root) return Boolean is (R.Released);

   function Name         (R : Root) return Name_String is
     (if R.Released
      then R.Release.Constant_Reference.Project
      else R.Name);

   function Release      (R : Root) return Releases.Release is (R.Release.Element);

end Alire.Roots;
