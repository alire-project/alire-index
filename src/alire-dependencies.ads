private with Ada.Containers.Indefinite_Holders;

with Semantic_Versioning;

package Alire.Dependencies with Preelaborate is

   --  A single dependency is a project name plus a version set

   type Dependency (<>) is tagged private;

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Dependency;

   function Project (Dep : Dependency) return Project_Name;

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;


private

   use all type Semantic_Versioning.Version;

   package Version_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Versioning.Version_Set, Semantic_Versioning."=");

   type Version_Set_Holder is new Version_Holders.Holder with null record;

   type Dependency (Name_Len : Positive) is tagged record
      Project    : Project_Name (1 .. Name_Len);
      Versions_H : Version_Set_holder;
   end record;

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Dependency
   is ((Name'Length, Name, To_Holder (Versions)));

   function Project (Dep : Dependency) return Project_Name is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is
     (Dep.Versions_H.Element);

end Alire.Dependencies;
