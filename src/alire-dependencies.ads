private with Ada.Containers.Indefinite_Holders;

limited with Alire.Dependencies.Vectors;

with Alire.Projects;
with Alire.Utils;

with Semantic_Versioning;

package Alire.Dependencies with Preelaborate is

   subtype Names is Projects.Names;

   --  A single dependency is a project name plus a version set

   type Dependency (<>) is tagged private;

   function New_Dependency (Name     : Names;
                            Versions : Semantic_Versioning.Version_Set) return Dependency;

   function Project (Dep : Dependency) return Names;

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;

   function Image (Dep : Dependency) return String;

   subtype Vector is Dependencies.Vectors.Vector;
   --  Thanks to limited with -- amazing

private

   use all type Semantic_Versioning.Version;

   package Version_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Versioning.Version_Set, Semantic_Versioning."=");

   type Version_Set_Holder is new Version_Holders.Holder with null record;

   type Dependency is tagged record
      Project    : Projects.Names;
      Versions_H : Version_Set_holder;
   end record;

   function New_Dependency (Name     : Names;
                            Versions : Semantic_Versioning.Version_Set) return Dependency
   is ((Name, To_Holder (Versions)));

   function Project (Dep : Dependency) return Names is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is
     (Dep.Versions_H.Element);

   function Image (Dep : Dependency) return String is
     (Utils.To_Lower_Case (Dep.Project'Img) & " is " &
        Semantic_Versioning.Image (Dep.Versions_H.Element));

end Alire.Dependencies;
