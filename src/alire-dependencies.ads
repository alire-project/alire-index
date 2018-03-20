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

   function Unavailable return Dependency;
   --  Special never available dependency to beautify a bit textual outputs

private

   use all type Semantic_Versioning.Version;

   type Dependency is tagged record
      Project    : Projects.Names;
      Versions   : Semantic_Versioning.Version_Set;
   end record;

   function New_Dependency (Name     : Names;
                            Versions : Semantic_Versioning.Version_Set) return Dependency
   is (Name, Versions);

   function Project (Dep : Dependency) return Names is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is
     (Dep.Versions);

   use all type Projects.Names;

   function Image (Dep : Dependency) return String is -- Exceptional case: alire=0.0.0 means Unavailable
     (if Dep.Project = Projects.Alire and then Semantic_Versioning.Satisfies (V ("0"), Dep.Versions)
      then "Unavailable"
      else
        (Utils.To_Lower_Case (Dep.Project'Img) & " is " &
           Semantic_Versioning.Image (Dep.Versions)));

   function Unavailable return Dependency is
     (New_Dependency (Projects.Alire, Semantic_Versioning.Exactly (Semantic_Versioning.V ("0"))));

end Alire.Dependencies;
