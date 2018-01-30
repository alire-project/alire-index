with Ada.Containers.Indefinite_Vectors;

package Alire.Depends with Preelaborate is

   package Dependency_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Dependency);
   subtype Dependencies is Dependency_Vectors.Vector;

   function Nothing return Dependencies is (Dependency_Vectors.Empty_Vector);

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Dependencies;
   function Depends_On (Name     : Project_Name;
                        Versions : Semantic_Versioning.Version_Set) return Dependencies renames New_Dependency;

   function "and" (Dep1, Dep2 : Dependencies) return Dependencies;

private

   use all type Dependencies;

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Dependencies
   is (To_Vector ((Name'Length, Name, To_Holder (Versions)), 1));

   function "and" (Dep1, Dep2 : Dependencies) return Dependencies is (Dep1 & Dep2);

end Alire.Depends;
