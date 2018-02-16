with Ada.Containers.Indefinite_Vectors;

package Alire.Dependencies.Vectors with Preelaborate is

   --  Dependencies are a plain list (vector) of individual dependencies
   --  There's nothing preventing giving version sets on the same project as distinct dependencies

   package Dependency_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Dependency);

   type Vector is new Dependency_Vectors.Vector with private;

   function No_Dependencies return Vector;

   --  Creation of dependency vectors

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Vector;

   function "and" (Dep1, Dep2 : Vector) return Vector is (Dep1 & Dep2);

private

   type Vector is new Dependency_Vectors.Vector with null record;
   --  New type so the "and" function is primitive

   function No_Dependencies return Vector is (Dependency_Vectors.Empty_Vector with null record);

   --------------------
   -- New_Dependency --
   --------------------

   function New_Dependency (Name     : Project_Name;
                            Versions : Semantic_Versioning.Version_Set) return Vector is
     (To_Vector ((Name'Length, Name, To_Holder (Versions)), 1));

end Alire.Dependencies.Vectors;
