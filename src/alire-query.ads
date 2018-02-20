with Alire.Containers;
with Alire.Index; use Alire.Index;

with Semantic_Versioning;

package Alire.Query is

   type Policies is (Oldest, Newest);

   --subtype Solution is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance is Containers.Release_Map; -- A list of releases complying with a Solution

   Empty_Instance : constant Instance := Containers.Project_Release_Maps.Empty_Map;

   function Exists (Project : Project_Name;
                    Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
                    return Boolean;

   function Find (Project : Project_Name;
                  Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
                  Policy  : Policies := Newest) return Release;

   function Resolve (Deps    :     Index.Dependencies;
                     Success : out Boolean;
                     Policy  :     Policies := Newest) return Instance;

   procedure Print_Solution (I : Instance);


   function Dependency_Image (Project  : Project_Name;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String;

end Alire.Query;
