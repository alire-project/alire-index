with Alire.Conditional;
with Alire.Projects;
with Alire.Roots;

with Semantic_Versioning;

package Alire.Root is

   --  When alr self-compiles it inserts a call to this function, so the dependency root is stablished

   --  The two flavors distinguish when it is an already indexed project and a new unindexed one (working copy)

   function Set (Project      : Projects.Names;
                 Version      : Semantic_Versioning.Version)
                 return Roots.Root;
   --  All information will be taken from the indexed release

   function Set (Project      : Name_String;
                 Dependencies : Conditional.Dependencies)
                 return Roots.Root;
   --  An unindexed working copy

   function Current return Roots.Root;

   function Is_Set return Boolean;

end Alire.Root;
