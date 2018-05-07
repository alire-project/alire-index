with Alire.Conditional;
with Alire.Dependencies;
--  with Alire.Dependencies.Vectors;
with Alire.Releases;

with Semantic_Versioning;

package Alire.Types with Preelaborate is

   --  Recopilation of types for convenient use and documentation

   subtype Dependency is Dependencies.Dependency;
   -- A single dependency on a single project+versions

   subtype Platform_Dependencies is Conditional.Dependencies
     with Dynamic_Predicate => Platform_Dependencies.Is_Unconditional;
   -- A plain tree without conditions (but might have OR nodes)

   subtype Abstract_Dependencies is Conditional.Dependencies;
   -- Conditional dependencies as yet unmaterialized for a precise platform

   function No_Dependencies return Conditional.Dependencies
     renames Conditional.For_Dependencies.Empty;

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Platform_Dependencies
     renames Conditional.New_Dependency;

   subtype Release is Releases.Release;
   -- A catalogued release

end Alire.Types;
