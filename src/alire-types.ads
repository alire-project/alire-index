with Alire.Conditional;
with Alire.Dependencies;
with Alire.Dependencies.Vectors;
with Alire.Releases;

package Alire.Types with Preelaborate is

   --  Recopilation of types for convenient use and documentation

   subtype Dependency is Dependencies.Dependency;
   -- A single dependency on a single project+versions

   subtype Platform_Dependencies is Dependencies.Vectors.Vector;
   -- A plain vector, all dependencies must be met

   subtype Abstract_Dependencies is Conditional.Dependencies;
   -- Conditional dependencies as yet unmaterialized for a precise platform

   subtype Release is Releases.Release;
   -- A catalogued release

end Alire.Types;
