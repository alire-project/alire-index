with Alire.Conditional;
with Alire.Dependencies.Vectors;

package Alire.Types with Preelaborate is

   --  Recopilation of types for convenient use in alr

   subtype Platform_Dependencies is Dependencies.Vectors.Vector;
   -- A plain vector

   subtype Abstract_Dependencies is Conditional.Dependencies;
   -- Conditional dependencies as yet unmaterialized for a precise platform

end Alire.Types;
