with Ada.Containers.Indefinite_Vectors;

package Alire.Properties with Preelaborate is

   --  Properties are the general mechanism used to store all info about a release.
   --  They can be specialized (e.g. in version, platform, compiler) but that can be transparent to the user.

   --  Since a property can be checked against a variety of conditions, this would require fully fledged
   --  multiple inheritance for the simplest design.
   --  Instead, a first check of matching tags is done and then the checks can proceed.

   type Property is tagged null record;

   package Property_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Property'Class);

   subtype Vector is Property_Vectors.Vector;

end Alire.Properties;
