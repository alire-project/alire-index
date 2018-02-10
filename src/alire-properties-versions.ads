with Semantic_Versioning;

package Alire.Properties.Versions is

   type Version is new Property with record
      Value : Semantic_Versioning.Version;
   end record;

end Alire.Properties.Versions;
