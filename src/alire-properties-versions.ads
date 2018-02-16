package Alire.Properties.Versions with Preelaborate is

   --  Internally we manage versions as a tree of conditions, so arbitrary logical expressions can be used

   package Values is new Properties.values (Semantic_Versioning.Version);

   function New_Version (V : Semantic_Versioning.Version) return Property'Class is
      (Values.New_Property (V));

end Alire.Properties.Versions;
