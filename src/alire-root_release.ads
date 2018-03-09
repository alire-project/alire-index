with Alire.Conditional;
with Alire.Releases;

with Semantic_Versioning;

package Alire.Root_Release is

   --  When alr self-compiles it inserts a call to this function, so the dependency root is stablished

   function Set (Project      : Name_String;
                 Version      : Semantic_Versioning.Version;
                 Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                 return Releases.Release;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  It could be manually parsed from the file, but that's precisely what we want to avoid
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.

   function Current return Releases.Release;

   function Is_Set return Boolean;

end Alire.Root_Release;
