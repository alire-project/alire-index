with Alire.Conditional;
with Alire.Releases;

with Semantic_Versioning;

package Alire.Root_Project is

   --  Only file needed from the project alr file (project_alr.ads).
   --  Besides the important Set_Root_Project, unfortunately it renames most of Alire.Index to
   --  make it directly visible in project_alr.ads

   function Set (Project    : Project_Name;
                 Version    : Semantic_Versioning.Version;
                 Dependencies : Conditional.Dependencies := Conditional.For_Dependencies.Empty)
                 return Releases.Release;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  It could be manually parsed from the file, but that's precisely what we want to avoid
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.

   function Current return Releases.Release;

   function Is_Set return Boolean;

end Alire.Root_Project;
