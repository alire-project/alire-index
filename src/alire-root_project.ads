with Alire.Containers;
with Alire.Dependencies.Vectors;
with Alire.Os_Lib;
with Alire.Releases;

with Semantic_Versioning;

package Alire.Root_Project is

   --  Only file needed from the project alr file (project_alr.ads).
   --  Besides the important Set_Root_Project, unfortunately it renames most of Alire.Index to
   --  make it directly visible in project_alr.ads

   Current : Alire.Containers.Release_H;
   --  Root dependency (the working project). If Is_Empty we know we must recompile,
   --  unless the hash already matches. In this case, we know the project file is
   --  missing the Set_Root_Project call

   procedure Ensure_Valid
     with Post => (not Current.Is_Empty or else raise Program_Error with "No root project when expected to exist");
   --  Graceful check that Current contains what it should.

   function Name return String
     with Pre => (not Current.Is_Empty);

   function GPR_File (Prj : Alire.Project_Name := Current.Element.Project) return String
     with Pre => (not Current.Is_Empty);
   --  The actual project root file (not the _alrbuild one!)

   function GPR_Alr_File (Prj : Alire.Project_Name := Current.Element.Project) return String
     with Pre => (not Current.Is_Empty);
   --  The alr environment project file (project_alr.gpr)

   function Enter_Root (Prj : Alire.Project_Name := Current.Element.Project) return Alire.OS_Lib.Folder_Guard
     with Pre => (not Current.Is_Empty);
   --  Enters the root folder if not already there

   function Set (Project    : Project_Name;
                 Version    : Semantic_Versioning.Version;
                 Depends_On : Dependencies.Vectors.Vector := Dependencies.Vectors.No_Dependencies)
                 return Releases.Release;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  It could be manually parsed from the file, but that's precisely what we want to avoid
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.

end Alire.Root_Project;
