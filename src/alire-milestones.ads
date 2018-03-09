with Alire.Projects;

with Semantic_Versioning;

package Alire.Milestones with Preelaborate is

   type Milestone (<>) is tagged private;

   function "<" (L, R : Milestone) return Boolean;

   function New_Milestone (Name    : Projects.Names;
                           Version : Semantic_Versioning.Version) return Milestone;

   function Project (M : Milestone) return Name_String;

   function Version (M : Milestone) return Semantic_Versioning.Version;

   function Image (M : Milestone) return String;

private

   use all type Projects.Names;

   type Milestone is tagged record
      Name    : Projects.Names;
      Version : Semantic_Versioning.Version;
   end record;

   use all type Semantic_Versioning.Version;

   function "<" (L, R : Milestone) return Boolean is
     (L.Name < R.Name or else (L.Name = R.Name and then L.Version < R.Version));

   function New_Milestone (Name    : Projects.Names;
                           Version : Semantic_Versioning.Version) return Milestone is
     (Name, Version);

   function Project (M : Milestone) return Name_String is (Projects.Image (M.Name));

   function Version (M : Milestone) return Semantic_Versioning.Version is (M.Version);

   function Image (M : Milestone) return String is
      (M.Project & "=" & Image (M.Version));

end Alire.Milestones;
