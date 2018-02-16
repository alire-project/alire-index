with Semantic_Versioning;

package Alire.Milestones with Preelaborate is

   type Milestone (<>) is tagged private;

   function "<" (L, R : Milestone) return Boolean;

   function New_Milestone (Name    : Project_Name;
                           Version : Semantic_Versioning.Version) return Milestone;

   function Project (M : Milestone) return Project_Name;

   function Version (M : Milestone) return Semantic_Versioning.Version;

private

   type Milestone (Name_Len : Positive) is tagged record
      Name    : Project_Name (1 .. Name_Len);
      Version : Semantic_Versioning.Version;
   end record;

   use all type Semantic_Versioning.Version;

   function "<" (L, R : Milestone) return Boolean is
     (L.Name < R.Name or else (L.Name = R.Name and then L.Version < R.Version));

   function New_Milestone (Name    : Project_Name;
                           Version : Semantic_Versioning.Version) return Milestone is
     (Name'Length, Name, Version);

   function Project (M : Milestone) return Project_Name is (M.Name);

   function Version (M : Milestone) return Semantic_Versioning.Version is (M.Version);

end Alire.Milestones;
