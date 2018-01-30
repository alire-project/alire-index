with Alire.Depends;
with Alire.Repositories;

package Alire.Releases 
with Preelaborate 
is

   subtype Dependencies is Depends.Dependencies;
   
   type Release (<>) is tagged private;      
   
   function New_Release (Project    : Project_Name;
                         Version    : Semantic_Versioning.Version;
                         Repository : Repositories.Repository'Class;
                         Id         : Repositories.Release_Id;
                         Depends_On : Dependencies) return Release;
   
   function "<" (L, R : Release) return Boolean;   
   
   function Project (R : Release) return Project_Name;
   function Version (R : Release) return Semantic_Versioning.Version;
   function Depends (R : Release) return Dependencies;
   
   function Image (R : Release) return String;
   -- Unique string built as name-version-id     
   function Unique_Folder (R : Release) return String renames Image;
   
   function Milestone_Image (R : Release) return String;
   -- project=version string
   
   procedure Checkout (R : Release; Parent_Folder : String);
   --  Appends its unique folder to Parent_Folder
   --  May raise File_Error
   
private 
   
   type Release (Name_Len, Id_Len : Positive) is tagged record
      Project    : Project_Name (1 .. Name_Len);
      Version    : Semantic_Versioning.Version;
      Repository : Repositories.Repository_H;
      Id         : Repositories.Release_Id (1 .. Id_Len);
      Depends_On : Dependencies;      
   end record;   
   
   function New_Release (Project    : Project_Name;
                         Version    : Semantic_Versioning.Version;
                         Repository : Repositories.Repository'Class;
                         Id         : Repositories.Release_Id;
                         Depends_On : Dependencies) return Release is
     (Project'Length, Id'Length,
      Project, 
      Version,
      Repositories.To_Holder (Repository),
      Id,
      Depends_On);
   
   function "<" (L, R : Release) return Boolean is 
     (L.Project < R.Project or else
        (L.Project = R.Project and then L.Version < R.Version) or else 
          (L.Project = R.Project and then 
           L.Version = R.Version and then 
           L.Repository.Element.Image < R.Repository.Element.Image));
   
   function Project (R : Release) return Project_Name is (R.Project);
   function Version (R : Release) return Semantic_Versioning.Version is (R.Version); 
   function Depends (R : Release) return Dependencies is (R.Depends_On);
   
   --  FIXME: this should be OS-sanitized to be a valid path
   function Image (R : Release) return String is
     (R.Project & "_" & Semantic_Versioning.Image (R.Version) & "_" & R.Id);
   
   function Milestone_Image (R : Release) return String is
      (R.Project & "=" & Semantic_Versioning.Image (R.Version));

end Alire.Releases;
