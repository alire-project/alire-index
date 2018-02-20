
with Alire.Dependencies.Vectors;
with Alire.Origins;
with Alire.Properties;
with Alire.Requisites;

with Semantic_Versioning;

package Alire.Releases with Preelaborate is

   subtype Dependencies is Alire.Dependencies.Vectors.Vector;

   type Release (<>) is tagged private;

   function New_Release (Name        : Project_Name;
                         Description : Project_Description;
                         Version     : Semantic_Versioning.Version;
                         Origin      : Origins.Origin;
                         Depends_On  : Dependencies;
                         Properties  : Alire.Properties.Vector;
                         Requisites  : Alire.Requisites.Tree;
                         Native      : Boolean) return Release;

   function "<" (L, R : Release) return Boolean;

   function Project (R : Release) return Project_Name;
   function Description (R : Release) return Project_Description;
   function Version (R : Release) return Semantic_Versioning.Version;
   function Depends (R : Release) return Dependencies;
   function Origin  (R : Release) return Origins.Origin;
--     function Origin_Image (R : Release) return String;

   function Image (R : Release) return String;
   -- Unique string built as name-version-id
   function Unique_Folder (R : Release) return String renames Image;

   function Milestone_Image (R : Release) return String;
   -- project=version string

   function Is_Native (R : Release) return Boolean;
   -- not alr packaged but from the platform
   
   procedure Print (R : Release);
   -- Dump info to console

private

   type Release (Name_Len, Descr_Len : Natural) is tagged record
      Name       : Project_Name (1 .. Name_Len);
      Description: Project_Description (1 .. Descr_Len);
      Version     : Semantic_Versioning.Version;
      Origin     : Origins.Origin;
      Depends_On : Dependencies;
      Props      : Properties.Vector;
      Reqs       : Requisites.Tree;
      Native     : Boolean;
   end record;

   function New_Release (Name        : Project_Name;
                         Description : Project_Description;
                         Version     : Semantic_Versioning.Version;
                         Origin      : Origins.Origin;
                         Depends_On  : Dependencies;
                         Properties  : Alire.Properties.Vector;
                         Requisites  : Alire.Requisites.Tree;
                         Native      : Boolean) return Release is
     (Name'Length, Description'Length,
      Name,
      Description, 
      Version,
      Origin,
      Depends_On,
      Properties,
      Requisites,
      Native);
   
   use Semantic_Versioning;

   function "<" (L, R : Release) return Boolean is
     (L.Project < R.Project or else
        (L.Project = R.Project and then 
         L.Version < R.Version) or else
          (L.Project = R.Project and then 
           L.Version = R.Version and then 
           Build (L.Version) < Build (R.Version)));

   function Project (R : Release) return Project_Name is (R.Name);
   function Description (R : Release) return Project_Description is (R.Description);
   function Version (R : Release) return Semantic_Versioning.Version is (R.Version);
   function Depends (R : Release) return Dependencies is (R.Depends_On);
   function Origin  (R : Release) return Origins.Origin is (R.Origin);

   function Is_Native (R : Release) return Boolean is (R.Native);

   --  FIXME: this should be OS-sanitized to be a valid path
   function Image (R : Release) return String is
     (R.Project & "_" &
        Image (R.Version) & "_" &
      (if R.Origin.Id'Length <= 8 then R.Origin.Id 
       else R.Origin.Id (R.Origin.Id'First .. R.Origin.Id'First + 7)));

   function Milestone_Image (R : Release) return String is
     (R.Project & "=" & Image (R.Version));

--     function Repo_Image (R : Release) return String is
--        (R.Repository.Element.Image);

end Alire.Releases;
