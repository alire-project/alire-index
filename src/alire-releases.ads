with Alire.Conditions;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Origins;
with Alire.Properties;
with Alire.Requisites;
with Alire.Utils;

with Semantic_Versioning;

private with Alire.OS_Lib;
private with Alire.Properties.Labeled;

package Alire.Releases with Preelaborate is

   type Release (<>) is tagged private;

   function New_Release (Name        : Project_Name;
                         Description : Project_Description;
                         Version     : Semantic_Versioning.Version;
                         Origin      : Origins.Origin;
                         Depends_On  : Conditions.Dependencies.Vector;
                         Properties  : Conditions.Properties.Vector;
                         Available   : Alire.Requisites.Tree) return Release;

   function "<" (L, R : Release) return Boolean;

   function Whenever (R : Release; P : Properties.Vector) return Release;
   --  Materialize conditions in a Release once the whatever properties are known
   --  At present only platform properties

   function Project (R : Release) return Project_Name;
   function Description (R : Release) return Project_Description;
   function Version (R : Release) return Semantic_Versioning.Version;
   function Depends (R : Release;
                     P : Properties.Vector := Properties.No_Properties)
                     return Dependencies.Vector;
   function Origin  (R : Release) return Origins.Origin;
   function Available (R : Release) return Requisites.Tree;

   function Default_Executable (R : Release) return String;
   --  We encapsulate here the fixing of platform extension

   function Executables (R : Release) return Utils.String_Vector;
   -- Only explicity declared ones

   function GPR_Files (R : Release) return Utils.String_Vector;
   -- Explicitly declared ones, or if default one if none declared

   function Image (R : Release) return Path_String;
   -- Unique string built as name_version_id
   function Unique_Folder (R : Release) return Path_String renames Image;

   function Milestone (R : Release) return Milestones.Milestone;

   procedure Print (R : Release);
   -- Dump info to console

   --  Search helpers

   function Property_Contains (R : Release; Str : String) return Boolean;
   --  True if some property contains the given string

private

   use Properties;
   function Describe is new Properties.Labeled.Generic_New_Label (Properties.Labeled.Description);

   type Release (Name_Len, Descr_Len : Natural) is tagged record
      Name         : Project_Name (1 .. Name_Len);
      Description  : Project_Description (1 .. Descr_Len);
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Dependencies : Conditions.Dependencies.Vector;
      Properties   : Conditions.Properties.Vector;
      Available    : Requisites.Tree;
   end record;

   use Conditions.Properties;

   function New_Release (Name        : Project_Name;
                         Description : Project_Description;
                         Version     : Semantic_Versioning.Version;
                         Origin      : Origins.Origin;
                         Depends_On  : Conditions.Dependencies.Vector;
                         Properties  : Conditions.Properties.Vector;
                         Available   : Alire.Requisites.Tree) return Release is
     (Name'Length, Description'Length,
      Name,
      Description,
      Version,
      Origin,
      Depends_On,
      +Conditions.For_Properties.New_Unconditional (Describe (Description)) and Properties,
      Available);

   use Semantic_Versioning;

   function "<" (L, R : Release) return Boolean is
     (L.Name < R.Name or else
        (L.Name = R.Name and then
         L.Version < R.Version) or else
          (L.Name = R.Name and then
           L.Version = R.Version and then
           Build (L.Version) < Build (R.Version)));

   function Project (R : Release) return Project_Name is (R.Name);
   function Description (R : Release) return Project_Description is (R.Description);
   function Version (R : Release) return Semantic_Versioning.Version is (R.Version);
   function Depends (R : Release;
                     P : Properties.Vector := Properties.No_Properties)
                     return Dependencies.Vector is
     (R.Dependencies.Evaluate (P));
   function Origin  (R : Release) return Origins.Origin is (R.Origin);
   function Available (R : Release) return Requisites.Tree is (R.Available);

   function Milestone (R : Release) return Milestones.Milestone is
      (Milestones.New_Milestone (R.Name, R.Version));

   function Default_Executable (R : Release) return String is
      (R.Name & OS_Lib.Exe_Suffix);

   function Image (R : Release) return Path_String is
     (R.Name & "_" &
        Image (R.Version) & "_" &
      (if R.Origin.Id'Length <= 8 then R.Origin.Id
       else R.Origin.Id (R.Origin.Id'First .. R.Origin.Id'First + 7)));

end Alire.Releases;
