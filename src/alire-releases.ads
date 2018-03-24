with Alire.Conditional;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Origins;
with Alire.Projects;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Requisites;
with Alire.Utils;
with Alire.Versions;

with Semantic_Versioning;

private with Alire.OS_Lib;

package Alire.Releases with Preelaborate is

   type Release (<>) is new Versions.Versioned with private;

   function New_Release (Name               : Projects.Names;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree) return Release;

   function New_Child (Parent             : Release;
                       Variant            : Projects.Variant_String;
                       Notes              : Description_String;
                       Dependencies       : Conditional.Dependencies;
                       Properties         : Conditional.Properties;
                       Private_Properties : Conditional.Properties;
                       Available          : Alire.Requisites.Tree) return Release;
   
   function "<" (L, R : Release) return Boolean;

   function Whenever (R : Release; P : Properties.Vector) return Release;
   --  Materialize conditions in a Release once the whatever properties are known
   --  At present dependencies and properties

   function Name (R : Release) return Projects.Names;   
   
   function Name_Img (R : Release) return Name_String;
   
   function Variant (R : Release) return String;
   --  name:variant
   
   function Is_Variant (R : Release) return Boolean;
   
   function Notes   (R : Release) return Description_String; -- Specific to release
   function Version (R : Release) return Semantic_Versioning.Version;
   
   function Description (R : Release) return Description_String;
   --  The global project description
   
   function Depends (R : Release) return Conditional.Dependencies;
   
   function Depends (R : Release;
                     P : Properties.Vector)
                     return Dependencies.Vector;
   
   function Origin  (R : Release) return Origins.Origin;
   function Available (R : Release) return Requisites.Tree;

   function Default_Executable (R : Release) return String;
   --  We encapsulate here the fixing of platform extension

   function Executables (R : Release; 
                         P : Properties.Vector) 
                         return Utils.String_Vector;
   -- Only explicity declared ones
   -- Under some conditions (usually current platform)

   function Project_Paths (R         : Release;
                           P         : Properties.Vector) return Utils.String_Set;
   --  Deduced from Project_Files
   
   function Project_Files (R         : Release;
                           P         : Properties.Vector;
                           With_Path : Boolean)
                           return Utils.String_Vector;
   --  with relative path on demand

   function Image (R : Release) return Folder_String;
   -- Unique string built as name_version_id
   function Unique_Folder (R : Release) return Folder_String renames Image;

   --  NOTE: property retrieval functions do not distinguish between public/private, since that's 
   --  merely informative for the users
   
   function On_Platform_Properties (R : Release; P : Properties.Vector) return Properties.Vector;
   --  Return properties that apply to R under platform properties P
   
   function Labeled_Properties (R : Release; P : Properties.Vector; Label : Properties.Labeled.Labels) 
                                   return Utils.String_Vector;
   --  Get all values for a given property for a given platform properties
   
   function Milestone (R : Release) return Milestones.Milestone;

   procedure Print (R : Release; Private_Too : Boolean := False);
   -- Dump info to console   

   --  Search helpers

   function Property_Contains (R : Release; Str : String) return Boolean;
   --  True if some property contains the given string
   
   --  Dependency generation helpers for all semantic versioning functions:
   --  These are here to avoid a 'body not seen' Program_Error if they were in Index
   
--     function On (Name     : Projects.Names; 
--                  Versions : Semantic_Versioning.Version_Set)
--                  return     Conditional.Dependencies;
--     
--     generic
--        with function Condition (V : Semantic_Versioning.Version) return Semantic_Versioning.Version_Set;
--     function From_Release (R : Release) return Conditional.Dependencies;
   
private
   
   use Semantic_Versioning;
   
   use all type Projects.Names;
   
   function All_Properties (R : Release) return Conditional.Properties;      

   use Alire.Properties;
   function Comment  is new Alire.Properties.Labeled.Cond_New_Label (Alire.Properties.Labeled.Comment);
   function Describe is new Alire.Properties.Labeled.Cond_New_Label (Alire.Properties.Labeled.Description);

   type Release (Var_Len, Descr_Len, Notes_Len : Natural) is new Versions.Versioned with record 
      Project      : Projects.Project (Var_Len, Descr_Len);
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Notes        : Description_String (1 .. Notes_Len);      
      Dependencies : Conditional.Dependencies;
      Properties   : Conditional.Properties;
      Priv_Props   : Conditional.Properties;
      Available    : Requisites.Tree;
   end record;

   use all type Conditional.Properties;

   function "<" (L, R : Release) return Boolean is
     (L.Variant < R.Variant or else
        (L.Variant = R.Variant and then
         L.Version < R.Version) or else
          (L.Name = R.Name and then
           L.Version = R.Version and then
           Build (L.Version) < Build (R.Version)));

   function Name    (R : Release) return Projects.Names is (Projects.Names'Value (R.Project.Name));   
   
   function Name_Img (R : Release) return Name_String is (Image (R.Name));
   
   function Variant (R : Release) return String is 
     (R.Project.Variant);
   
   function Is_Variant (R : Release) return Boolean is (Utils.Contains (R.Variant, ":"));
   
   function Description (R : Release) return Description_String is (Projects.Description (R.Name));
   function Notes (R : Release) return Description_String is (R.Notes);
   
   function Depends (R : Release) return Conditional.Dependencies is (R.Dependencies); 
   
   function Depends (R : Release;
                     P : Properties.Vector)
                     return Dependencies.Vector is (R.Dependencies.Evaluate (P));
   
   function Origin  (R : Release) return Origins.Origin is (R.Origin);
   function Available (R : Release) return Requisites.Tree is (R.Available);

   function Milestone (R : Release) return Milestones.Milestone is
      (Milestones.New_Milestone (R.Name, R.Version));

   function Default_Executable (R : Release) return String is
      (Utils.Replace (R.Variant, ":", "_") & OS_Lib.Exe_Suffix);

   use all type Origins.Kinds;
   function Image (R : Release) return Folder_String is
     (Image (R.Name) & "_" &
      Image (R.Version) & "_" &
      (case R.Origin.Kind is
          when Filesystem => "filesystem",
          when Native     => "native",
          when Git | Hg   => (if R.Origin.Commit'Length <= 8 
                              then R.Origin.Commit
                              else R.Origin.Commit (R.Origin.Commit'First .. R.Origin.Commit'First + 7))));

end Alire.Releases;
