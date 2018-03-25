with Alire.Conditional;
with Alire.Dependencies;
with Alire.Milestones;
with Alire.Origins;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Requisites;
with Alire.Utils;
with Alire.Versions;

with Semantic_Versioning;

private with Alire.OS_Lib;

package Alire.Releases with Preelaborate is

   type Release (<>) is new Versions.Versioned with private;

   function "<" (L, R : Release) return Boolean;

   function New_Release (Project            : Alire.Project;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree) return Release;

   function Extending (Base               : Release;
                       Dependencies       : Conditional.Dependencies := Conditional.For_Dependencies.Empty;
                       Properties         : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Private_Properties : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Available          : Alire.Requisites.Tree    := Requisites.Trees.Empty_Tree)                        
                       return Release;
   --  Takes a release and merges given fields
                       
   
   function Replacing (Base               : Release;
                       Project            : Alire.Project      := "";
                       Notes              : Description_String := "") return Release;      
   --  Takes a release and replaces the given fields
   
   function Replacing (Base   : Release;
                       Origin : Origins.Origin) return Release;
   
   function Upgrading (Base    : Release;
                       Version : Semantic_Versioning.Version;
                       Origin  : Origins.Origin) return Release;
   --  Takes a release and replaces version and origin   

   function Whenever (R : Release; P : Properties.Vector) return Release;
   --  Materialize conditions in a Release once the whatever properties are known
   --  At present dependencies and properties
   
   overriding function Project (R : Release) return Alire.Project;   
   
   function Project_Base (R : Release) return String;
   --  Project up to first dot, if any; which is needed for extension projects in templates and so on
   
   function Is_Extension (R : Release) return Boolean;
   
   function Notes   (R : Release) return Description_String; -- Specific to release
   function Version (R : Release) return Semantic_Versioning.Version;
   
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

   function Unique_Folder (R : Release) return Folder_String;

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
   
--     function On (Name     : Alire.Project; 
--                  Versions : Semantic_Versioning.Version_Set)
--                  return     Conditional.Dependencies;
--     
--     generic
--        with function Condition (V : Semantic_Versioning.Version) return Semantic_Versioning.Version_Set;
--     function From_Release (R : Release) return Conditional.Dependencies;
   
private
   
   use Semantic_Versioning;
   
   function All_Properties (R : Release) return Conditional.Properties;      

   use Alire.Properties;
   function Comment  is new Alire.Properties.Labeled.Cond_New_Label (Alire.Properties.Labeled.Comment);
   function Describe is new Alire.Properties.Labeled.Cond_New_Label (Alire.Properties.Labeled.Description);

   type Release (Prj_Len, Notes_Len : Natural) is new Versions.Versioned with record 
      Project      : Alire.Project (1 .. Prj_Len);
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
     (L.Project < R.Project or else
        
      (L.Project = R.Project and then
       L.Version < R.Version) or else
      
      (L.Project = R.Project and then
       L.Version = R.Version and then
       Build (L.Version) < Build (R.Version)));
   
   function Is_Extension (R : Release) return Boolean is
      (R.Project_Base'Length < R.Project'Length);
   
   overriding function Project (R : Release) return Alire.Project is (R.Project);  
   
   function Project_Base (R : Release) return String is
      (Utils.Head (+R.Project, Extension_Separator));
   
   function Notes (R : Release) return Description_String is (R.Notes);
   
   function Depends (R : Release) return Conditional.Dependencies is (R.Dependencies); 
   
   function Depends (R : Release;
                     P : Properties.Vector)
                     return Dependencies.Vector is (R.Dependencies.Evaluate (P));
   
   function Origin  (R : Release) return Origins.Origin is (R.Origin);
   function Available (R : Release) return Requisites.Tree is (R.Available);

   function Milestone (R : Release) return Milestones.Milestone is
      (Milestones.New_Milestone (R.Project, R.Version));

   function Default_Executable (R : Release) return String is
      (Utils.Replace (+R.Project, ":", "_") & OS_Lib.Exe_Suffix);

   use all type Origins.Kinds;
   function Unique_Folder (R : Release) return Folder_String is
     (Utils.Head (+R.Project, Extension_Separator) & "_" &
      Image (R.Version) & "_" &
      (case R.Origin.Kind is
          when Filesystem => "filesystem",
          when Native     => "native",
          when Git | Hg   => (if R.Origin.Commit'Length <= 8 
                              then R.Origin.Commit
                              else R.Origin.Commit (R.Origin.Commit'First .. R.Origin.Commit'First + 7))));

end Alire.Releases;
