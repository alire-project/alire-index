with Alire.Conditional;
with Alire.Dependencies;
with Alire.Dependencies.Vectors;
with Alire.Milestones;
with Alire.Origins;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Requisites;
with Alire.Utils;

with Semantic_Versioning;

private with Alire.OS_Lib;

package Alire.Releases with Preelaborate is

   type Release (<>) is tagged private; 

   function New_Release (Name          : Project_Name;
                         Description   : Project_Description;
                         Version       : Semantic_Versioning.Version;
                         Origin        : Origins.Origin;
                         Dependencies  : Conditional.Dependencies;
                         Properties    : Conditional.Properties;
                         Available     : Alire.Requisites.Tree) return Release;

   function "<" (L, R : Release) return Boolean;

   function Whenever (R : Release; P : Properties.Vector) return Release;
   --  Materialize conditions in a Release once the whatever properties are known
   --  At present dependencies and properties

   function Project (R : Release) return Project_Name;
   function Description (R : Release) return Project_Description;
   function Version (R : Release) return Semantic_Versioning.Version;
   
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

   function GPR_Files (R : Release;
                       P : Properties.Vector) 
                       return Utils.String_Vector;
   -- Explicitly declared ones, or if default one if none declared
   -- Under some conditions (usually current platform)

   function Image (R : Release) return Folder_String;
   -- Unique string built as name_version_id
   function Unique_Folder (R : Release) return Folder_String renames Image;

   function On_Platform_Properties (R : Release; P : Properties.Vector) return Properties.Vector;
   --  Return properties that apply to R under platform properties P
   
   function Labeled_Properties (R : Release; P : Properties.Vector; Label : Properties.Labeled.Labels) 
                                   return Utils.String_Vector;
   --  Get all values for a given property for a given platform properties
   
   function Milestone (R : Release) return Milestones.Milestone;

   procedure Print (R : Release);
   -- Dump info to console   

   --  Search helpers

   function Property_Contains (R : Release; Str : String) return Boolean;
   --  True if some property contains the given string
   
   --  Dependency generation helpers for all semantic versioning functions:
   --  These are here to avoid a 'body not seen' Program_Error if they were in Index
   
   function On (Name     : Project_Name; 
                Versions : Semantic_Versioning.Version_Set)
                return     Conditional.Dependencies;
   
   generic
      with function Condition (V : Semantic_Versioning.Version) return Semantic_Versioning.Version_Set;
   function From_Release (R : Release) return Conditional.Dependencies;
   
   generic
      with function Condition (V : Semantic_Versioning.Version) return Semantic_Versioning.Version_Set;
   function From_Names (P : Project_Name; 
                        V : Semantic_Versioning.Version) return Conditional.Dependencies;   

   function Unavailable return Conditional.Dependencies;
   --  A never available dependency that is useful in conditional chained dependencies (see Index)
   
private
   
   function Unavailable return Conditional.Dependencies is 
     (On ("alire_unavailable", Semantic_Versioning.Any));

   use Alire.Properties;
   function Describe is new Alire.Properties.Labeled.Cond_New_Label (Alire.Properties.Labeled.Description);

   type Release (Name_Len, Descr_Len : Natural) is tagged record
      Name         : Project_Name (1 .. Name_Len);
      Description  : Project_Description (1 .. Descr_Len);
      Version      : Semantic_Versioning.Version;
      Origin       : Origins.Origin;
      Dependencies : Conditional.Dependencies;
      Properties   : Conditional.Properties;
      Available    : Requisites.Tree;
   end record;

   use all type Conditional.Properties;
   
   function New_Release (Name          : Project_Name;
                         Description   : Project_Description;
                         Version       : Semantic_Versioning.Version;
                         Origin        : Origins.Origin;
                         Dependencies  : Conditional.Dependencies;
                         Properties    : Conditional.Properties;
                         Available     : Alire.Requisites.Tree) return Release is
     (Name'Length, Description'Length,
      Name,
      Description,
      Version,
      Origin,
      Dependencies,
      Describe (Description) and Properties,
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
                     P : Properties.Vector)
                     return Dependencies.Vector is (R.Dependencies.Evaluate (P));
   
   function Origin  (R : Release) return Origins.Origin is (R.Origin);
   function Available (R : Release) return Requisites.Tree is (R.Available);

   function Milestone (R : Release) return Milestones.Milestone is
      (Milestones.New_Milestone (R.Name, R.Version));

   function Default_Executable (R : Release) return String is
      (R.Name & OS_Lib.Exe_Suffix);

   use all type Origins.Kinds;
   function Image (R : Release) return Folder_String is
     (R.Name & "_" &
        Image (R.Version) & "_" &
      (case R.Origin.Kind is
          when Filesystem => "filesystem",
          when Native     => "native",
          when Git | Hg   => (if R.Origin.Commit'Length <= 8 
                              then R.Origin.Commit
                              else R.Origin.Commit (R.Origin.Commit'First .. R.Origin.Commit'First + 7))));
   
   --  Dependency helpers
         
   function On (Name     : Project_Name; 
                Versions : Semantic_Versioning.Version_Set)
                return     Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value -- A conditional (without condition) dependency vector
        (Dependencies.Vectors.New_Dependency (Name, Versions))); -- A dependency vector
   
   function From_Release (R : Release) return Conditional.Dependencies is
     (On (R.Project, Condition (R.Version)));
   
   function From_Names (P : Project_Name; 
                        V : Semantic_Versioning.Version) return Conditional.Dependencies is
      (On (P, Condition (V)));

end Alire.Releases;
