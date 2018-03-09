private with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies.Vectors;
with Alire.GPR;
with Alire.Licensing;
with Alire.Origins;
with Alire.Platforms;
with Alire.Projects; 
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Properties.Scenarios;
with Alire.Releases;
with Alire.Requisites;
with Alire.Requisites.Dependencies;
with Alire.Requisites.Platform;
with Alire.Root;
with Alire.Roots;
with Alire.Utils;

with Semantic_Versioning;

package Alire.Index is

   ---------------
   --  CATALOG  --
   ---------------
   
   Catalog : Containers.Release_Set;

   -----------------
   -- Index types --
   -----------------   
   
   subtype Names                is Projects.Names;
   subtype Release_Dependencies is Conditional.Dependencies;      
   subtype Release_Properties   is Conditional.Properties;   
   subtype Release_Requisites   is Requisites.Tree;

   No_Dependencies : constant Release_Dependencies := Conditional.For_Dependencies.Empty;
   No_Properties   : constant Release_Properties   := Conditional.For_Properties.Empty;
   No_Requisites   : constant Requisites.Tree      := Requisites.Trees.Empty_Tree;
   
   subtype Release is Alire.Releases.Release;

   function Register (--  Mandatory
                      Project            : Names;
                      Version            : Semantic_Versioning.Version;
                      Origin             : Origins.Origin;
                      -- we force naming beyond this point with this ugly guard:
                      XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
                      --  Optional 
                      Notes              : Description_String    := "";
                      Dependencies       : Release_Dependencies  := No_Dependencies;
                      Properties         : Release_Properties    := No_Properties;                      
                      Private_Properties : Release_Properties    := No_Properties;
                      Available_When     : Release_Requisites    := No_Requisites)
                      return Release;
   --  Properties are generally interesting to the user
   --  Private_Properties are only interesting to alr

   subtype Platform_Independent_Path is String with Dynamic_Predicate =>
     (for all C of Platform_Independent_Path => C /= '\');
   --  This type is used to ensure that folder separators are externally always '/',
   --  and internally properly converted to the platform one
   
   function To_Native (Path : Platform_Independent_Path) return String;
   
   ---------------------
   --  BASIC QUERIES  --
   ---------------------

   function Exists (Project : Name_String) return Boolean;

   function Exists (Project : Name_String;
                    Version : Semantic_Versioning.Version)
                    return Boolean;

   function Find (Project : Name_String;
                  Version : Semantic_Versioning.Version) return Release;
   
   function Value (Project : Name_String) return Names;

   ------------------------
   --  INDEXING SUPPORT  --
   ------------------------
   
   use all type Projects.Names;

   --  Shortcuts for origins:

   function Git (URL : Alire.URL; Commit : Origins.Git_Commit) return Origins.Origin renames Origins.New_Git;
   function Hg  (URL : Alire.URL; Commit : Origins.Hg_Commit) return Origins.Origin renames Origins.New_Hg;
   
   use all type Platforms.Distributions;
   
   function Packaged_As (S : String) return Origins.Package_Names renames Origins.Packaged_As;
   
   function Unavailable return Origins.Package_Names renames Origins.Unavailable;
   
   function Native (Distros : Origins.Native_Packages) return Origins.Origin renames Origins.New_Native;

   ------------------
   -- Dependencies --
   ------------------

   package Semver renames Semantic_Versioning;

   function V (Semantic_Version : String) return Semver.Version
               renames Semver.Relaxed;

   function On (Name     : Names; 
                Versions : Semver.Version_Set)
                return     Conditional.Dependencies renames Releases.On;

   --  We provide two easy shortcut forms:
   --  One, using another release, from which we'll take name and version
   --    The advantage is that strong typing is used
   --  Two, using textual name plus version
   --    Simpler if there's no exact release matching the versions we want to say
   --    Also needed for the generated _alr files which don't know about package names

   function Unavailable return Release_Dependencies renames Releases.Unavailable;
   --  A never available release
   
   function Current (R : Release) return Release_Dependencies is
     (On (R.Name, Semver.Within_Major (R.Version)));
   --  Within the major of R,
   --    it will accept the newest/oldest version according to the resolution policy (by default, newest)

   --  These take a release and use its name and version to derive a dependency
   function Within_Major is new Releases.From_Release (Semver.Within_Major);
   function Within_Minor is new Releases.From_Release (Semver.Within_Minor);
   function At_Least     is new Releases.From_Release (Semver.At_Least);
   function At_Most      is new Releases.From_Release (Semver.At_Most);
   function Less_Than    is new Releases.From_Release (Semver.Less_Than);
   function More_Than    is new Releases.From_Release (Semver.More_Than);
   function Exactly      is new Releases.From_Release (Semver.Exactly);
   function Except       is new Releases.From_Release (Semver.Except);

   subtype Version     is Semantic_Versioning.Version;
   subtype Version_Set is Semantic_Versioning.Version_Set;

   function Current (P : Names) return Release_Dependencies is (On (P, Semver.Any)); 
   
   --  These take a project name and a semantic version (see V above)
   function Within_Major is new Releases.From_Names (Semver.Within_Major);
   function Within_Minor is new Releases.From_Names (Semver.Within_Minor);
   function At_Least     is new Releases.From_Names (Semver.At_Least);
   function At_Most      is new Releases.From_Names (Semver.At_Most);
   function Less_Than    is new Releases.From_Names (Semver.Less_Than);
   function More_Than    is new Releases.From_Names (Semver.More_Than);
   function Exactly      is new Releases.From_Names (Semver.Exactly);
   function Except       is new Releases.From_Names (Semver.Except);   
   
   function On_Condition (Condition  : Requisites.Tree;
                          When_True  : Release_Dependencies;
                          When_False : Release_Dependencies := No_Dependencies) 
                          return       Release_Dependencies 
                          renames      Conditional.For_Dependencies.New_Conditional;
   --  Excplicitly conditional
   
   function When_Available (Preferred : Release_Dependencies;
                            Otherwise : Release_Dependencies := Releases.Unavailable) 
                            return Release_Dependencies is
     (On_Condition (Requisites.Dependencies.New_Requisite (Preferred),
                    Preferred,
                    Otherwise));
   --  Chained conditional dependencies (use first available)   
   
   function "and" (L, R : Release_Dependencies) return Release_Dependencies 
                   renames Conditional.For_Dependencies."and";

   ------------------
   --  Properties  --
   ------------------

   use all type Alire.Dependencies.Vectors.Vector;
   use all type GPR.Value;
   use all type GPR.Value_Vector;
   use all type Licensing.Licenses;
   use all type Platforms.Compilers;
   use all type Platforms.Operating_Systems;
   use all type Platforms.Versions;
   use all type Platforms.Word_Sizes;
   use all type Properties.Property'Class;
   use all type Release_Dependencies;
   use all type Release_Properties;
   use all type Requisites.Tree;   
   
   function On_Condition (Condition  : Requisites.Tree;
                          When_True  : Release_Properties;
                          When_False : Release_Properties := No_Properties) 
                          return       Release_Properties renames Conditional.For_Properties.New_Conditional;
   --  Conditional properties

   --  Attributes (named pairs of label-value)
   --  We need them as Properties.Vector (inside conditionals) but also as
   --    Conditional vectors (although with unconditional value inside)
   package PL renames Properties.Labeled;

   function Author           is new PL.Cond_New_Label (Properties.Labeled.Author);
   function Comment          is new PL.Cond_New_Label (Properties.Labeled.Comment);
   function Executable       is new PL.Cond_New_Label (Properties.Labeled.Executable);
   function Maintainer       is new PL.Cond_New_Label (Properties.Labeled.Maintainer);
   function Project_File     is new PL.Cond_New_Label (Properties.Labeled.Project_File);
   function Website          is new PL.Cond_New_Label (Properties.Labeled.Website);
   
   function U (Prop : Properties.Vector) return Conditional.Properties 
               renames Conditional.For_Properties.New_Value;

   --  Non-label attributes require a custom builder function
   function GPR_Free_Scenario (Name : String) return Properties.Vector is (+Properties.Scenarios.New_Property (GPR.Free_Variable (Name)));
   function GPR_Free_Scenario (Name : String) return Conditional.Properties is (U (GPR_Free_Scenario (Name)));

   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Properties.Vector is (+Properties.Scenarios.New_Property (GPR.Enum_Variable (Name, Values)));
   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Conditional.Properties is (U (GPR_Scenario (Name, Values)));

   function License (L : Licensing.Licenses) return Properties.Vector is (+Properties.Licenses.Values.New_Property (L));
   function License (L : Licensing.Licenses) return Conditional.Properties is (U (License (L)));

   function "and" (L, R : Release_Properties) return Release_Properties 
                      renames Conditional.For_Properties."and";

   ------------------------
   --  BUILD PROPERTIES  --
   ------------------------
   --  Those instruct alr on how to build, but are not the main concern of the project user
   
   function GPR_External (Name : String; Value : String) return Conditional.Properties is 
     (U (+Properties.Scenarios.New_Property (GPR.External_Value (Name, Value))));
   
   function GPR_File (File : Platform_Independent_Path) return Release_Properties;
   function GPR_Path (Path : Platform_Independent_Path) return Release_Properties;

   ------------------
   --  REQUISITES  --
   ------------------
   
   package Plat_Reqs renames Requisites.Platform;

   function Compiler is new Requisites.Platform.Compilers.Factory;
   function Compiler_Is_Native return Release_Requisites renames Plat_Reqs.Compiler_Is_Native;
   use all type Requisites.Platform.Compilers.Comparable;

   function Distribution is new Requisites.Platform.Distributions.Factory;
   use all type Requisites.Platform.Distributions.Comparable;

   function Operating_System is new Requisites.Platform.Op_Systems.Factory;
   use all type Requisites.Platform.Op_Systems.Comparable;  
   
   function Distro_Release is new Requisites.Platform.Versions.Factory;
   use all type Requisites.Platform.Versions.Comparable;
   
   function Word_Size is new Requisites.Platform.Word_Sizes.Factory;
   use all type Requisites.Platform.Word_Sizes.Comparable;

   ------------
   --  ROOT  --
   ------------
   --  The root determines the starting point to look for dependencies
   
   subtype Root is Roots.Root;
   
   function Set_Root (Project      : Projects.Names;
                      Version      : Semantic_Versioning.Version)
                      return Roots.Root renames Alire.Root.Set;
   --  All information will be taken from the indexed release

   function Set_Root (Project      : Name_String;
                      Dependencies : Conditional.Dependencies)
                      return Roots.Root renames Alire.Root.Set;
   --  An unindexed working copy
   
private
   
   function GPR_File_Unsafe is new PL.Cond_New_Label (Properties.Labeled.GPR_File);
   function GPR_Path_Unsafe is new PL.Cond_New_Label (Properties.Labeled.GPR_Path);
   
   function GPR_File (File : Platform_Independent_Path) return Release_Properties renames GPR_File_Unsafe;
   function GPR_Path (Path : Platform_Independent_Path) return Release_Properties renames GPR_Path_Unsafe;

end Alire.Index;
