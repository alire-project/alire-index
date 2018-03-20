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
with Alire.Versions;

with Semantic_Versioning;

package Alire.Index is

   ---------------
   --  CATALOG  --
   ---------------
   
   Catalog : Containers.Release_Set;
   
   type Catalog_Entry (<>) is new Versions.Comparable with private;
   
   function Name (C : Catalog_Entry) return Projects.Names;
   
   generic
      Name       : Projects.Names;
      
      --  The following two allow to group several releases in one index file
      --  It's a bit of an abuse, but I'm feeling lazy right now
      --  The instance should be called subproject_name instead of just project
      Parent     : Projects.Names := Name;
   function Catalogued_Project return Catalog_Entry;
   
   function Callable_String (C : Catalog_Entry) return String;
   --  Returns Name.Project, for master projects
   --  Returns Parent.Subproject_Name, for subprojects
   
   function Package_Name (C : Catalog_Entry) return String;
   --  Returns the unique part only, e.g. Alr for Alire.Index.Alr
   --  As an exception, for Alire it returns the full path

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
                      Project            : Catalog_Entry;
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
   
   function Bypass (--  Mandatory
                      Project            : Catalog_Entry;
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
   --  Does nothing: used for some examples and available to quickly retire a release (!)

   subtype Platform_Independent_Path is String with Dynamic_Predicate =>
     (for all C of Platform_Independent_Path => C /= '\');
   --  This type is used to ensure that folder separators are externally always '/',
   --  and internally properly converted to the platform one
   
   function To_Native (Path : Platform_Independent_Path) return String;
   
   ---------------------
   --  BASIC QUERIES  --
   ---------------------
   
   function Is_Currently_Indexed (Name : Projects.Names) return Boolean;
   --  It will depend on the compilation scope
   
   function Current (C : Catalog_Entry) return Release;
   --  Get newest release of C project
   
   function Get (Name : Projects.Names) return Catalog_Entry;
   --  Master entry for project

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
   
   use Versions.Expressions;
   use Versions.Expressions_With_Versioned;

   function Unavailable return Release_Dependencies;
   --  A never available release
   
   --  DEPENDENCIES BUILT FROM RELEASES
   
   --  See also Alire.Versions.Versioned'Class methods       

   subtype Version     is Semantic_Versioning.Version;
   subtype Version_Set is Semantic_Versioning.Version_Set;
   
   function Current (C : Catalog_Entry) return Conditional.Dependencies;
   
   function At_Version (C : Catalog_Entry; V : Version) return Conditional.Dependencies;
   function At_Version (C : Catalog_Entry; V : String)  return Conditional.Dependencies;
   
   function Within_Major (C : Catalog_Entry; V : Version) return Conditional.Dependencies;
   function Within_Major (C : Catalog_Entry; V : String)  return Conditional.Dependencies;
   
   function Within_Minor (C : Catalog_Entry; V : Version) return Conditional.Dependencies;
   function Within_Minor (C : Catalog_Entry; V : String)  return Conditional.Dependencies;
   
   function On_Condition (Condition  : Requisites.Tree;
                          When_True  : Release_Dependencies;
                          When_False : Release_Dependencies := No_Dependencies) 
                          return       Release_Dependencies 
                          renames      Conditional.For_Dependencies.New_Conditional;
   --  Explicitly conditional
   
   function When_Available (Preferred : Release_Dependencies;
                            Otherwise : Release_Dependencies := Unavailable) 
                            return Release_Dependencies is
     (On_Condition (Requisites.Dependencies.New_Requisite (Preferred),
                    Preferred,
                    Otherwise));
   --  Chained conditional dependencies (use first available)   
   
   function "or" (L, R : Release_Dependencies) return Release_Dependencies is (When_Available (L, R));
   --  In the sense of "or else": the first one that is resolvable will be taken
   
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
   
   function Set_Root (Project      : Catalog_Entry;
                      Version      : Semantic_Versioning.Version)
                      return Roots.Root is (Alire.Root.Set (Project.Name, Version));
   --  All information will be taken from the indexed release

   function Set_Root (Project      : Name_String;
                      Dependencies : Conditional.Dependencies)
                      return Roots.Root renames Alire.Root.Set;
   --  An unindexed working copy
   
private         
   
   type Catalog_Entry is new Versions.Comparable with record
      Name   : Projects.Names;
      Parent : Projects.Names;
   end record;      
   
   overriding 
   function New_Dependency (L : Catalog_Entry; VS : Semantic_Versioning.Version_Set)
                            return Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value -- A conditional (without condition) dependency vector
        (Dependencies.Vectors.New_Dependency (L.Name, VS)));      
   
   function Callable_String (C : Catalog_Entry) return String is
     (if C.Parent = C.Name 
      then C.Package_Name & ".Project"
      else C.Package_Name & ".Subproject_" & Utils.To_Mixed_Case (Image (C.Name)));
   
   function Package_Name (C : Catalog_Entry) return String is
     ((if C.Name = Projects.Alire
       then "Alire.Index." 
       else "") &
      (if C.Parent = C.Name 
       then Utils.To_Mixed_Case (Projects.Image (C.Name))
       else Utils.To_Mixed_Case (Projects.Image (C.Parent))));
   
   function Current (C : Catalog_Entry) return Conditional.Dependencies is
     (Conditional.New_Dependency (C.Name, Semver.Any));
   
   function At_Version (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Exactly (V)));     
   function At_Version (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Exactly (Index.V (V))));
   
   function Within_Major (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Within_Major (V)));
   function Within_Major (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Within_Major (Index.V (V))));
   
   function Within_Minor (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Within_Minor (V)));
   function Within_Minor (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Name, Semver.Within_Minor (Index.V (V))));      
   
   function Name (C : Catalog_Entry) return Projects.Names is (C.Name);
   
   function GPR_File_Unsafe is new PL.Cond_New_Label (Properties.Labeled.GPR_File);
   function GPR_Path_Unsafe is new PL.Cond_New_Label (Properties.Labeled.GPR_Path);
   
   function GPR_File (File : Platform_Independent_Path) return Release_Properties renames GPR_File_Unsafe;
   function GPR_Path (Path : Platform_Independent_Path) return Release_Properties renames GPR_Path_Unsafe;
   
   function Unavailable return Conditional.Dependencies is 
     (Conditional.For_Dependencies.New_Value -- A conditional (without condition) dependency vector
        (Dependencies.Vectors.To_Vector (Dependencies.Unavailable, 1)));
   
end Alire.Index;
