private with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Alire.Actions;
with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies;
--  with Alire.Dependencies.Vectors;
with Alire.GPR;
with Alire.Licensing;
with Alire.Origins;
with Alire.Platforms;
with Alire.Properties;
with Alire.Properties.Labeled;
with Alire.Properties.Licenses;
with Alire.Properties.Scenarios;
with Alire.Releases;
with Alire.Requisites;
--  with Alire.Requisites.Dependencies;
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
   --  Used to force the declaration of a single variable to refer to a project in index specs
   --  NOTE that the following generics internally use GNAT.Source_Info to
   --    ascertain the package and project names. 
   --  This is probably the hardest GNAT dependency all around,
   --    but at the same time avoids manual duplication and chance of error.
   
   generic
      Description  : Description_String;

      --  For internal use:
      First_Use    : access Boolean := new Boolean'(True);
   function Catalogued_Project return Catalog_Entry;
   --  A regular project
   --  See above note on Catalog_Entry
   
   generic
      with function Base return Catalog_Entry;
      Description    : Description_String;
      
      --  For internal use
      First_Use    : access Boolean := new Boolean'(True);
   function Extension return Catalog_Entry;
   -- A "variant/flavor" project that resides in the same package. 
   -- It may either extend or override a base project
            
   function Project (C : Catalog_Entry) return Alire.Project;
      
   function Ada_Identifier (C : Catalog_Entry) return String;
   --  Returns Name.Project, for master projects
   --  Returns Parent.Subproject_Name, for subprojects
   
   function Package_Name (C : Catalog_Entry) return String;
   --  Returns the unique part only, e.g. Alr for Alire.Index.Alr
   --  As an exception, for Alire it returns the full path

   -----------------
   -- Index types --
   -----------------   
   
   subtype Release_Dependencies is Conditional.Dependencies;      
   subtype Release_Properties   is Conditional.Properties;   
   subtype Release_Requisites   is Requisites.Tree;

   No_Dependencies : constant Release_Dependencies := Conditional.For_Dependencies.Empty;
   No_Properties   : constant Release_Properties   := Conditional.For_Properties.Empty;
   No_Requisites   : constant Requisites.Tree      := Requisites.Trees.Empty_Tree;
   No_Origin       : constant Origins.Origin       := Origins.New_Filesystem ("/unavailable");
   No_Version      : constant Semantic_Versioning.Version := Semantic_Versioning.Relaxed ("0");
   
   subtype Release is Alire.Releases.Release;

   function Register (--  Mandatory
                      This               : Catalog_Entry;
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
   
   function Register (Extension          : Catalog_Entry;
                      Extended_Release   : Release)
                      return Release;
   --  Register an extension
   --  A extension is a secondary project in the same commit as its parent release
   --  Essentially, another project file with additional properties/dependencies
   --  A extension name is parent:name (e.g.: adayaml:server)
   --  It inherits all properties (including project files)
   
   function Unreleased (This               : Catalog_Entry;
                        Version            : Semantic_Versioning.Version := No_Version;
                        Origin             : Origins.Origin        := No_Origin; 
                        Notes              : Description_String    := "";
                        Dependencies       : Release_Dependencies  := No_Dependencies;
                        Properties         : Release_Properties    := No_Properties;                      
                        Private_Properties : Release_Properties    := No_Properties;
                        Available_When     : Release_Requisites    := No_Requisites)
                        return Release;
   --  Does nothing: useful to prepare base releases for extending/upgrading
   
   ------------------------------------------------------------------
   --  NEW INDEXING FACILITIES USING Enclosing_Entity for the version
   
   generic
      Base : Release;
      with function Project return Catalog_Entry is <>;
   package Released is
      
      function Release return Index.Release;
      
      function Version return Semantic_Versioning.Version;
      function Version return Semantic_Versioning.Version_Set;
      
      function This_Version return Conditional.Dependencies;
      function Within_Major return Conditional.Dependencies;
      function Within_Minor return Conditional.Dependencies;
      
   end Released;
      
   ------------------------------------------------------------------   
   
   ---------------------
   --  BASIC QUERIES  --
   ---------------------
   
   function Is_Currently_Indexed (Name : Alire.Project) return Boolean;
   --  It will depend on the compilation scope
   
   function Current (C : Catalog_Entry) return Release;
   --  Get newest release of C project
   
   function Get (Name : Alire.Project) return Catalog_Entry;
   --  Master entry for project
   
   function Exists (Project : Alire.Project;
                    Version : Semantic_Versioning.Version)
                    return Boolean;

   function Find (Project : Alire.Project;
                  Version : Semantic_Versioning.Version) return Release;

   ------------------------
   --  INDEXING SUPPORT  --
   ------------------------
   
   use all type Alire.Project;

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

   function Case_Distribution_Is (Arr : Requisites.Platform.Distribution_Cases_Deps.Arrays) 
                                 return Release_Dependencies 
                                 renames Requisites.Platform.Distribution_Cases_Deps.Case_Is;
   
   function "or" (L, R : Release_Dependencies) return Release_Dependencies 
                  renames Conditional.For_Dependencies."or";
   --  In the sense of "or else": the first one that is available will be taken
   
   function "and" (L, R : Release_Dependencies) return Release_Dependencies 
                   renames Conditional.For_Dependencies."and";

   ------------------
   --  Properties  --
   ------------------

   use all type Actions.Moments;
--     use all type Alire.Dependencies.Vectors.Vector;
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
   
   function Case_Compiler_Is (Arr : Requisites.Platform.Compiler_Cases.Arrays) 
                              return Release_Properties
                              renames Requisites.Platform.Compiler_Cases.Case_Is;
   
   function Case_Distribution_Is (Arr : Requisites.Platform.Distribution_Cases_Props.Arrays) 
                                 return Release_Properties
                                 renames Requisites.Platform.Distribution_Cases_Props.Case_Is;
   
   function Case_Operating_System_Is (Arr : Requisites.Platform.Op_System_Cases.Arrays) 
                                      return Release_Properties
                                      renames Requisites.Platform.Op_System_Cases.Case_Is;

   --  Attributes (named pairs of label-value)
   --  We need them as Properties.Vector (inside conditionals) but also as
   --    Conditional vectors (although with unconditional value inside)
   package PL renames Properties.Labeled;

   function Author           is new PL.Cond_New_Label (Properties.Labeled.Author);
   function Comment          is new PL.Cond_New_Label (Properties.Labeled.Comment);
   function Executable       is new PL.Cond_New_Label (Properties.Labeled.Executable);
   function Maintainer       is new PL.Cond_New_Label (Properties.Labeled.Maintainer);
   function Website          is new PL.Cond_New_Label (Properties.Labeled.Website);
   
--     function U (Prop : Properties.Vector) return Conditional.Properties renames Conditional.For_Properties.New_Value;
   function U (Prop : Properties.Property'Class) return Conditional.Properties 
     renames Conditional.For_Properties.New_Value;
--       (U (+Prop));

   --  Non-label attributes or processed data require a custom builder function
   
   function GPR_Free_Scenario (Name : String) return Conditional.Properties is 
     (U (Properties.Scenarios.New_Property (GPR.Free_Variable (Name))));

   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Conditional.Properties is 
     (U (Properties.Scenarios.New_Property (GPR.Enum_Variable (Name, Values))));

   function License (L : Licensing.Licenses) return Conditional.Properties is 
     (U (Properties.Licenses.Values.New_Property (L)));

   function Project_File (File : Platform_Independent_Path) return Release_Properties;
   
   --  Concatenate
   function "and" (L, R : Release_Properties) return Release_Properties 
                      renames Conditional.For_Properties."and";

   ------------------------
   --  BUILD PROPERTIES  --
   ------------------------
   --  Those instruct alr on how to build, but are not the main concern of the project user
   
   function Action_Run (Moment           : Actions.Moments; 
                        Relative_Command : Platform_Independent_Path;
                        Working_Folder   : Platform_Independent_Path := "") return Release_Properties is
     (U (Actions.New_Run (Moment, Relative_Command, Working_Folder)));
   
   function GPR_External (Name : String; Value : String) return Conditional.Properties is 
     (U (Properties.Scenarios.New_Property (GPR.External_Value (Name, Value))));

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
                      return Roots.Root is (Alire.Root.Set (Project.Project, Version));
   --  All information will be taken from the indexed release

   function Set_Root (Project      : Alire.Project;
                      Dependencies : Conditional.Dependencies)
                      return Roots.Root renames Alire.Root.Set;
   --  An unindexed working copy
   
private         
   
   type Catalog_Entry (Name_Len, Descr_Len, Pack_Len, Self_Len : Natural) is new Versions.Comparable with record
      Project      : Alire.Project (1 .. Name_Len);
      Description  : Description_String (1 .. Descr_Len);
      Package_Name : String (1 .. Pack_Len);
      Self_Name    : String (1 .. Self_Len);
   end record;     
   
   overriding 
   function New_Dependency (L : Catalog_Entry; VS : Semantic_Versioning.Version_Set)
                            return Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value -- A conditional (without condition) dependency vector
        (Dependencies.New_Dependency (L.Project, VS)));  
   
   function Ada_Identifier (C : Catalog_Entry) return String is
     ((if Utils.To_Lower_Case (C.Package_Name) = "alire"
       then "Alire.Index.Alire"
       else Utils.To_Mixed_Case (C.Package_Name)) & 
        "." & Utils.To_Mixed_Case (C.Self_Name));
   
   function Package_Name (C : Catalog_Entry) return String is
     (Utils.To_Mixed_Case (C.Package_Name));       
      
   function Current (C : Catalog_Entry) return Conditional.Dependencies is
     (Conditional.New_Dependency (C.Project, Semver.Any));
   
   function At_Version (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Exactly (V)));     
   function At_Version (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Exactly (Index.V (V))));
   
   function Within_Major (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Within_Major (V)));
   function Within_Major (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Within_Major (Index.V (V))));
   
   function Within_Minor (C : Catalog_Entry; V : Version) return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Within_Minor (V)));
   function Within_Minor (C : Catalog_Entry; V : String)  return Conditional.Dependencies is
      (Conditional.New_Dependency (C.Project, Semver.Within_Minor (Index.V (V))));      
   
   function Project (C : Catalog_Entry) return Alire.Project is (C.Project);
   
   function Project_File_Unsafe is new PL.Cond_New_Label (Properties.Labeled.Project_File);
   
   function Project_File (File : Platform_Independent_Path) return Release_Properties is 
     (Project_File_Unsafe (Utils.To_Native (File)));
   
   function Unavailable return Conditional.Dependencies is 
     (Conditional.For_Dependencies.New_Value -- A conditional (without condition) dependency vector
        (Dependencies.Unavailable));
   
end Alire.Index;
