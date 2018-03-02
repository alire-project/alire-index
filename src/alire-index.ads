private with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Ada.Directories;

with Alire.Conditions;
with Alire.Containers;
with Alire.Dependencies.Vectors;
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
with Alire.Requisites.Platform;
with Alire.Root_Project;

with Semantic_Versioning;

package Alire.Index is

   Catalog : Containers.Release_Set;

   subtype Release_Dependencies is Conditions.Dependencies.Vector;
   subtype Release_Properties is Conditions.Properties.Vector;

   No_Dependencies : constant Release_Dependencies := Conditions.Dependencies.Empty_Vector;
   No_Properties   : constant Release_Properties   := Conditions.Properties.Empty_Vector;
   No_Requisites   : constant Requisites.Tree      := Requisites.Trees.Empty_Tree;

   subtype Release      is Alire.Releases.Release;

   function Register (--  Mandatory
                      Project      : Project_Name;
                      Version      : Semantic_Versioning.Version;
                      Description  : Project_Description;
                      Origin       : Origins.Origin;
                      --  Optional
                      Depends_On     : Release_Dependencies  := No_Dependencies;
                      Properties     : Release_Properties    := No_Properties;
                      Available_When : Alire.Requisites.Tree := No_Requisites)
                      return Release;
   --  Properties are of the Release
   --  Requisites are properties that dependencies have to fulfill, not used yet.
   --  Available_On are properties the platform has to fulfill.

   ---------------------
   --  BASIC QUERIES  --
   ---------------------


   function Exists (Project : Project_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean;

   function Find (Project : Project_Name;
                  Version : Semantic_Versioning.Version) return Release;

   ------------------------
   --  INDEXING SUPPORT  --
   ------------------------

   --  Shortcuts for common origins:

   function Native (Pack : String ) return Origins.Origin renames Origins.New_Native;
   function Git (URL : Alire.URL; Commit : Origins.Git_Commit) return Origins.Origin renames Origins.New_Git;
   function Hg  (URL : Alire.URL; Commit : Origins.Hg_Commit) return Origins.Origin renames Origins.New_Hg;

   -- Shortcuts to give dependencies:

   package Semver renames Semantic_Versioning;

   function V (Semantic_Version : String) return Semver.Version
               renames Semver.New_Version;

   function On (Name     : Project_Name; 
                Versions : Semver.Version_Set)
                return     Conditions.Dependencies.Vector renames Releases.On;

   --  We provide two easy shortcut forms:
   --  One, using another release, from which we'll take name and version
   --    The advantage is that strong typing is used
   --  Two, using textual name plus version
   --    Simpler if there's no exact release matching the versions we want to say
   --    Also needed for the generated _alr files which don't know about package names

   function Current (R : Release) return Release_Dependencies is
     (On (R.Project, Semver.Within_Major (Semver.New_Version (Semver.Major (R.Version)))));
   --  Within the major of R,
   --    it will accept the newest/oldest version according to the resolution policy (by default, newest)
   --  Note: it might be older than R itself

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

   function Current (P : Project_Name) return Release_Dependencies is (On (P, Semver.Any));
   
   --  These take a project name and a version string
   function Within_Major is new Releases.From_Names (Semver.Within_Major);
   function Within_Minor is new Releases.From_Names (Semver.Within_Minor);
   function At_Least     is new Releases.From_Names (Semver.At_Least);
   function At_Most      is new Releases.From_Names (Semver.At_Most);
   function Less_Than    is new Releases.From_Names (Semver.Less_Than);
   function More_Than    is new Releases.From_Names (Semver.More_Than);
   function Exactly      is new Releases.From_Names (Semver.Exactly);
   function Except       is new Releases.From_Names (Semver.Except);   

   ------------------
   --  PROPERTIES  --
   ------------------
   -- (as vectors of conditionals) --

   use all type Alire.Dependencies.Vectors.Vector;
   use all type GPR.Value;
   use all type GPR.Value_Vector;
   use all type Licensing.Licenses;
   use all type Platforms.Compilers;
   use all type Platforms.Distributions;
   use all type Platforms.Operating_Systems;
   use all type Properties.Property'Class;
   use all type Release_Dependencies;
   use all type Release_Properties;
   use all type Requisites.Tree;

   --  Function for introducing conditional properties depending on platform conditions
   function If_Platform (Condition  : Requisites.Tree;
                         When_True  : Dependencies.Vector;
                         When_False : Dependencies.Vector := Dependencies.Vectors.No_Dependencies) 
                         return       Release_Dependencies;
   
   function If_Platform (Condition  : Requisites.Tree;
                         When_True  : Properties.Vector;
                         When_False : Properties.Vector := Properties.No_Properties) 
                            return    Release_Properties;

   --  Attributes (named pairs of label-value)
   --  We need them as Properties.Vector (inside conditionals) but also as
   --    Conditional vectors (although with unconditional value inside)
   package PL renames Properties.Labeled;

   function Author is new PL.Generic_New_Label (Properties.Labeled.Author);
   function Author is new PL.Unconditional_New_Label (Properties.Labeled.Comment);

   function Comment is new PL.Generic_New_Label (Properties.Labeled.Comment);
   function Comment is new PL.Unconditional_New_Label (Properties.Labeled.Comment);

   function Executable is new PL.Generic_New_Label (Properties.Labeled.Executable);
   function Executable is new PL.Unconditional_New_Label (Properties.Labeled.Executable);

   function GPR_File is new PL.Generic_New_Label (Properties.Labeled.GPR_File);
   function GPR_File is new PL.Unconditional_New_Label (Properties.Labeled.Executable);

   function Maintainer is new PL.Generic_New_Label (Properties.Labeled.Maintainer);
   function Maintainer is new PL.Unconditional_New_Label (Properties.Labeled.Maintainer);

   function Website is new PL.Generic_New_Label (Properties.Labeled.Website);
   function Website is new PL.Unconditional_New_Label (Properties.Labeled.Website);
   
   function U (Prop : Properties.Vector) return Conditions.Properties.Vector 
     renames Conditions.Properties.New_Unconditional;

   --  Non-label attributes require a custom builder function
   function GPR_Free_Scenario (Name : String) return Properties.Vector is (+Properties.Scenarios.New_Variable (GPR.Free_Variable (Name)));
   function GPR_Free_Scenario (Name : String) return Conditions.Properties.Vector is (U (GPR_Free_Scenario (Name)));

   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Properties.Vector is (+Properties.Scenarios.New_Variable (GPR.Enum_Variable (Name, Values)));
   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Conditions.Properties.Vector is (U (GPR_Scenario (Name, Values)));

   function License (L : Licensing.Licenses) return Properties.Vector is (+Properties.Licenses.Values.New_Property (L));
   function License (L : Licensing.Licenses) return Conditions.Properties.Vector is (U (License (L)));

   function "and" (D1, D2 : Dependencies.Vector) return Dependencies.Vector renames Alire.Dependencies.Vectors."and";
   function "and" (P1, P2 : Properties.Vector) return Properties.Vector renames Alire.Properties."and";

--     function Verifies (P : Properties.Property'Class) return Properties.Vector;
--     function "+"      (P : Properties.Property'Class) return Properties.Vector renames Verifies;
--
--     function Requires (R : Requisites.Requisite'Class) return Requisites.Tree;
--     function "+"      (R : Requisites.Requisite'Class) return Requisites.Tree renames Requires;

   ------------------
   --  REQUISITES  --
   ------------------

   function Compiler_Is_At_Least (V : Platforms.Compilers) return Requisites.Tree
                                  renames Requisites.Platform.Compiler_Is_At_Least;

   function Distribution_Is (V : Platforms.Distributions) return Requisites.Tree
                             renames Requisites.Platform.Distribution_Is;

   function System_is (V : Platforms.Operating_Systems) return Requisites.Tree
                       renames Requisites.Platform.System_Is;

   --  Other useful functions

   function "/" (L, R : String) return String is (Ada.Directories.Compose  (L, R));
   --  Path composition.
   --  FIXME: hardcoded path separators shouldn't reach the index, not sure how to force-prevent this...

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set_Root_Project (Project    : Alire.Project_Name;
                              Version    : Semantic_Versioning.Version;
                              Depends_On : Conditions.Dependencies.Vector :=
                                Conditions.Dependencies.Empty_Vector)
                              return Release renames Root_Project.Set;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.

private

--     function Verifies (P : Properties.Property'Class) return Properties.Vector is
--       (Properties.To_Vector (P, 1));
--  
--     function Requires (R : Requisites.Requisite'Class) return Requisites.Tree is
--       (Requisites.Trees.Leaf (R));

   function If_Platform (Condition  : Requisites.Tree;
                         When_True  : Dependencies.Vector;
                         When_False : Dependencies.Vector := Dependencies.Vectors.No_Dependencies) 
                         return       Release_Dependencies is 
     (Conditions.Dependencies.New_Conditional (Condition, When_True, When_False));

   function If_Platform (Condition  : Requisites.Tree;
                         When_True  : Properties.Vector;
                         When_False : Properties.Vector := Properties.No_Properties) 
                         return       Release_Properties is
      (Conditions.Properties.New_Conditional (Condition, When_True, When_False));

end Alire.Index;
