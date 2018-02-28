private with Alire_Early_Elaboration; pragma Unreferenced (Alire_Early_Elaboration);

with Ada.Directories;

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

   Releases : Containers.Release_Set;

   subtype Dependencies is Alire.Dependencies.Vectors.Vector;
   
   No_Dependencies : constant Dependencies      := Alire.Dependencies.Vectors.No_Dependencies;
   No_Properties   : constant Properties.Vector := Properties.No_Properties;
   No_Requisites   : constant Requisites.Tree   := Requisites.Trees.Empty_Tree;

   subtype Release      is Alire.Releases.Release;

   function Register (--  Mandatory
                      Project      : Project_Name;
                      Version      : Semantic_Versioning.Version;                      
                      Description  : Project_Description;
                      Origin       : Origins.Origin;
                      --  Optional
                      Depends_On     : Dependencies            := No_Dependencies;
                      Properties     : Alire.Properties.Vector := No_Properties;
                      Requisites     : Alire.Requisites.Tree   := No_Requisites;
                      Available_When : Alire.Requisites.Tree   := No_Requisites) return Release;
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
   
   function Apt (Pack : String ) return Origins.Origin renames Origins.New_Apt;
   function Git (URL : Alire.URL; Commit : Origins.Git_Commit) return Origins.Origin renames Origins.New_Git;
   function Hg  (URL : Alire.URL; Commit : Origins.Hg_Commit) return Origins.Origin renames Origins.New_Hg;

   -- Shortcuts to give dependencies:

   function V (Semantic_Version : String) return Semantic_Versioning.Version
                  renames Semantic_Versioning.New_Version;

   function Current (R : Release) return Dependencies;  
   --  Within the major of R,
   --    it will accept the newest/oldest version according to the resolution policy (by default, newest)
   --  Note: it might be older than R itself
   
   function Within_Major (R : Release) return Dependencies;
   function Within_Minor (R : Release) return Dependencies;   

   function At_Least  (R : Release) return Dependencies;
   function At_Most   (R : Release) return Dependencies;
   function Less_Than (R : Release) return Dependencies;
   function More_Than (R : Release) return Dependencies;
   function Exactly   (R : Release) return Dependencies;
   function Except    (R : Release) return Dependencies;

   subtype Version     is Semantic_Versioning.Version;
   subtype Version_Set is Semantic_Versioning.Version_Set;

   function Current (P : Project_Name) return Dependencies;  
   --  Will accept the newest/oldest version according to the resolution policy (by default, newest)
   
   function Within_Major (P : Project_Name; V : Version) return Dependencies;
   function Within_Minor (P : Project_Name; V : Version) return Dependencies;

   function At_Least  (P : Project_Name; V : Version) return Dependencies;
   function At_Most   (P : Project_Name; V : Version) return Dependencies;
   function Less_Than (P : Project_Name; V : Version) return Dependencies;
   function More_Than (P : Project_Name; V : Version) return Dependencies;
   function Exactly   (P : Project_Name; V : Version) return Dependencies;
   function Except    (P : Project_Name; V : Version) return Dependencies;

   --  Shortcuts for properties/requisites:   
      
   use all type Alire.Dependencies.Vectors.Vector;
   use all type GPR.Value;
   use all type GPR.Value_Vector;
   use all type Licensing.Licenses;
   use all type Platforms.Compilers;   
   use all type Platforms.Distributions;
   use all type Platforms.Operating_Systems;
   use all type Properties.Property'Class; 
   use all type Requisites.Requisite'Class;
   use all type Requisites.Tree;           
   --  These "use all" are useful for alire-index-* packages, but not for project_alr metadata files
   
   --  "Typed" attributes (named pairs of label-value)
   function Author     is new Properties.Labeled.Generic_New_Label (Properties.Labeled.Author);   
   function Executable is new Properties.Labeled.Generic_New_Label (Properties.Labeled.Executable);
   function GPR_File   is new Properties.Labeled.Generic_New_Label (Properties.Labeled.GPR_File);
   function GPR_Free_Scenario (Name : String) return Properties.Vector;
   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Properties.Vector;
   function Maintainer is new Properties.Labeled.Generic_New_Label (Properties.Labeled.Maintainer);
   function Website    is new Properties.Labeled.Generic_New_Label (Properties.Labeled.Website);
   
   function License (L : Licensing.Licenses) return Properties.Vector is
      (+Properties.Licenses.Values.New_Property (L));

   Default_Properties : constant Properties.Vector := No_Properties;
   
   function "and" (Dep1, Dep2 : Dependencies) return Dependencies renames Alire.Dependencies.Vectors."and";
   function "and" (P1, P2 : Properties.Vector) return Properties.Vector renames Alire.Properties."and";

   function Verifies (P : Properties.Property'Class) return Properties.Vector;
   function "+"      (P : Properties.Property'Class) return Properties.Vector renames Verifies;

   function Requires (R : Requisites.Requisite'Class) return Requisites.Tree;
   function "+"      (R : Requisites.Requisite'Class) return Requisites.Tree renames Requires;

   --  Specific shortcuts:

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
                              Depends_On : Alire.Index.Dependencies := Alire.Index.No_Dependencies;
                              Properties : Alire.Properties.Vector := No_Properties)
                              return Release renames Root_Project.Set;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.
   
private   
   
   use Semantic_Versioning;  

   function Current (R : Release) return Dependencies is
      (New_Dependency (R.Project, Within_Major (New_Version (Major (R.Version)))));
   
   function Within_Major (R : Release) return Dependencies is
     (New_Dependency (R.Project, Within_Major (R.Version)));
   
   function Within_Minor (R : Release) return Dependencies is 
     (New_Dependency (R.Project, Within_Minor (R.Version)));

   function At_Least  (R : Release) return Dependencies is
     (New_Dependency (R.Project, At_Least (R.Version)));

   function At_Most   (R : Release) return Dependencies is
     (New_Dependency (R.Project, At_Most (R.Version)));

   function Less_Than (R : Release) return Dependencies is
     (New_Dependency (R.Project, Less_Than (R.Version)));

   function More_Than (R : Release) return Dependencies is
     (New_Dependency (R.Project, More_Than (R.Version)));

   function Exactly   (R : Release) return Dependencies is
     (New_Dependency (R.Project, Exactly (R.Version)));

   function Except    (R : Release) return Dependencies is
     (New_Dependency (R.Project, Except (R.Version)));


   function Current (P : Project_Name) return Dependencies is
      (New_Dependency (P, At_Least (V ("0.0.0"))));
   
   function Within_Major (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, Within_Major (V)));
   
   function Within_Minor (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, Within_Minor (V)));

   function At_Least  (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, At_Least (V)));

   function At_Most   (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, At_Most (V)));

   function Less_Than (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, Less_Than (V)));

   function More_Than (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, More_Than (V)));

   function Exactly   (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, Exactly (V)));

   function Except    (P : Project_Name; V : Version) return Dependencies is
     (New_Dependency (P, Except (V)));


   function Verifies (P : Properties.Property'Class) return Properties.Vector is
     (Properties.To_Vector (P, 1));

   function Requires (R : Requisites.Requisite'Class) return Requisites.Tree is
     (Requisites.Trees.Leaf (R));
   
   -- Property builders
   function GPR_Free_Scenario (Name : String) return Properties.Vector is
      (+Properties.Scenarios.New_Variable (GPR.Free_Variable (Name)));
   
   function GPR_Scenario (Name : String; Values : GPR.Value_Vector) return Properties.Vector is
      (+Properties.Scenarios.New_Variable (GPR.Enum_Variable (Name, Values)));

end Alire.Index;
