with Alire.Containers;
with Alire.Depends;
with Alire.Platform;
with Alire.Properties;
with Alire.Properties.Platform;
with Alire.Releases;
with Alire.Repositories.Git;
with Alire.Requisites;

with Semantic_Versioning;

package Alire.Index is

   Releases   : Containers.Release_Set;

   subtype Dependencies is Depends.Dependencies;
   use all type Dependencies;

   subtype Release      is Alire.Releases.Release;

   subtype Solution     is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance     is Containers.Release_Map; -- A list of releases complying with a Solution

   Empty_Instance : constant Instance := Containers.Project_Release_Maps.Empty_Map;

   function V (Semantic_Version : String) return Semantic_Versioning.Version
               renames Semantic_Versioning.New_Version;

   function Register (Project     : Project_Name;
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Depends.Nothing;
                      Properties  : Alire.Properties.Vector := Alire.Properties.Property_Vectors.Empty_Vector;
                      Requisites  : Alire.Requisites.Tree   := Alire.Requisites.No_Requisites;
                      Native      : Boolean                 := False) return Release;

   function Register_Git (Project     : Project_Name;
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID;
                          Depends_On  : Dependencies := Depends.Nothing) return Release;

   -- Shortcuts to give dependencies:

   function At_Least_Within_Major (R : Release) return Dependencies;

   function At_Least  (R : Release) return Dependencies;
   function At_Most   (R : Release) return Dependencies;
   function Less_Than (R : Release) return Dependencies;
   function More_Than (R : Release) return Dependencies;
   function Exactly   (R : Release) return Dependencies;
   function Except    (R : Release) return Dependencies;

   subtype Version     is Semantic_Versioning.Version;
   subtype Version_Set is Semantic_Versioning.Version_Set;

   function At_Least_Within_Major (P : Project_Name; V : Version) return Dependencies;

   function At_Least  (P : Project_Name; V : Version) return Dependencies;
   function At_Most   (P : Project_Name; V : Version) return Dependencies;
   function Less_Than (P : Project_Name; V : Version) return Dependencies;
   function More_Than (P : Project_Name; V : Version) return Dependencies;
   function Exactly   (P : Project_Name; V : Version) return Dependencies;
   function Except    (P : Project_Name; V : Version) return Dependencies;
   
   --  Shortcuts for properties/requisites:
   use all type Platform.Operating_Systems;
   
   use all type Properties.Property'Class; -- for "and" operator
   use all type Requisites.Tree;           -- for logical operators
   
   Default_Properties : constant Properties.Vector := Properties.Property_Vectors.Empty_Vector;   
   No_Requisites      : constant Requisites.Tree   := Requisites.No_Requisites;      
   
   function Verifies (P : Properties.Property'Class) return Properties.Vector;
   function "+"      (P : Properties.Property'Class) return Properties.Vector renames Verifies;
   
   function Require (R : Requisites.Requisite'Class) return Requisites.Tree;
   function "+"     (R : Requisites.Requisite'Class) return Requisites.Tree renames Require;
   
   --  Specific shortcuts:
   function Available_On (V : Alire.Platform.Operating_Systems) return Properties.Property'Class 
                          renames Properties.Platform.Available_On;
   
   function Compiles_With (C : Alire.Platform.Compilers) return Properties.Property'Class
                           renames Properties.Platform.Compiles_With;

private

   function Register_Git (Project     : Project_Name;
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID;
                          Depends_On  : Dependencies := Depends.Nothing) return Release
   is (Register (Project,
                 Version,
                 Repositories.Git.New_Repository (String (Hosting)),
                 Repositories.Release_Id (Commit),
                 Depends_On,
                 Native => False));

   use Depends;
   use Semantic_Versioning;

   function At_Least_Within_Major (R : Release) return Dependencies is
     (New_Dependency (R.Project, At_Least_Within_Major (R.Version)));

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


   function At_Least_Within_Major (P : Project_Name; V : Version) return Dependencies is
      (Depends_On (P, At_Least_Within_Major (V)));

   function At_Least  (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, At_Least (V)));

   function At_Most   (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, At_Most (V)));

   function Less_Than (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, Less_Than (V)));

   function More_Than (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, More_Than (V)));

   function Exactly   (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, Exactly (V)));

   function Except    (P : Project_Name; V : Version) return Dependencies is
     (Depends_On (P, Except (V)));

end Alire.Index;
