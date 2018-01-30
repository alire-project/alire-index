with Alire.Containers;
with Alire.Depends;
with Alire.Releases;
with Alire.Repositories.Git;

with Semantic_Versioning;

package Alire.Index is  
   
   Releases   : Containers.Release_Set;
   
   subtype Dependencies is Depends.Dependencies;
   subtype Release      is Alire.Releases.Release;
   
   subtype Solution     is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance     is Containers.Release_Map; -- A list of releases complying with a Solution
      
   function V (Semantic_Version : String) return Semantic_Versioning.Version 
               renames Semantic_Versioning.New_Version;
   
   function Register (Project     : Project_Name; 
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Depends.Nothing;
                      License     : Licenses := Unknown) return Release;
   
   function Register_Git (Project     : Project_Name; 
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID; 
                          Depends_On  : Dependencies := Depends.Nothing;
                          License     : Licenses := Unknown) return Release;        

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
   
   function At_Least_Within_Major (V : Version) return Version_Set renames Semantic_Versioning.At_Least_Within_Major;
   
   function At_Least  (V : Version) return Version_Set renames Semantic_Versioning.At_Least;
   function At_Most   (V : Version) return Version_Set renames Semantic_Versioning.At_Most;
   function Less_Than (V : Version) return Version_Set renames Semantic_Versioning.Less_Than;
   function More_Than (V : Version) return Version_Set renames Semantic_Versioning.More_Than;
   function Exactly   (V : Version) return Version_Set renames Semantic_Versioning.Exactly;
   function Except    (V : Version) return Version_Set renames Semantic_Versioning.Except;   
   
private
   
   function Register_Git (Project     : Project_Name; 
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID; 
                          Depends_On  : Dependencies := Depends.Nothing;
                          License     : Licenses := Unknown) return Release
   is (Register (Project,
                 Version,
                 Repositories.Git.New_Repository (String (Hosting)),
                 Repositories.Release_Id (Commit),
                 Depends_On,
                 License));
   
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
                   

end Alire.Index;
