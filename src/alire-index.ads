with Alire.Containers;
with Alire.Depends;
with Alire.Releases;
with Alire.Repositories.Git;

with Semantic_Versioning;

package Alire.Index is  
   
   Releases   : Containers.Release_Set;
   
   subtype Dependencies is Depends.Dependencies;
   use all type Dependencies;
   
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
   
--     function Register_Local (Project    : Project_Name;
--                              Version    : Semantic_Versioning.Version;
--                              Depends_On : Dependencies := Depends.Nothing;
--                              License    : Licenses := Unknown) return Release;
                            

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
   
--     function Register_Local (Project    : Project_Name;
--                              Version    : Semantic_Versioning.Version;
--                              Depends_On : Dependencies := Depends.Nothing;
--                              License    : Licenses := Unknown) return Release is
--       (Register (Project, 
--                  Version,
--                  Repositories.Local.Repo, 
--                  "filesystem",
--                  Depends_On,
--                  License));
   
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
