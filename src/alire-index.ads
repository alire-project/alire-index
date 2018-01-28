with Alire.Repositories.Git;

with Semantic_Versioning;

package Alire.Index with Preelaborate is  
      
   function V (Semantic_Version : String) return Semantic_Versioning.Version 
               renames Semantic_Versioning.New_Version;
   
   type Release (<>) is private;   
   
   function Register (Project     : Project_Name; 
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Nothing;
                      License     : Licenses := Unknown) return Release;
   
   function Register_Git (Project     : Project_Name; 
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID; 
                          Depends_On  : Dependencies := Nothing;
                          License     : Licenses := Unknown) return Release;          

   -- Shortcuts to give dependencies:
   
   function At_Least_Within_Major (R : Release) return Dependencies;
   
   function At_Least  (V : Release) return Dependencies;
   function At_Most   (V : Release) return Dependencies;
   function Less_Than (V : Release) return Dependencies;
   function More_Than (V : Release) return Dependencies;
   function Exactly   (V : Release) return Dependencies;
   function Except    (V : Release) return Dependencies;
   
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
   
   type Release is null record;
   
   function Register_Git (Project     : Project_Name; 
                          Version     : Semantic_Versioning.Version;
                          Hosting     : URL;
                          Commit      : Repositories.Git.Commit_ID; 
                          Depends_On  : Dependencies := Nothing;
                          License     : Licenses := Unknown) return Release
   is (Register (Project,
                 Version,
                 Repositories.Git.New_Repository (String (Hosting)),
                 Repositories.Release_Id (Commit),
                 Depends_On,
                 License));
   
   function At_Least_Within_Major (R : Release) return Dependencies is (null record);
   
   function At_Least  (V : Release) return Dependencies is (null record);
   function At_Most   (V : Release) return Dependencies is (null record);
   function Less_Than (V : Release) return Dependencies is (null record);
   function More_Than (V : Release) return Dependencies is (null record);
   function Exactly   (V : Release) return Dependencies is (null record);
   function Except    (V : Release) return Dependencies is (null record);
                   

end Alire.Index;
