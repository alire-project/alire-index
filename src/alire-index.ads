with Alire.Repositories.Git;

with Semantic_Versioning;

package Alire.Index with Preelaborate is  

   type Milestone (<>) is private;
   
   function Register (Released    : Release;
                      Depends_On  : Dependencies := Nothing) return Milestone;
   --  General registering function
   
   
      
   --  SHORTCUTS TO SIMPLIFY INDEX ENTRIES REGISTRATION  --
   
   function V (Semantic_Version : String) return Semantic_Versioning.Version renames Semantic_Versioning.New_Version;
   
      
   
   function Git_Project (Name : Project_Name; Git_URL : String) return Project;
   
   function Git_Release (Prj     : Project;
                         Version : Semantic_Versioning.Version;
                         Commit  : Repositories.Git.Commit_ID) return Release;
   
   function Register_Git_Milestone (Name        : Project_Name; 
                                    Version     : Semantic_Versioning.Version;
                                    Git_URL	: String;
                                    Commit      : Repositories.Git.Commit_ID;
                                    Depends_On  : Dependencies := Nothing) return Milestone;   
   --  Single git releases
   
private
   
   type Milestone is null record;

end Alire.Index;
