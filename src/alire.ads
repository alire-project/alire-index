limited with Alire.Repositories;

with Semantic_Versioning;

package Alire with Preelaborate is        
   
   type Project (<>) is private;
   --  A name + Storage location
   
   type Project_Name is new String;
   
   function New_Project (Name : Project_Name; 
                         Repo : Repositories.Repository'Class) return Project;
   
   
   
   type Release (<>) is private;
   
   function New_Release (Prj     : Project; 
                         Version : Semantic_Versioning.Version;
                         Commit  : Repositories.Commit_ID) return Release;
      

   
   type Dependency (<>) is private;   
   
   type Dependencies (<>) is private;
   
   Nothing : constant Dependencies;
   
   function New_Dependency (Name : Project_Name; Versions : Semantic_Versioning.Version_Set) return Dependency;
   
   function "and" (Dep1, Dep2 : Dependency)               return Dependencies;
   function "and" (Deps : Dependencies; Dep : Dependency) return Dependencies;                  
   
private      
   
   type Dependency is null record;
   
   type Dependencies is null record;
   
   Nothing : constant Dependencies := (null record);
   
   type Project is null record;
   
   type Release is null record;
   
   type Release_Set is null record;
   
end Alire;
