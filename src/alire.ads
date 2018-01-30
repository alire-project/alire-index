private with Ada.Containers.Indefinite_Holders;

private with GNAT.IO; -- For debugging purposes, FIXME getting rid of it and using some proper Trace lib

with Semantic_Versioning;

package Alire with Preelaborate is       
         
   File_Error : exception;
   
   
   
   type URL is new String; 
   
   
   
   subtype Project_Name is String;

   type Licenses is (Unknown);
       
   
   
   type Dependency (<>) is tagged private;               
               
   function Project (Dep : Dependency) return Project_Name;
   
   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;

   
   
   type Milestone (<>) is tagged private;
   
   function "<" (L, R : Milestone) return Boolean;
   
   function New_Milestone (Name    : Project_Name; 
                           Version : Semantic_Versioning.Version) return Milestone;
   
   function Project (M : Milestone) return Project_Name;
   
   function Version (M : Milestone) return Semantic_Versioning.Version;
   
private               
   
   use all type Semantic_Versioning.Version;
   
   package Version_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Versioning.Version_Set, Semantic_Versioning."=");
   
   type Version_Set_Holder is new Version_Holders.Holder with null record;
   
   type Dependency (Name_Len : Positive) is tagged record
      Project    : Project_Name (1 .. Name_Len);
      Versions_H : Version_Set_holder;
   end record;         
   
   function Project (Dep : Dependency) return Project_Name is (Dep.Project);
   
   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is 
     (Dep.Versions_H.Element);
   
   
   type Milestone (Name_Len : Positive) is tagged record
      Name    : Project_Name (1 .. Name_Len);
      Version : Semantic_Versioning.Version;
   end record;
   
   function "<" (L, R : Milestone) return Boolean is 
     (L.Name < R.Name or else (L.Name = R.Name and then L.Version < R.Version));
   
   function New_Milestone (Name    : Project_Name; 
                           Version : Semantic_Versioning.Version) return Milestone is 
     (Name'Length, Name, Version);      
   
   function Project (M : Milestone) return Project_Name is (M.Name);
   
   function Version (M : Milestone) return Semantic_Versioning.Version is (M.Version);
   
   
   procedure Log (S : String) renames GNAT.IO.Put_Line;
      
end Alire;
