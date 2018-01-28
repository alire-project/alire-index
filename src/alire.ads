with Semantic_Versioning;

package Alire with Preelaborate is        
   
   type URL is new String; 
   
   
   
   type Project_Name is new String;

   type Licenses is (Unknown);
       
   
   
   type Dependencies (<>) is private;
   
   Nothing : constant Dependencies;
   
   function New_Dependency (Name     : Project_Name; 
                            Versions : Semantic_Versioning.Version_Set) return Dependencies;
   function Depends_On (Name     : Project_Name; 
                        Versions : Semantic_Versioning.Version_Set) return Dependencies renames New_Dependency;
   
   function "and" (Dep1, Dep2 : Dependencies) return Dependencies;
   
private      
   
   type Dependencies is null record;
   
   Nothing : constant Dependencies := (null record);
   
   
   function New_Dependency (Name     : Project_Name; 
                            Versions : Semantic_Versioning.Version_Set) return Dependencies
   is (null record);
   
   function "and" (Dep1, Dep2 : Dependencies) return Dependencies is (null record);
      
end Alire;
