with Alire.Containers;

package Alire.Index.Query with Preelaborate is
   
   function Exists (Project : Project_Name) return Boolean;

   function Resolve (Deps : Dependencies) return Containers.Version_Map;  
   
   procedure Print_Solution (Solution : Containers.Version_Map);

end Alire.Index.Query;
