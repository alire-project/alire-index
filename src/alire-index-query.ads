package Alire.Index.Query is
   
   function Exists (Project : Project_Name) return Boolean;

   function Resolve (Deps : Dependencies) return Instance;        
   
   procedure Print_Solution (I : Instance);

end Alire.Index.Query;
