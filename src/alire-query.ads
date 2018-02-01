with Alire.Index; use Alire.Index;

package Alire.Query is
   
   function Exists (Project : Project_Name) return Boolean;

   function Resolve (Deps : Dependencies;
                     Success    : out Boolean) return Instance;        
   
   procedure Print_Solution (I : Instance);

end Alire.Query;
