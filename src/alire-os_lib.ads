package Alire.OS_Lib with Preelaborate is

   function Spawn (Command   : String; 
                   Arguments : String) return Integer;
   --  Returns exit code
   
   procedure Spawn (Command   : String; 
                    Arguments : String);
   --  Raises PROGRAM_ERROR if exit code /= 0   

end Alire.OS_Lib;
