with Ada.Directories;
with Ada.Finalization;

package Alire.OS_Lib is

   function Spawn (Command   : String; 
                   Arguments : String := "") return Integer;
   --  Returns exit code
   
   procedure Spawn (Command   : String; 
                    Arguments : String:= "");
   --  Raises PROGRAM_ERROR if exit code /= 0   
   
   type Folder_Guard (<>) is limited private;
   --  use this type in conjunction with Enter_Folder to ensure that 
   --  the CWD is modified and restored when creating/destroying the Folder_Guard
   
   function Enter_Folder (Path : String) return Folder_Guard;
   
   function "/" (L, R : String) return String is
     (Ada.Directories.Compose (L, R));
   --  Shorthand for path composition
   
private
   
   type Folder_Guard (Original_Len : Positive) is new Ada.Finalization.Limited_Controlled with record
      Original    : String (1 .. Original_Len);
      Initialized : Boolean := False;
   end record;
   
   overriding procedure Finalize (This : in out Folder_Guard);   

end Alire.OS_Lib;
