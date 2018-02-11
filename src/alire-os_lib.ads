with Ada.Directories;
with Ada.Finalization;

package Alire.OS_Lib is

   function Spawn (Command   : String;
                   Arguments : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet : Boolean := False) return Integer;
   --  If understands, an extra -v will be passed on Debug log levels

   procedure Spawn (Command   : String;
                    Arguments : String := "";
                    Understands_Verbose : Boolean := False;
                    Force_Quiet : Boolean := False);
   --  Raises PROGRAM_ERROR if exit code /= 0

   procedure Spawn_Bypass (Command   : String;
                           Arguments : String := "");
   --  Direct launch, without any shenanigangs on output, for example for respawning the canonical version

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False);

   type Folder_Guard (<>) is limited private;
   --  use this type in conjunction with Enter_Folder to ensure that
   --  the CWD is modified and restored when creating/destroying the Folder_Guard

   function Enter_Folder (Path : String) return Folder_Guard;

   function Stay_In_Current_Folder return Folder_Guard;

   function "/" (L, R : String) return String is
     (Ada.Directories.Compose (L, R));
   --  Shorthand for path composition

private

   type Folder_Guard (Original_Len : Natural) is new Ada.Finalization.Limited_Controlled with record
      Original    : String (1 .. Original_Len);
      Initialized : Boolean := False;
   end record;

   overriding procedure Finalize (This : in out Folder_Guard);

end Alire.OS_Lib;
