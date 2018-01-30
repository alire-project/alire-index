with GNAT.OS_Lib;

package body Alire.OS_Lib is

   use GNAT.OS_Lib;

   --------------------
   -- Locate_In_Path --
   --------------------

   function Locate_In_Path (Name : String) return String is
      Target : String_Access := Locate_Exec_On_Path (Name);
   begin
      if Target /= null then
         return Result : constant String := Target.all do
            Free (Target);
         end return;
      else
         raise Program_Error with "Could not locate " & Name & " in $PATH";
      end if;
   end Locate_In_Path;

   -----------
   -- Spawn --
   -----------
   -- FIXME: memory leaks
   function Spawn (Command   : String;
                   Arguments : String) return Integer is
     (Spawn (Locate_In_Path (Command),
             Argument_String_To_List (Arguments).all));

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command   : String;
                    Arguments : String)
   is
      Code : constant Integer := Spawn (Command, Arguments);
   begin
      if Code /= 0 then
         raise Program_Error with "Exit code:" & Code'Image;
      end if;
   end Spawn;

end Alire.OS_Lib;
