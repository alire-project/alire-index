with GNAT.Expect;
with GNAT.OS_Lib;

with System;

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

   -------------------------
   -- Spawn_With_Progress --
   -------------------------

   function Spawn_With_Progress (Command      : String;
                                 Arguments    : String;
                                 Percent_Only : Boolean) return Integer
   is
      use GNAT.Expect;

      procedure Filter
        (Descriptor : Process_Descriptor'Class;
         Str        : String;
         User_Data  : System.Address)
      is
         pragma Unreferenced (Descriptor, User_Data);
      begin
         Log ("X: " & Str, Always);
      end Filter;

      Pid : Process_Descriptor;

      Trash : Expect_Match;
   begin
      Log ("A: " & Command & " " & Arguments, Always);
      Non_Blocking_Spawn
        (Pid,
         Command,
         Argument_String_To_List (Arguments).all,
         Err_To_Out => True);
      Log ("B", Always);
      Add_Filter (Pid, Filter'Unrestricted_Access); -- No fear, clearly in scope
      Log ("C", Always);

      loop
         begin
            Expect (Pid, Trash, "");
            Log ("D", Always);
         exception
            when Process_Died =>
               exit;
         end;
      end loop;
      Log ("D2", Always);

      return Code : Integer do
         Close (Pid, Code);
         Log ("E", Always);
      end return;
   exception
      when Process_Died =>
         Log ("THE END", Always);
         raise;
   end Spawn_With_Progress;

   -----------
   -- Spawn --
   -----------
   -- FIXME: memory leaks
   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False) return Integer
   is
      Extra : constant String := (if Understands_Verbose then "-v " else "");
      File  : File_Descriptor;
      Name  : String_Access;
      Ok    : Boolean;
   begin
      Log ("Spawning: " & Command & " " & Arguments, Debug);

      case Simple_Logging.Level is
         when Always | Error | Warning =>
            Create_Temp_Output_File (File, Name);
            return Code : Integer do
               Spawn
                 (Locate_In_Path (Command),
                  Argument_String_To_List (Arguments).all,
                  File,
                  Code,
                  Err_To_Out => False);
               Delete_File (Name.all, Ok);
               if not Ok then
                  Log ("Failed to delete tmp file: " & Name.all, Warning);
               end if;
               Free (Name);
            end return;
         when Info | Detail =>
            return Spawn_With_Progress (Command, Arguments, Percent_Only => Simple_Logging.Level = Info);
         when Debug =>
            return
              (Spawn (Locate_In_Path (Command),
               Argument_String_To_List (Extra & Arguments).all));
      end case;
   end Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command   : String;
                    Arguments : String := "";
                    Understands_Verbose : Boolean := False)
   is
      Code : constant Integer := Spawn (Command, Arguments, Understands_Verbose);
   begin
      if Code /= 0 then
         raise Program_Error with "Exit code:" & Code'Image;
      end if;
   end Spawn;

   ------------------
   -- Enter_Folder --
   ------------------

   function Enter_Folder (Path : String) return Folder_Guard is
      Current : constant String := Ada.Directories.Current_Directory;
   begin
      return Guard : Folder_Guard (Current'Length) do
         Guard.Original := Current;
         Log ("Entering folder: " & Path, Debug);
         Ada.Directories.Set_Directory (Path);
         Guard.Initialized := True;
      end return;
   end Enter_Folder;

   ----------------------------
   -- Stay_In_Current_Folder --
   ----------------------------

   function Stay_In_Current_Folder return Folder_Guard is
   begin
      return Guard : Folder_Guard (0) do
         Log ("Staying in current folder", Debug);
         Guard.Initialized := False;
      end return;
   end Stay_In_Current_Folder;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Folder_Guard) is
   begin
      if This.Initialized then
         Log ("Going back to folder: " & This.Original, Debug);
         Ada.Directories.Set_Directory (This.Original);
         --  FIXME: what if this throws?
      end if;
   end Finalize;

end Alire.OS_Lib;
