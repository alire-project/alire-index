with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Expect;
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

   -------------------------
   -- Spawn_With_Progress --
   -------------------------

   function Spawn_With_Progress (Command      : String;
                                 Arguments    : String) return Integer
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use GNAT.Expect;

      Simple_Command : constant String := Ada.Directories.Simple_Name (Command);

      --------------
      -- Sanitize --
      --------------

      function Sanitize (S : String) return String is -- Remove CR y LFs
      begin
         return Result : String := S do
            for I in Result'Range loop
               if Result (I) = Ada.Characters.Latin_1.CR then
                  Result (I) := ' ';
               elsif Result (I) = Ada.Characters.Latin_1.LF then
                  Result (I) := ' ';
               end if;
            end loop;
         end return;
      end Sanitize;

      Indicator : constant String := "/-\|/-\|";
      type Indicator_Mod is mod Indicator'Length;
      Pos : Indicator_Mod := 0;

      Pid : Process_Descriptor;

      Match     : Expect_Match;
      Last_Line : Unbounded_String;
      Max_Len   : Natural := 0;
   begin
      Non_Blocking_Spawn
        (Pid,
         Command,
         Argument_String_To_List (Arguments).all,
         Err_To_Out => True);

      loop
         begin
            Expect (Pid, Match,
                    "([ \t\S]+)[ \n\r\f\v]", -- works for \n and \r in output (git vs gprbuild)
                    Timeout => 200);

            if Match >= 0 then
               Last_Line := To_Unbounded_String (Sanitize (Expect_Out_Match (Pid)));
            end if;

            declare
               Progress : constant String :=
                            Ada.Characters.Latin_1.CR &
                            Simple_Command & ": " &
                            Indicator (Integer (Pos) + 1) & " " &
                            To_String (Last_Line);
            begin
               Max_Len := Natural'Max (Max_Len, Progress'Length);
               Put (Progress &
                      String'(1 .. Max_Len - Progress'Length => ' ')); -- Wipe remainder of old lines
               Pos := Pos + 1;
            end;

         exception
            when Process_Died =>
               Log ("Spawned process died", Debug);
               exit;
         end;
      end loop;

      return Code : Integer do
         Close (Pid, Code);

         declare
            Line : constant String :=
                     Ada.Characters.Latin_1.CR & Simple_Command &
                     (if Code = 0
                      then " completed"
                      else " ended with error (exit code" & Code'Img & ")");
         begin
            Max_Len := Natural'Max (Max_Len, Simple_Command'length + 2); -- If there weren't any output
            Put_Line (Line & String'(1 .. Max_Len - Line'Length => ' '));
         end;
      end return;
   end Spawn_With_Progress;

   -----------
   -- Spawn --
   -----------
   -- FIXME: memory leaks
   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer
   is
      Extra : constant String := (if Understands_Verbose then "-v " else "");
      File  : File_Descriptor;
      Name  : String_Access;
      Ok    : Boolean;
   begin
      if Simple_Logging.Level = Debug then
         Log ("Spawning: " & Command & " " & Extra & Arguments, Debug);
      else
         Log ("Spawning: " & Command & " " & Arguments, Debug);
      end if;

      if (Force_Quiet and then Log_Level /= Debug) or else Log_Level in Always | Error | Warning then
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
      elsif Log_Level = Info then
         return Spawn_With_Progress (Command, Arguments);
      elsif Log_Level = Detail then -- All lines, without -v
         return
           (Spawn (Locate_In_Path (Command),
            Argument_String_To_List (Arguments).all));
      else  -- Debug: all lines plus -v in commands
         return
           (Spawn (Locate_In_Path (Command),
            Argument_String_To_List (Extra & Arguments).all));
      end if;
   end Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command   : String;
                    Arguments : String := "";
                    Understands_Verbose : Boolean := False;
                    Force_Quiet         : Boolean := False)
   is
      Code : constant Integer := Spawn (Command, Arguments, Understands_Verbose, Force_Quiet);
   begin
      if Code /= 0 then
         raise Program_Error with "Exit code:" & Code'Img;
      end if;
   end Spawn;

   ------------------------
   -- Spawn_And_Redirect --
   ------------------------

   procedure Spawn_And_Redirect (Out_File   : String;
                                 Command    : String;
                                 Arguments  : String := "";
                                 Err_To_Out : Boolean := False)
   is
      File : constant File_Descriptor := Create_File (Out_File, Text);
      Code : Integer;
   begin
      Spawn (Locate_In_Path (Command),
             Argument_String_To_List (Arguments).all,
             File, Code, Err_To_Out);
      Close (File);

      if Code /= 0 then
         raise Program_Error with "Exit code:" & Code'Img;
      end if;
   end Spawn_And_Redirect;

   ------------------
   -- Spawn_Bypass --
   ------------------

   procedure Spawn_Bypass (Command   : String;
                           Arguments : String := "")
   is
      Code : constant Integer := Spawn (Locate_In_Path (Command),
                                        Argument_String_To_List (Arguments).all);
   begin
      if Code /= 0 then
         raise Program_Error with "Exit code:" & Code'Image;
      end if;
   end Spawn_Bypass;

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
