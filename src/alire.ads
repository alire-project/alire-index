with Simple_Logging;

package Alire with Preelaborate is

   Query_Unsuccessful : exception;
   --  Raised by subprograms that return releases/dependencies when not found/impossible

   subtype URL is String;

   Max_Name_Length        : constant := 72; -- Github maximum is 100 and bitbucket 128, but since Description is 72...
   Max_Description_Length : constant := 72; -- Git line recommendation (although it's 50 for subject line)

   --  Strings that are used quite generally

   subtype Name_String is String with Dynamic_Predicate =>
     Name_String'Length >= 3 and then
     Name_String'Length <= Max_Name_Length and then
     Name_String (Name_String'First) /= '_' and then
     (for all C of Name_String => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_');

   subtype Designation_String is String with Dynamic_Predicate =>
     Designation_String'Length >= 7 and then
     Designation_String'Length <= Max_Name_Length * 2 + 1 and then
     Designation_String (Designation_String'First) /= '_' and then
     Designation_String (Designation_String'First) /= ':' and then
     Designation_String (Designation_String'Last) /= ':' and then
     (for all C of Designation_String => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ':' );

   subtype Description_String is String with Dynamic_Predicate =>
     Description_String'Length <= Max_Description_Length;

   subtype Folder_String is String with Dynamic_Predicate =>
     Folder_String'Length > 0 and then
     (for all C of Folder_String => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.');
   --  Used for cross-platform folder names

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Log_Level : Simple_Logging.Levels renames Simple_Logging.Level;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info) renames Simple_Logging.Log;

end Alire;
