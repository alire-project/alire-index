with Simple_Logging;

package Alire with Preelaborate is

   Query_Unsuccessful : exception;
   --  Raised by subprograms that return releases/dependencies when not found/impossible

   subtype URL is String;

   Max_Name_Length        : constant := 72; -- Github maximum is 100 and bitbucket 128, but since Description is 72...
   Max_Description_Length : constant := 72; -- Git line recommendation (although it's 50 for subject line)

   Extension_Separator    : constant Character := '.';
   --  Refers to extension releases! Nothing to do with files

   --  Strings that are used quite generally

   type Project is new String with Dynamic_Predicate =>
     Project'Length >= 3 and then
     Project (Project'First) /= '_' and then
     Project (Project'First) /= ':' and then
     Project (Project'Last) /= ':' and then
     (for all C of Project => C in 'a' .. 'z' | '0' .. '9' | '_' | '.' );

   function "+" (P : Project) return String  is (String (P));
   function "+" (P : String)  return Project is (Project (P));

   subtype Description_String is String with Dynamic_Predicate =>
     Description_String'Length <= Max_Description_Length;

   subtype Folder_String is String with Dynamic_Predicate =>
     Folder_String'Length > 0 and then
     (for all C of Folder_String => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | Extension_Separator);
   --  Used for cross-platform folder names

   subtype Platform_Independent_Path is String with Dynamic_Predicate =>
     (for all C of Platform_Independent_Path => C /= '\');
   --  This type is used to ensure that folder separators are externally always '/',
   --  and internally properly converted to the platform one

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Log_Level : Simple_Logging.Levels renames Simple_Logging.Level;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info) renames Simple_Logging.Log;

end Alire;
