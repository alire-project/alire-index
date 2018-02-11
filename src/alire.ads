private with Ada.Containers.Indefinite_Holders;

with Semantic_Versioning;

with Simple_Logging;

package Alire with Preelaborate is

   File_Error : exception;


   type URL is new String;


   subtype Project_Name is String;
   --  FIXME: add predicate on valid characters (must be a valid gnat filename part)


   type Dependency (<>) is tagged private;

   function Project (Dep : Dependency) return Project_Name;

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set;



   type Milestone (<>) is tagged private;

   function "<" (L, R : Milestone) return Boolean;

   function New_Milestone (Name    : Project_Name;
                           Version : Semantic_Versioning.Version) return Milestone;

   function Project (M : Milestone) return Project_Name;

   function Version (M : Milestone) return Semantic_Versioning.Version;

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Log_Level : Simple_Logging.Levels renames Simple_Logging.Level;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info) renames Simple_Logging.Log;

private

   use all type Semantic_Versioning.Version;

   package Version_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Versioning.Version_Set, Semantic_Versioning."=");

   type Version_Set_Holder is new Version_Holders.Holder with null record;

   type Dependency (Name_Len : Positive) is tagged record
      Project    : Project_Name (1 .. Name_Len);
      Versions_H : Version_Set_holder;
   end record;

   function Project (Dep : Dependency) return Project_Name is (Dep.Project);

   function Versions (Dep : Dependency) return Semantic_Versioning.Version_Set is
     (Dep.Versions_H.Element);


   type Milestone (Name_Len : Positive) is tagged record
      Name    : Project_Name (1 .. Name_Len);
      Version : Semantic_Versioning.Version;
   end record;

   function "<" (L, R : Milestone) return Boolean is
     (L.Name < R.Name or else (L.Name = R.Name and then L.Version < R.Version));

   function New_Milestone (Name    : Project_Name;
                           Version : Semantic_Versioning.Version) return Milestone is
     (Name'Length, Name, Version);

   function Project (M : Milestone) return Project_Name is (M.Name);

   function Version (M : Milestone) return Semantic_Versioning.Version is (M.Version);

end Alire;
