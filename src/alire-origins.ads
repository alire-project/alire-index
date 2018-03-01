private with Ada.Strings.Unbounded;

package Alire.Origins with Preelaborate is

   --  Minimal information about origins of sources.
   --  We use the term origins to avoid mixing 'alire sources' with 'project sources' or other 'sources'.

   --  The actual capabilities for check-outs or fetches are in alr proper

   type Kinds is (Filesystem, -- Not really an origin, but a working copy of a project
                  Git,        -- Remote git repo
                  Hg,         -- Remote hg repo
                  Native      -- Native platform package
                 );

   type Origin is tagged private;

   function Kind (This : Origin) return Kinds;

   function URL (This : Origin) return Alire.URL;

   function Id (This : Origin) return String;

   function Is_Native (This : Origin) return Boolean is (This.Kind = Native);

   --  Helper types

   subtype Git_Commit is String (1 .. 40);
   subtype Hg_Commit  is String (1 .. 40);

   --  Constructors

   function New_Filesystem (Path : String) return Origin;

   function New_Git (URL  : Alire.URL;
                     Id   : Git_Commit)
                     return Origin;

   function New_Hg (URL  : Alire.URL;
                    Id   : Hg_Commit)
                    return Origin;

   function New_Native (Package_Name : String) return Origin;

   function Native_Package (This : Origin) return String
     with Pre => This.Kind = Native;

   function Image (This : Origin) return String;

private

   use Ada.Strings.Unbounded;

   type Origin is tagged record
      Kind : Kinds;
      URL  : Unbounded_String;
      Id   : Unbounded_String;
   end record;

   function New_Filesystem (Path : String) return Origin is
     (Filesystem,
      Id  => Null_Unbounded_String,
      URL => To_Unbounded_String (Path));

   function New_Git (URL  : Alire.URL;
                     Id   : Git_Commit)
                     return Origin is
     (Git,
      URL => To_Unbounded_String (URL),
      Id  => To_Unbounded_String (Id));

   function New_Hg (URL  : Alire.URL;
                    Id   : Hg_Commit)
                    return Origin is
     (Hg,
      URL => To_Unbounded_String (URL),
      Id  => To_Unbounded_String (Id));

   function New_Native (Package_Name : String) return Origin is
     (Native,
      Id  => To_Unbounded_String (Package_Name),
      URL => Null_Unbounded_String);

   function Kind (This : Origin) return Kinds is (This.Kind);

   function URL (This : Origin) return Alire.URL is (Alire.URL (To_String (This.URL)));

   function Id (This : Origin) return String is (To_String (This.Id));

   function Native_Package (This : Origin) return String renames Id;

   function S (Str : Unbounded_String) return String is (To_String (Str));

   function Image (This : Origin) return String is
     (case This.Kind is
         when Git | Hg   => "commit " & S (This.Id) & " from " & S (This.URL),
         when Native     => "package " & S (This.Id) & " from platform software manager",
         when Filesystem => "path " & S (This.Id));


end Alire.Origins;
