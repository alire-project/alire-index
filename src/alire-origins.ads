with Alire.Interfaces;
with Alire.Platforms;
with Alire.Utils;

private with Ada.Strings.Unbounded;

package Alire.Origins with Preelaborate is

   --  Minimal information about origins of sources.
   --  We use the term origins to avoid mixing 'alire sources' with 'project sources' or other 'sources'.

   --  The actual capabilities for check-outs or fetches are in alr proper

   --------------------------------------------
   --  supporting types for native packages  --
   --------------------------------------------
   --  These are used to represent native packages in a comfortable way in the index

   type Package_Names is tagged private;

   function Image (This : Package_Names) return String;

   function Unavailable return Package_Names;

   function Packaged_As (Name : String) return Package_Names;

   type Native_Packages is array (Platforms.Distributions) of Package_Names;
   --  The name of a package in every distro for a given version

   type Kinds is (Filesystem, -- Not really an origin, but a working copy of a project
                  Git,        -- Remote git repo
                  Hg,         -- Remote hg repo
                  SVN,        -- Remote svn repo
                  Native      -- Native platform package
                 );

   type Origin is new Interfaces.Codifiable with private;

   function Kind (This : Origin) return Kinds;

   -------------------
   --  member data  --
   -------------------

   function Commit (This : Origin) return String with Pre => This.Kind in Git | Hg | SVN;
   function URL (This : Origin) return Alire.URL with Pre => This.Kind in Git | Hg | SVN;

   function Path (This : Origin) return String with Pre => This.Kind = Filesystem;

   function Is_Native (This : Origin) return Boolean is (This.Kind = Native);
   function Package_Name (This         : Origin;
                          Distribution : Platforms.Distributions)
                          return String;
   function All_Native_Names (This : Origin) return Native_Packages;

   --  Helper types

   subtype Git_Commit is String (1 .. 40);
   subtype Hg_Commit  is String (1 .. 40);

   --  Constructors

   function New_Filesystem (Path : String) return Origin;

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit)
                     return Origin;

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit)
                    return Origin;

   function New_SVN (URL : Alire.URL; Commit : String) return Origin;

   function New_Native (Packages : Native_Packages) return Origin;

   function Image (This : Origin) return String;

   overriding function To_Code (This : Origin) return Utils.String_Vector;

private

   use Ada.Strings.Unbounded;
   function "+" (S : String) return Unbounded_String renames To_Unbounded_String;
   function "+" (U : Unbounded_String) return String renames To_String;

   type Package_Names is tagged record
      Name : Unbounded_String;
   end record;

   function Image (This : Package_Names) return String is (+This.Name);

   function Unavailable return Package_Names is (Name => Null_Unbounded_String);
   function Packaged_As (Name : String) return Package_Names is (Name => +Name);

   type Origin is new Interfaces.Codifiable with record
      Kind   : Kinds;

      Commit : Unbounded_String;
      URL    : Unbounded_String;

      Packages : Native_Packages;

      Path : Unbounded_String;
   end record;

   function New_Filesystem (Path : String) return Origin is
     (Filesystem,
      Path => +Path,
      others => <>);

   function New_Git (URL    : Alire.URL;
                     Commit : Git_Commit)
                     return Origin is
     (Git,
      URL => +URL,
      Commit => +Commit,
      others => <>);

   function New_Hg (URL    : Alire.URL;
                    Commit : Hg_Commit)
                    return Origin is
     (Hg,
      URL    => +URL,
      Commit => +Commit,
      others => <>);

   function New_SVN (URL : Alire.URL; Commit : String) return Origin is
     (SVN,
      URL    => +URL,
      Commit => +Commit,
      others => <>);

   function New_Native (Packages : Native_Packages) return Origin is
     (Native,
      Packages => Packages,
      others => <>);

   function Kind (This : Origin) return Kinds is (This.Kind);

   function URL    (This : Origin) return Alire.URL is (Alire.URL (+This.URL));
   function Commit (This : Origin) return String is (+This.Commit);

   function Path (This : Origin) return String is (+This.Path);

   function Package_Name (This         : Origin;
                          Distribution : Platforms.Distributions)
                          return String is (+This.Packages (Distribution).Name);

   function All_Native_Names (This : Origin) return Native_Packages is (This.Packages);

   function S (Str : Unbounded_String) return String is (To_String (Str));

   function Image (This : Origin) return String is
     (case This.Kind is
         when Git | Hg | SVN => "commit " & S (This.Commit) & " from " & S (This.URL),
         when Native         => "native package from platform software manager",
         when Filesystem     => "path " & S (This.Path));

   overriding function To_Code (This : Origin) return Utils.String_Vector is
     (if This.Kind = Filesystem
      then Utils.To_Vector (Path (This))
      else raise Program_Error with "Unimplemented");


end Alire.Origins;
