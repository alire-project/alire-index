with Alire.Utils;

package Alire.Projects with Preelaborate is

   -----------
   -- Names --
   -----------

   --  Full list in alphabetical order
   type Names is
     (Ada_Lua,
      Adacurses,
      Alire,
      Alr,
      APQ,
      AUnit,

      DAK_Components,
      DAK_Components_Connections,
      DAK_Components_Connections_Secure,
      DAK_Components_NTP,
      DAK_Components_ODBC,
      DAK_Components_SQLite,
      DAK_Strings_Edit,
      DAK_Tables,

      Eagle_Lander,

      -------
      -- G --
      -------

      Globe_3D,
      GLUT,
      GtkAda,

      -------
      -- H --
      -------

      Hangman,
      Hello,

      -------
      -- L --
      -------

      Libadacrypt,
      Libglfw3,
      LibGNUTLS,
      Libhello,
      Liblua,
      LibSDL2,
      LibSDL2_Image,
      LibSDL2_TTF,
      LibX11,

      -------
      -- M --
      -------

      Mathpaqs,

      NcursesAda,

      OpenGLAda,

      PragmARC,

      RxAda,

      SDLAda,
      Semantic_Versioning,
      Simple_Logging,
      Steamsky,

      UnixODBC,

      Whitakers_Words);

   -----------
   -- Image --
   -----------

   function Image (Name : Names) return String;
   --  Lower case

   -----------------
   -- Description --
   -----------------

   function Description (Name : Names) return Description_String is
     (case Name is
         when Ada_Lua =>
            "An Ada binding for Lua",
         when Adacurses =>
            "Wrapper on different packagings of NcursesAda",
         when Alire =>
            "Alire project catalog and support files",
         when Alr =>
            "Command-line tool from the Alire project",
         when APQ =>
            "APQ Ada95 Database Library (core)",
         when AUnit
           => "Ada unit test framework",

         when DAK_Components =>
            "Simple Components (base)",
         when DAK_Components_Connections =>
            "Simple Components (clients/servers)",
         when DAK_Components_Connections_Secure =>
            "Simple Components (clients/servers over TLS)",
         when DAK_Components_NTP =>
            "Simple Components (ntp)",
         when DAK_Components_ODBC =>
            "Simple Components (ODBC bindings)",
         when DAK_Components_SQLite =>
            "Simple Components (SQLite)",
         when DAK_Strings_Edit =>
            "Simple Components (strings)",
         when DAK_Tables =>
            "Simple Components (tables)",

         when Eagle_Lander =>
            "Apollo 11 lunar lander simulator (Ada/Gtk/Cairo)",

      -------
      -- G --
      -------

         when Globe_3D =>
            "GL Object Based Engine for 3D in Ada",
         when GLUT =>
            "OpenGL Utility Toolkit",
         when GtkAda =>
            "Ada binding for the GTK+ GUI",

      -------
      -- H --
      -------

         when Hangman =>
            "Hangman game for the console",
         when Hello =>
            """Hello, world!"" demonstration project",

      -------
      -- L --
      -------

         when Libadacrypt =>
            "A crypto library for Ada with a nice API",
         when Libglfw3 =>
            "Portable library for OpenGL, window and input",
         when LibGNUTLS =>
            "GNU TLS library",
         when Libhello =>
            """Hello, world!"" demonstration project support library",
         when Liblua =>
            "Development files for the Lua language",
         when LibSDL2 =>
            "Simple DirectMedia Layer development files",
         when LibSDL2_Image =>
            "Image loading library for Simple DirectMedia Layer 2",
         when LibSDL2_TTF =>
            "TrueType Font library for Simple DirectMedia Layer 2",
         when LibX11 =>
            "X11 client-side library",

      -------
      -- M --
      -------

         when Mathpaqs =>
            "A collection of mathematical, 100% portable, packages",

         when NcursesAda =>
            "Ada binding to the ncurses text interface library",

         when OpenGLAda =>
            "Thick Ada binding for OpenGL and GLFW",

         when PragmARC =>
            "PragmAda Reusable Components (PragmARCs)",

         when RxAda =>
            "RxAda port of the Rx framework",

         when SDLAda =>
            "Ada 2012 bindings to SDL 2",
         when Semantic_Versioning =>
            "Semantic Versioning in Ada",
         when Simple_Logging =>
            "Simple logging to console",
         when Steamsky =>
            "Roguelike in sky with steampunk theme",

         when UnixODBC =>
            "Open Database Connectivity drivers for UNIX platforms",

         when Whitakers_Words =>
            "William Whitaker's WORDS, a Latin dictionary");

private

   function Image (Name : Names) return String is (Utils.To_Lower_Case (Name'Img));

end Alire.Projects;
