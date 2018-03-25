package Alire.Index.NcursesAda is

   function Project is new Catalogued_Project ("ncursesada",
                                               "Ada binding to the ncurses text interface library");

   V_6 : constant Release :=
           Project.Register
             (V ("6"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada5-dev"),
                       others          => Unavailable)),

              Available_When =>
                Compiler_Is_Native
             );

   V_5 : constant Release :=
           Project.Register
             (V ("5"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada3-dev"),
                       others          => Unavailable)),

              Available_When =>
                Compiler_Is_Native
             );

end Alire.Index.NcursesAda;
