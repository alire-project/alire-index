package Alire.Index.NcursesAda is

   Name : constant String := "ncursesada";
   Desc : constant String := "Ada binding to the ncurses text interface library";

   V_5 : constant Release :=
         Register (Name,
                   V ("5.0.0+6.0.20170708-2"),
                   Desc,
                   Native ((Debian | Ubuntu => Packaged_As ("libncursesada5-dev"),
                            others          => Unavailable)),

                   Available_When => Compiler_Is_Native
                   -- Unfortunately packaged libs can't be used with non-platform compilers due to .ali clashes
                  );

   V_3 : constant Release :=
           Register (Name,
                     V ("3.0.0+5.9.20140726-1build1"),
                     Desc,
                     Native ((Debian | Ubuntu => Packaged_As ("libncursesada3-dev"),
                              others          => Unavailable)),

                     Available_When => Compiler_Is_Native);

end Alire.Index.NcursesAda;
