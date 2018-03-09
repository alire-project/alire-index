package Alire.Index.NcursesAda is

   V_5 : constant Release :=
         Register (Projects.NcursesAda,
                   V ("5.0.0+6.0.20170708-2"),
                   Native ((Debian | Ubuntu => Packaged_As ("libncursesada5-dev"),
                            others          => Unavailable)),

                   Available_When => Compiler_Is_Native
                   -- Unfortunately packaged libs can't be used with non-platform compilers due to .ali clashes
                  );

   V_3 : constant Release :=
           Register (Projects.NcursesAda,
                     V ("3.0.0+5.9.20140726-1build1"),
                     Native ((Debian | Ubuntu => Packaged_As ("libncursesada3-dev"),
                              others          => Unavailable)),

                     Available_When => Compiler_Is_Native);

end Alire.Index.NcursesAda;
