package Alire.Index.GtkAda is

   V_16_1 : constant Release :=
              Register (Projects.GtkAda,
                        V ("16.1"),
                        Native ((Debian | Ubuntu => Packaged_As ("libgtkada16.1.0-dev"),
                                 others          => Unavailable)),

                        Available_When => Compiler_Is_Native
                        -- Unfortunately packaged libs can't be used with non-platform compilers due to .ali clashes
                       );

end Alire.Index.GtkAda;
