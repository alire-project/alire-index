package Alire.Index.GtkAda is

   function Project is new Catalogued_Project (Projects.GtkAda);

   V_16_1 : constant Release :=
              Project.Register
                (V ("16.1"),
                 Native ((Debian | Ubuntu => Packaged_As ("libgtkada16.1.0-dev"),
                          others          => Unavailable)),

                 Available_When => Compiler_Is_Native
                 -- Unfortunately packaged libs can't be used with non-platform compilers due to .ali clashes
                );

end Alire.Index.GtkAda;
