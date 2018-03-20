with Alire.Index.GNAT;

package Alire.Index.GtkAda is

   function Project is new Catalogued_Project (Projects.GtkAda);

   V_17 : constant Release :=
              Project.Register
                (V ("17"),
                 Native ((Debian | Ubuntu => Packaged_As ("libgtkada16.1.0-dev"),
                          others          => Unavailable)),

                 Available_When =>
                   Compiler_Is_Native
                );

end Alire.Index.GtkAda;
