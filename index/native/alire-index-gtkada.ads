with Alire.Index.GNAT;

package Alire.Index.GtkAda is

   function Project is new Catalogued_Project (Projects.GtkAda);

   V_16_1 : constant Release :=
              Project.Register
                (V ("16.1"),
                 Native ((Debian | Ubuntu => Packaged_As ("libgtkada16.1.0-dev"),
                          others          => Unavailable)),

                 Dependencies =>
                   GNAT.Project >= GNAT.V_7
                );

end Alire.Index.GtkAda;
