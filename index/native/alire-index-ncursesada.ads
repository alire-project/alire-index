with Alire.Index.GNAT;

package Alire.Index.NcursesAda is

   function Project is
     new Catalogued_Project ("Ada binding to the ncurses text interface library");

   V_6 : constant Release :=
           Project.Register
             (V ("6"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada5-dev"),
                       others          => Unavailable)),

              Dependencies =>
                GNAT.Project.Current
             );

   V_5 : constant Release :=
           Project.Register
             (V ("5"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada3-dev"),
                       others          => Unavailable)),

              Dependencies =>
                GNAT.Project.Current
             );

end Alire.Index.NcursesAda;
