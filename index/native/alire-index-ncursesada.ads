with Alire.Index.GNAT;

package Alire.Index.NcursesAda is

   function Project is new Catalogued_Project (Projects.NcursesAda);

   V_5 : constant Release :=
           Project.Register
             (V ("5.0.0+6.0.20170708-2"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada5-dev"),
                       others          => Unavailable)),

              Available_When =>
                Compiler_Is_Native
             );

   V_3 : constant Release :=
           Project.Register
             (V ("3.0.0+5.9.20140726-1build1"),
              Native ((Debian | Ubuntu => Packaged_As ("libncursesada3-dev"),
                       others          => Unavailable)),

              Available_When =>
                Compiler_Is_Native
             );

end Alire.Index.NcursesAda;
