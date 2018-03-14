package Alire.Index.GNAT is

   function Project is new Catalogued_Project (Projects.GNAT);

   --  If minor versions proved important they could be segregated with platform-specific knowledge

   V_8 : constant Release :=
           Project.Register
             (V ("8"),
              Native ((Debian | Ubuntu => Packaged_As ("gnat-8"),
                       others          => Unavailable)));

   V_7 : constant Release :=
           Project.Register
             (V ("7"),
              Native ((Debian | Ubuntu => Packaged_As ("gnat-7"),
                       others          => Unavailable)));

end Alire.Index.GNAT;
