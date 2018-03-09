package Alire.Index.Libglfw3 is

   V_3 : constant Release :=
               Register (Projects.Libglfw3,
                         V ("3"),
                         Native ((Debian | Ubuntu => Packaged_As ("libglfw3-dev"),
                                  others          => Unavailable)));

end Alire.Index.Libglfw3;
