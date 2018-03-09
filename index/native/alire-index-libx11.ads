package Alire.Index.LibX11 is

   V_2 : constant Release :=
           Register (Projects.LibX11,
                     V ("2"),
                     Native ((Debian | Ubuntu => Packaged_As ("libx11-dev"),
                              others          => Unavailable)));

end Alire.Index.LibX11;
