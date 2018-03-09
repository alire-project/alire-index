package Alire.Index.LibGNUTLS is

   V_3_5_8 : constant Release :=
               Register (Projects.LibGNUTLS,
                         V ("3.5.8"),
                         Native ((Debian | Ubuntu => Packaged_As ("libgnutls28-dev"),
                                 others           => Unavailable)));

end Alire.Index.LibGNUTLS;
