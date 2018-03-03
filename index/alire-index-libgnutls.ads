package Alire.Index.LibGNUTLS is

   Name : constant String := "libgnutls";
   Desc : constant String := "GNU TLS library";

   V_3_5_8 : constant Release :=
               Register (Name,
                         V ("3.5.8"),
                         Desc,
                         Native ((Debian | Ubuntu => Packaged_As ("libgnutls28-dev"),
                                 others           => Unavailable)));

end Alire.Index.LibGNUTLS;
