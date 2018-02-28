package Alire.Index.LibGNUTLS is

   Name : constant String := "libgnutls";
   Desc : constant String := "GNU TLS library";

   V_3_5_8 : constant Release :=
               Register (Name,
                         V ("3.5.8"),
                         Desc,
                         Apt ("libgnutls28-dev"),
                         Available_When =>
                           Distribution_Is (Debian_Buster) or
                           Distribution_Is (Ubuntu_Artful));

end Alire.Index.LibGNUTLS;
