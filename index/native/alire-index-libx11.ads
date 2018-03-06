package Alire.Index.LibX11 is

   Name : constant String := "libx11";
   Desc : constant String := "X11 client-side library";

   V_2 : constant Release :=
           Register (Name,
                     V ("2"),
                     Desc,
                     Native ((Debian | Ubuntu => Packaged_As ("libx11-dev"),
                              others          => Unavailable)));
   --  Note: the minor version will change with versions of distributions.
   --  If this proved to be a problem several releases should be isolated using the Version property

end Alire.Index.LibX11;
