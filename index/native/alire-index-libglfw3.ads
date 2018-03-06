package Alire.Index.Libglfw3 is

   Name : constant String := "libglfw3";
   Desc : constant String := "portable library for OpenGL, window and input";

   V_3 : constant Release :=
               Register (Name,
                         V ("3"),
                         Desc,
                         Native ((Debian | Ubuntu => Packaged_As ("libglfw3-dev"),
                                  others          => Unavailable)));
   --  Note: the minor version will change with versions of distributions.
   --  If this proved to be a problem several releases should be isolated using the Version property

end Alire.Index.Libglfw3;
