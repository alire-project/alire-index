package Alire.Index.Liblua is

   Name : constant String := "liblua";
   Desc : constant String := "Development files for the Lua language";

   V_5_3 : constant Release :=
               Register (Name,
                         V ("5.3"),
                         Desc,
                         Native ((Debian | Ubuntu => Packaged_As ("liblua5.3-dev"),
                                 others           => Unavailable)));

end Alire.Index.Liblua;
