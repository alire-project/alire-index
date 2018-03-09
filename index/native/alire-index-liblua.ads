package Alire.Index.Liblua is

   V_5_3 : constant Release :=
             Register (Projects.Liblua,
                       V ("5.3"),
                       Native ((Debian | Ubuntu => Packaged_As ("liblua5.3-dev"),
                                others           => Unavailable)));

end Alire.Index.Liblua;
