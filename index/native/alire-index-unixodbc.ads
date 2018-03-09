package Alire.Index.UnixODBC is

   V_2_3 : constant Release :=
             Register (Projects.UnixODBC,
                       V ("2.3"),
                       Native ((Debian | Ubuntu => Packaged_As ("unixodbc-dev"),
                                others          => Unavailable)),
                       Properties =>
                         Website ("www.unixodbc.org"));

end Alire.Index.UnixODBC;
