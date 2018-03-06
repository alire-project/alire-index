package Alire.Index.UnixODBC is

   Name : constant String := "unixodbc";
   Desc : constant String := "Open Database Connectivity drivers";

   V_2_3 : constant Release :=
             Register (Name,
                       V ("2.3"),
                       Desc,
                       Native ((Debian | Ubuntu => Packaged_As ("unixodbc-dev"),
                                others          => Unavailable)),
                       Properties =>
                         Website ("www.unixodbc.org"));

end Alire.Index.UnixODBC;
