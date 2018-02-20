package Alire.Index.Libhello is

   Name : constant Project_Name := "libhello";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/libhello.git";

   Desc : constant Project_Description := "A sample dependency for the hello project";

   V_1_0_0 : constant Release :=
               Register (Name,
                         V ("1.0.0"),
                         Desc,
                         Git (Repo, "ce78e7706c9d3f97605df48d8befca5407f8d328"));

end Alire.Index.Libhello;
