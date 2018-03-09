package Alire.Index.Libhello is

   Repo : constant URL          := "https://bitbucket.org/aleteolabs/libhello.git";

   Desc : constant Description_String := "A sample dependency for the hello project";

   V_1_0 : constant Release :=
             Register (Projects.Libhello,
                       V ("1.0"),
                       Git (Repo, "ce78e7706c9d3f97605df48d8befca5407f8d328"));

end Alire.Index.Libhello;
