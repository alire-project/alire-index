package Alire.Index.Libhello is

   function Project is new Catalogued_Project (Projects.Libhello);

   Repo : constant URL := "https://bitbucket.org/aleteolabs/libhello.git";

   V_1_0 : constant Release :=
             Project.Register
               (V ("1.0"),
                Git (Repo, "ce78e7706c9d3f97605df48d8befca5407f8d328"));

end Alire.Index.Libhello;
