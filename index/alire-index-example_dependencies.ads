with Alire.Repositories.Local;

package Alire.Index.Example_Dependencies is

   V_1_0_0  : constant Release :=
                Register ("example_dependencies",
                          V ("1.0.0"),
                          Repositories.Local.Repo,
                          Repositories.Local.Local_Id,
                          Requisites => Available_On (GNU_Linux) and
                            (Available_On (GNU_Linux) or not Available_On (GNU_Linux))
                          --  Compiles_With (GNAT_GPL_2017)));
                          );

end Alire.Index.Example_Dependencies;
