with Alire.Repositories.Local;

package Alire.Index.Example_Dependencies is

   V_1_0_0  : constant Release :=
                Register ("example_dependencies",
                          V ("1.0.0"),
                          "Release with assorted advanced dependency conditions",
                          Repositories.Local.Repo,
                          Repositories.Local.Local_Id,
                          Available_When => -- Note that it's impossible
                            (System_Is (Windows) and System_Is (GNU_Linux)) and
                            Compiler_Is_At_Least (GNAT_Any));

end Alire.Index.Example_Dependencies;
