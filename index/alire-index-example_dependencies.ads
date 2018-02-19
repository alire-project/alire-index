with Alire.Origins;

package Alire.Index.Example_Dependencies is

   V_1_0_0  : constant Release :=
                Register ("example_dependencies",
                          V ("1.0.0"),
                          "Release with assorted advanced dependency conditions",
                          Origins.New_Filesystem ("/fake"),
                          Available_When => -- Impossible mix
                            (System_Is (Windows) and System_Is (GNU_Linux)) or
                            (Compiler_Is_At_Least (GNAT_Any) and not Compiler_Is_At_Least (GNAT_Any)));

end Alire.Index.Example_Dependencies;
