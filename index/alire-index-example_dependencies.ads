with Alire.Repositories.Local;
with Alire.Requisites;
with Alire.Properties.Platform;

package Alire.Index.Example_Dependencies is

   V_1_0_0  : constant Release :=
                Register ("example_dependencies",
                          V ("1.0.0"),
                          Repositories.Local.Repo,
                          Repositories.Local.Local_Id,
                          Properties => Default_Properties and Available_On (GNU_Linux),
                          Requisites => No_Requisites and (No_Requisites or No_Requisites));

end Alire.Index.Example_Dependencies;
