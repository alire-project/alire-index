with Alire.Index.Libhello;

package Alire.Index.Hello is

   Name : constant Project_Name := "hello";
   Repo : constant URL		:= "https://bitbucket.org/aleteolabs/hello.git";

   Desc : constant Project_Description := """Hello, world!"" demonstration project";

   V_1_0_0  : constant Release :=
                Register_Git (Name,
                              V ("1.0.0"),
                              Desc,
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));

   V_1_0_1  : constant Release :=
                Register_Git (Name,
                              V ("1.0.1"),
                              Desc,
                              Repo,
                              "65725c20778875eef12b61a01b437120932965f3",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));

end Alire.Index.Hello;
