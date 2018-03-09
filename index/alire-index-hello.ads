with Alire.Index.Libhello;

package Alire.Index.Hello is

   Repo : constant URL		:= "https://bitbucket.org/aleteolabs/hello.git";

   V_1_0_0  : constant Release :=
                Register (Projects.Hello,
                          V ("1.0.0"),
                          Git (Repo, "8cac0afddc505794ae3e5634745ce0830129d241"),
                          Dependencies => Within_Major (Libhello.V_1_0));

   V_1_0_1  : constant Release :=
                Register (Projects.Hello,
                          V ("1.0.1"),
                          Git (Repo, "65725c20778875eef12b61a01b437120932965f3"),
                          Dependencies => Within_Major (Libhello.V_1_0));

end Alire.Index.Hello;
