package Alire.Index.Libadacrypt is

   Prj_Name : constant Project_Name        := "libadacrypt";
   Prj_Desc : constant Project_Description := "A crypto library for Ada with a nice API";
   Prj_Repo : constant URL                 := "https://github.com/cforler/Ada-Crypto-Library.git";

   Prj_Author     : constant String := "Christian Forler";

   V_0_8_7 : constant Release :=
               Register (Prj_Name,
                         V ("0.8.7"),
                         Prj_Desc,
                         Git (Prj_Repo, "06b4907c6ae6dcdce62339ff8fd2913d7bbbc0d9"),
                         Properties =>
                           Author     (Prj_Author) and

                           License    (GMGPL_2_0) and
                           License    (GMGPL_3_0)
                        );

end Alire.Index.Libadacrypt;
