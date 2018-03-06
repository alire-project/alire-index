with Alire.Index.AUnit;

package Alire.Index.Libadacrypt is

   Prj_Name : constant Project_Name        := "libadacrypt";
   Prj_Desc : constant Project_Description := "A crypto library for Ada with a nice API";
   Prj_Repo : constant URL                 := "https://github.com/alire-project/Ada-Crypto-Library.git";

   Prj_Author     : constant String := "Christian Forler";

   V_0_8_7 : constant Release :=
               Register (Prj_Name,
                         V ("0.8.7"),
                         Prj_Desc,
                         Git (Prj_Repo, "33d15283abbc6d8a51d717de2bd822e026710c0d"),

                         Dependencies =>
                           Within_Major (AUnit.V_2017),

                         Properties   =>
                           GPR_File ("libadacrypt.gpr") and
                           GPR_File ("acltest.gpr") and

                           GPR_Scenario ("system", "unix" or "windows") and
                           GPR_Scenario ("mode", "debug" or "release") and

                           Executable ("test-asymmetric_ciphers") and
                           Executable ("test-big_number") and
                           Executable ("test-kdf") and
                           Executable ("test-symmetric_ciphers") and
                           Executable ("test-tests") and

                           Author     (Prj_Author) and
                           License    (GMGPL_2_0) and
                           License    (GMGPL_3_0),

                         Alr_Properties =>
                           On_Condition
                             (System_Is (GNU_Linux),
                              GPR_Config ("-Xsystem=unix")) and
                           On_Condition
                             (System_Is (Windows),
                              GPR_Config ("-Xsystem=windows"))
                        );

end Alire.Index.Libadacrypt;
