package Alire.Index.AUnit is

   Prj_Name : constant Project_Name        := "aunit";
   Prj_Desc : constant Project_Description := "Ada unit test framework";
   Prj_Repo : constant URL                 := "https://github.com/alire-project/libaunit.git";

   Prj_Maintainer : constant String := "AdaCore";
   Prj_Website    : constant URL    := "https://www.adacore.com/download/more";

   V_2017 : constant Release :=
               Register (Prj_Name,
                         V ("2017"),
                         Prj_Desc,
                         Git (Prj_Repo, "9cc524fe3c3cb3d857e6d1a38defbf58af88f7f9"),
                         Properties =>
                           GPR_File ("lib/gnat/aunit.gpr") and
                           GPR_File ("test/aunit_tests.gpr") and

                           GPR_File ("examples/calculator/harness.gpr") and
                           GPR_File ("examples/calculator/tested_lib/testlib.gpr") and
                           GPR_File ("examples/failures/harness.gpr") and
                           GPR_File ("examples/failures/tested_lib/testlib.gpr") and
                           GPR_File ("examples/liskov/harness.gpr") and
                           GPR_File ("examples/liskov/tested_lib/testlib.gpr") and
                           GPR_File ("examples/simple_test/harness.gpr") and
                           GPR_File ("examples/simple_test/tested_lib/testlib.gpr") and
                           GPR_File ("examples/test_caller/harness/harness.gpr") and
                           GPR_File ("examples/test_caller/tested_lib/testlib.gpr") and
                           GPR_File ("examples/test_fixture/harness.gpr") and
                           GPR_File ("examples/test_fixture/tested_lib/testlib.gpr") and

                           Executable ("aunit_harness") and
                           Executable ("run-ppc-elf") and
                           Executable ("test_liskov") and
                           Executable ("test_calculator") and
                           Executable ("test_math") and

                           Maintainer (Prj_Maintainer) and
                           Website    (Prj_Website) and
                           License    (GPL_3_0)
                        );

end Alire.Index.AUnit;
