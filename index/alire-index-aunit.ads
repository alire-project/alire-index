package Alire.Index.AUnit is

   Prj_Repo       : constant URL    := "https://github.com/alire-project/libaunit.git";
   Prj_Maintainer : constant String := "AdaCore";
   Prj_Website    : constant URL    := "https://www.adacore.com/download/more";

   V_2017 : constant Release :=
               Register (Projects.AUnit,
                         V ("2017"),
                         Git (Prj_Repo, "b66a41ceb35bfc81b9345655c5f46317a57de3b4"),
                         Properties =>
                           GPR_Scenario ("RUNTIME",
                                         "full" or "zfp" or "ravenscar" or "ravenscar-cert" or "cert") and

                           Executable ("aunit_harness") and
                           Executable ("run-ppc-elf") and
                           Executable ("test_liskov") and
                           Executable ("test_calculator") and
                           Executable ("test_math") and

                           Maintainer (Prj_Maintainer) and
                           Website    (Prj_Website) and
                           License    (GPL_3_0),

                         Private_Properties =>
                           GPR_File ("aunit.gpr") and
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
                           GPR_File ("examples/test_fixture/tested_lib/testlib.gpr")
                        );

end Alire.Index.AUnit;
