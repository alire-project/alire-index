package Alire.Index.GNATCOLL is

   function Project is new Catalogued_Project
     ("GNAT Components Collection - Core packages");

   Repo_AdaCore : constant URL := "https://github.com/AdaCore/gnatcoll-core.git";
   --  Upstream

   Repo_Alire   : constant URL := "https://github.com/alire-project/gnatcoll-core.git";
   --  For slim picks

   Base         : constant Release :=
                    Project.Unreleased
                      (Properties         =>
                                          Author     ("AdaCore") and
                         Maintainer ("alejandro@mosteo.com") and
                         License    (GPL_3_0) and

                         Project_File ("gnatcoll.gpr") and
                         GPR_Scenario ("GNATCOLL_ATOMICS",
                           "intrinsic" or "mutex") and
                         GPR_Scenario ("GNATCOLL_OS",
                           "windows" or "unix" or "osx") and
                         GPR_Scenario ("BUILD",
                           "DEBUG" or "PROD") and
                         GPR_Scenario ("LIBRARY_TYPE",
                           "relocatable" or "static" or "static-pic"),

                       Private_Properties =>
                         GPR_External ("BUILD", "PROD") and
                         GPR_External ("LIBRARY_TYPE", "static-pic") and
                         On_Condition
                           (Operating_System = GNU_Linux,
                            GPR_External ("GNATCOLL_OS", "unix")) and
                         On_Condition
                           (Operating_System = Windows,
                            GPR_External ("GNATCOLL_OS", "windows")) and
                         On_Condition
                           (Operating_System = OSX,
                            GPR_External ("GNATCOLL_OS", "osx")));

   package Slim is

      function Project is new Catalogued_Project
        ("GNAT Components Collection - Slim version (no dependencies)");

      package V_20180425 is new Released
        (Base
         .Replacing (Git (Repo_Alire,
                          "81bc37d7548fe40024eb0f647df65ec42f65443b")));

   end Slim;

   package Strings is

      function Project is new Catalogued_Project
        ("GNAT Components Collection - Strings-only");

      package V_20180425 is new Released
        (Base
         .Replacing (Git (Repo_Alire,
                          "7823e31add7133b9fbc6e037d9986a823e840dc0")));

   end Strings;

end Alire.Index.GNATCOLL;
