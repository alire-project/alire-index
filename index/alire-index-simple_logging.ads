package Alire.Index.Simple_Logging is

   Name : constant Project_Name := "simple_logging";
   Repo : constant URL          := "https://github.com/mosteo/simple_logging.git";

   V_1_0_0 : constant Release :=
               Register_Git (Name,
                             V ("1.0.0"),
                             Repo,
                             "77896e4a9d0539a63e6bfb657ab955656c2e3c0f");

end Alire.Index.Simple_Logging;
