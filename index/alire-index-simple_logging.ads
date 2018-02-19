package Alire.Index.Simple_Logging is

   Name : constant Project_Name := "simple_logging";
   Repo : constant URL          := "https://github.com/mosteo/simple_logging.git";

   Desc : constant Project_Description := "Simple logging to console";

   V_1_0_0 : constant Release :=
               Register_Git (Name,
                             V ("1.0.0"),
                             Desc,
                             Repo,
                             "81a00b835cc84a74e7008015623bce018b2fa72a");

end Alire.Index.Simple_Logging;
