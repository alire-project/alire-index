package Alire.Index.Simple_Logging is

   Name : constant Project_Name := "simple_logging";
   Repo : constant URL          := "https://github.com/mosteo/simple_logging.git";

   Desc : constant Project_Description := "Simple logging to console";

   V_1 : constant Release :=
           Register_Git (Name,
                         V ("1.0.1"),
                         Desc,
                         Repo,
                         "0d5ab8764f667107892a0c832a7a70a2c6b7efe2");

end Alire.Index.Simple_Logging;
