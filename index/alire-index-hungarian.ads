package Alire.Index.Hungarian is

   function Project is new Catalogued_Project
     ("Ada wrapper for the fast Stachniss' Hungarian solver");

   Repo : constant URL := "https://github.com/mosteo/hungarian.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 GPR_Scenario ("Build_Type", "Debug"      or "Release" or
                                             "No_Options" or "Profile") and

                 Author     ("alejandro@mosteo.com") and
                 License    (LGPL_3_0));

   package V_1_0_0 is new Released
     (Base
      .Replacing (Git (Repo, "c784e09b8f9bd02c76d89b458ad6a32f65cfd146")));

end Alire.Index.Hungarian;
