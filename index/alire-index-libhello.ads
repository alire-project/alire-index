package Alire.Index.Libhello is
   
   Name : constant Project_Name := "libhello";   
   Repo : constant URL          := "git@bitbucket.org:aleteolabs/libhello.git";
   
   V_1_0_0 : constant Release := 
               Register_Git (Name,
                             V ("1.0.0"),
                             Repo,
                             "ce78e7706c9d3f97605df48d8befca5407f8d328");

end Alire.Index.Libhello;
