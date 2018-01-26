package Alire.Index.Libhello is
   
   Libhello : constant Project := Git_Project ("libhello", 
                                               "git@bitbucket.org:aleteolabs/libhello.git");
   
   V_1_0_0 : constant Milestone := Register (Git_Release (
                                             Libhello,
                                             V ("1.0.0"),
                                             "ce78e7706c9d3f97605df48d8befca5407f8d328"));                                             

end Alire.Index.Libhello;
