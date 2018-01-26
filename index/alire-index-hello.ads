package Alire.Index.Hello is

   Hello : constant Project := Git_Project ("hello", 
                                               "git@bitbucket.org:aleteolabs/hello.git");
   
   V_1_0_0  : constant Milestone := Register (Git_Release (
                                              Hello,
                                              V ("1.0.0"),
                                              "8cac0afddc505794ae3e5634745ce0830129d241"));    

end Alire.Index.Hello;
