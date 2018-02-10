package Alire.Platform with Preelaborate is

   type Operating_Systems is (Unknown,
                              Linux,
                              Windows);

   type Compilers is (Unknown,
                      GNAT_GPL_2017,
                      GNAT_FSF_7_2);

   function Compiler return Compilers is (Unknown);

end Alire.Platform;
