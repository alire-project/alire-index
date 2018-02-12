package Alire.Platform with Preelaborate is

   --  OSs supported by a release
   type Operating_Systems is (Cross_Platform,
                              GNU_Linux,
                              Windows);

   --  Compilers known to compile a release
   type Compilers is (GNAT_Any,
                      GNAT_FSF_7_2,
                      GNAT_GPL_2017);

end Alire.Platform;
