package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Compilers is (GNAT_Unknown,
                      GNAT_FSF_7_2,
                      GNAT_GPL_2017);

   type Operating_Systems is (GNU_Linux,
                              Windows);

end Alire.Platforms;
