package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Compilers is (GNAT_Unknown,
                      GNAT_FSF_7_2,
                      GNAT_GPL_2017);

   type Operating_Systems is (GNU_Linux,
                              Windows,
                              Unsupported);

   type Distributions is (Debian_Buster,
                          Ubuntu_Artful,
                          Unsupported);
   --  Known flavors of OSs
   --  It turns out that Debian uses no numbers for its non-stable releases, so we'll prefer the codename
   --  These are important mostly to tie platform package names to releases

end Alire.Platforms;
