package Alire.Platforms with Preelaborate is

   --  Platform information necessary for some releases

   type Compilers is (GNAT_Unknown,
                      GNAT_FSF_7_2,
                      GNAT_GPL_2017);

   type Operating_Systems is (GNU_Linux,
                              Windows,
                              Unsupported);

   type Distributions is (Debian,
                          Ubuntu,
                          Unsupported);

   type Versions is (Debian_Buster,
                     Ubuntu_Xenial,
                     Ubuntu_Yakkety,
                     Ubunty_Zesty,
                     Ubuntu_Artful,
                     Unsupported);
   --  Known flavors of OSs
   --  It turns out that Debian uses no numbers for its non-stable releases, so we'll prefer the codename
   --  Not really used very much for now

   type Word_Sizes is (Bits_32,
                       Bits_64,
                       Unsupported);

   type Package_Managers is (Apt,
                             Unsupported);

   function Package_Manager (D : Distributions) return Package_Managers is
     (case D is
         when Debian | Ubuntu => Apt,
         when others => Unsupported);

end Alire.Platforms;
