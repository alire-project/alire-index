package Alire.Origins is

   --  Minimal information about origins of sources.
   --  We use the term origins to avoid mixing 'alire sources' with 'project sources' or other 'sources'.

   --  The actual capabilities for check-outs or fetches are in alr proper

   type Kinds is (Apt, Git, Hg, HTTP, RPM);
   --  Only Apt-installed and Git remotes supported, for now

   type Origin (Kind : Kinds) is tagged private;

   function New_Origin (Kind : Kinds;
                        URL  : Alire.URL;  -- A locator for the resource/server/entity containing the sources
                        ID   : String)     -- A unique identifier within the server (commit id, zipfile, package name...)
                        return Origin;
   --  This should be general enough for all foreseeable sources.

   function URL (This : Origin) return String;

end Alire.Origins;
