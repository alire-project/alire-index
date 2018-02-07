package Simple_Logging with Preelaborate is

   type Levels is (Always,
                   Error,
                   Warning,
                   Info,
                   Detail,
                   Debug);
   --  From most important to less important

   Level : Levels := Info;
   --  Any message at the same level or below will be output to console

   procedure Log (S : String; Level : Levels := Info);
   --  Report a log message

end Simple_Logging;
