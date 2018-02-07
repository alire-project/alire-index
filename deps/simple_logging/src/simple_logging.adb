with GNAT.IO;

package body Simple_Logging is

   function Prefix (Level : Levels) return String is
     (case Level is
         when Always  => "",
         when Error   => "ERROR: ",
         when WARNING => "Warning: ",
         when Info    => "",
         when Detail  => "-> ",
         when Debug   => "-->> ");

   ---------
   -- Log --
   ---------

   procedure Log (S : String; Level : Levels := Info) is
   begin
      if Level <= Simple_Logging.Level then
         GNAT.IO.Put_Line (Prefix (Level) & S);
      end if;
   end Log;

end Simple_Logging;
