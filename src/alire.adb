with GNAT.IO;

package body Alire is

   procedure Log (S : String; Level : Verbosities := Terse) is
   begin
      if Level >= Verbosity then
         GNAT.IO.Put_Line
           ((case Level is
               when Terse => "",
               when Verbose => ": ",
               when Debug   => "+++ ") & S);
      end if;
   end Log;

end Alire;
