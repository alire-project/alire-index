with Alire; -- Kind of circularity here but somehow it slips past static GNAT rules??

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Simple_Logging;

package body Alire_Early_Elaboration is

   ----------------------------
   -- Early_Switch_Detection --
   ----------------------------

   procedure Early_Switch_Detection is
      use GNAT.Command_Line;

      Config : Command_Line_Configuration;

   begin
      Define_Switch (Config,
                     Switch_Q'Access,
                     "-q",
                     Help => "Limit output to errors");
      Define_Switch (Config,
                     Switch_V'Access,
                     "-v",
                     Help => "Be more verbose");

      Define_Switch (Config,
                     Switch_D'Access,
                     "-d",
                     Help => "Be even more verbose (including debug messages)");

      Getopt (Config);

      --  Exclusivity check
      if (Switch_D and Switch_V) or (Switch_D and Switch_Q) or (Switch_V and Switch_Q) then
         Alire.Trace.Info ("Ada Library Repository manager (alr)");
         Alire.Trace.Error ("Only one verbosity switch allowed (either -d, -v or -q)");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Level setting
      if Switch_D then
         Alire.Log_Level := Simple_Logging.Debug;
      elsif Switch_V then
         Alire.Log_Level := Simple_Logging.Detail;
      elsif Switch_Q then
         Alire.Log_Level := Simple_Logging.Error;
      end if;
   end Early_Switch_Detection;

begin
   Early_Switch_Detection;
end Alire_Early_Elaboration;
