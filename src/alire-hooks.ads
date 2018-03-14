with Alire.Origins;

package Alire.Hooks with Preelaborate is

   --  Hackish way to enable Alire to obtain some platform-specific info that only Alr knows how to get

   Version_Getter_Hook : access function (O : Origins.Origin) return String;

end Alire.Hooks;
