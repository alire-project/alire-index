with GNAT.IO;

package body Alire.Properties is

   -----------
   -- Print --
   -----------

   procedure Print (V : Vector; Prefix : String := "") is
   begin
      for Prop of V loop
         GNAT.IO.Put_Line (Prefix & Prop.Image);
      end loop;
   end Print;

end Alire.Properties;
