with GNAT.IO; -- To keep preelaborable

package body Alire.Releases is

   -----------
   -- Print --
   -----------

   procedure Print (R : Release) is
      use GNAT.IO;
   begin
      --  MILESTONE
      Put_Line (R.Milestone_Image & ": " & R.Description);

      --  ORIGIN
      Put_Line ("Origin: " & R.Origin.Image);

      --  REQUISITES
      if not R.Reqs.Is_Empty then
         Put ("Requisites: ");
         R.Reqs.Print_Skeleton;
      end if;

      --  PROPERTIES
      if not R.Props.Is_Empty then
         Put_Line ("Properties:");
         for Prop of R.Props loop
            Put_Line ("   " & Prop.Image);
         end loop;
      end if;

      --  DEPENDENCIES
      if not R.Depends.Is_Empty then
         Put_Line ("Dependencies (direct):");
         for Dep of R.Depends loop
            Put_Line ("   " & Dep.Image);
         end loop;
      end if;
   end Print;

end Alire.Releases;
