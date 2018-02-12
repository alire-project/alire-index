package body Alire.Requisites is

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (R : Requisite'Class; P : Properties.Vector) return Boolean is
   begin
      for Prop of P loop
         if R.Satisfies (P) then
            return True;
         end if;
      end loop;

      return False;
   end Satisfies;

end Alire.Requisites;
