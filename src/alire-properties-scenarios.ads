with Alire.GPR;

private with Ada.Containers.Indefinite_Holders;

package Alire.Properties.Scenarios with Preelaborate is

   type Variable is new Property with private;

   function New_Variable (V : GPR.Variable) return Variable;

   overriding function Image (V : Variable) return String;

private

   package Holders is new Ada.Containers.Indefinite_Holders (Gpr.Variable, GPR."=");

   type Variable is new Property with record
      Var : Holders.Holder;
   end record;

   function New_Variable (V : GPR.Variable) return Variable is
      (Var => Holders.To_Holder (V));

   overriding function Image (V : Variable) return String is
      ("GPR Scenario: " & V.Var.Constant_Reference.Image);

end Alire.Properties.Scenarios;
