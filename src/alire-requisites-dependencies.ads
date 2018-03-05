with Alire.Conditional;
with Alire.Properties.Dependencies;

package Alire.Requisites.Dependencies with Preelaborate is

   --  Special requisite that is fulfilled when a dependency is available on a platform
   --  This is checked against a special property that encapsulates the check of
   --    actual packages available once the platform is known

   package Typed is new Typed_Requisites (Properties.Dependencies.Availability_Checker);

   type Requisite is new Typed.Requisite with private;

   function New_Requisite (On : Conditional.Dependencies) return Tree;

   overriding function Is_Satisfied (R : Requisite;
                                     P : Properties.Dependencies.Availability_Checker)
                                     return Boolean;

   overriding function Image (R : Requisite) return String;

private

   type Requisite is new Typed.Requisite with record
      Deps : Conditional.Dependencies;
   end record;

   function New_Requisite (On : Conditional.Dependencies) return Tree is
      (Trees.Leaf (Requisite'(Deps => On)));

   overriding function Is_Satisfied (R : Requisite;
                                     P : Properties.Dependencies.Availability_Checker)
                                     return Boolean is
     (P.Checker.all (R.Deps.Evaluate (P.Properties)));

   overriding function Image (R : Requisite) return String is
      (R.Deps.Image_One_Line & " resolvable");

end Alire.Requisites.Dependencies;
