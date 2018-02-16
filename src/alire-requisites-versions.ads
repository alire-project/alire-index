with Alire.Properties.Versions;

package Alire.Requisites.Versions with Preelaborate is

   package Semver renames Semantic_Versioning;

   function Exactly (V : Semver.Version) return Requisite'Class;

private

   package Props renames Alire.Properties.Versions;

   package Version_Requisites is new Typed_Requisites (Props.Values.Property'Class);

   type Version_Requisite is new Version_Requisites.Requisite with record
      Set : Semver.Version_Set;
   end record;

   ------------------
   -- Is_Satisfied --
   ------------------

   overriding function Is_Satisfied (R : Version_Requisite;
                                     P : Props.Values.Property'Class) return Boolean is
     (Semver.Is_In (V => P.Element, VS => R.Set));

   -------------
   -- Exactly --
   -------------

   function Exactly (V : Semver.Version) return Requisite'Class is
      (Version_Requisite'(Set => Semver.Exactly (V)));

end Alire.Requisites.Versions;
