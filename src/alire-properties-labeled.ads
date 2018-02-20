private with Alire.Utils;

package Alire.Properties.Labeled with Preelaborate is

   --  Properties that have a single string value and a name

   type Labels is (Maintainer,
                   Website);

   type Label (<>) is new Properties.Property with private;

   function New_Label (Name : Labels; Value : String) return Label;

   function Name (L : Label) return Labels;

   function Value (L : Label) return String;

   overriding function Image (L : Label) return String;

   generic
      Name : Labels;
   function Generic_New_Label (Value : String) return Label;
   --  Returns a vector so its directly usable during indexing


private

   type Label (Length : Natural) is new Properties.Property with record
      Name  : Labels;
      Value : String (1 .. Length);
   end record;

   function New_Label (Name : Labels; Value : String) return Label is
     (Properties.Property with Value'Length, Name, Value);

   function Name (L : Label) return Labels is (L.Name);

   function Value (L : Label) return String is (L.Value);

   function Generic_New_Label (Value : String) return Label is (New_Label (Name, Value));

   overriding function Image (L : Label) return String is (Utils.To_Mixed_Case (L.Name'Img & ": " & L.Value));

end Alire.Properties.Labeled;
