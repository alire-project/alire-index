package Alire.Index.XMLAda is

   function Project is new Catalogued_Project ("The XML/Ada toolkit");

   Repo : constant URL := "https://github.com/AdaCore/xmlada.git";

   Base : constant Release := Project.Unreleased
     (Properties =>
        Maintainer ("AdaCore") and
        Website    ("https://github.com/AdaCore/xmlada") and
        License    (GPL_3_0) and

        Action_Run (Post_Fetch, "sh configure"));

   ---------
   -- DOM --
   ---------

   package DOM is

      function Project is new Catalogued_Project
        (XMLAda.Project.Description & " (DOM)");

      Base : constant Release := Project.Unreleased
        (Properties =>
           XMLAda.Base.Properties and
           Project_File ("dom/xmlada_dom.gpr"));

      package V_18_2 is new Project_Release
        (Base
         .Replacing
           (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

   end DOM;

   -------------------
   -- Input_Sources --
   -------------------

   package Input_Sources is

      function Project is new Catalogued_Project
        (XMLAda.Project.Description & " (Input Sources)");

      Base : constant Release := Project.Unreleased
        (Properties =>
           XMLAda.Base.Properties and
           Project_File ("input_sources/xmlada_input.gpr"));

      package V_18_2 is new Project_Release
        (Base
         .Replacing
           (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

   end Input_Sources;

   ---------
   -- SAX --
   ---------

   package SAX is

      function Project is new Catalogued_Project
        (XMLAda.Project.Description & " (SAX)");

      Base : constant Release := Project.Unreleased
        (Properties =>
           XMLAda.Base.Properties and
           Project_File ("sax/xmlada_sax.gpr"));

      package V_18_2 is new Project_Release
        (Base
         .Replacing
           (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

   end SAX;

   ------------
   -- Schema --
   ------------

   package Schema is

      function Project is new Catalogued_Project
        (XMLAda.Project.Description & " (Schema)");

      Base : constant Release := Project.Unreleased
        (Properties =>
           XMLAda.Base.Properties and
           Project_File ("schema/xmlada_schema.gpr"));

      package V_18_2 is new Project_Release
        (Base
         .Replacing
           (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

   end Schema;

   -------------
   -- Unicode --
   -------------

   package Unicode is

      function Project is new Catalogued_Project
        (XMLAda.Project.Description & " (Unicode)");

      Base : constant Release := Project.Unreleased
        (Properties =>
           XMLAda.Base.Properties and
           Project_File ("unicode/xmlada_unicode.gpr"));

      package V_18_2 is new Project_Release
        (Base
         .Replacing
           (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

   end Unicode;

end Alire.Index.XMLAda;
