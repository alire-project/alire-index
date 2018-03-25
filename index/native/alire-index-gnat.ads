package Alire.Index.GNAT is

--  This is apparently a good idea but it is not, in practice
--  Since the compiler is a requisite of alr itself, there's no point in requiring it again here
--  Furthermore, the compiler might be installed but not be the one in use
-- (e.g., GPL version is in an earlier path)
--  Conclusion: use the requisite Compiler_Is_Native instead

--  Still, this might come back for cross-compilations or sumzing

--     function Project is new Catalogued_Project (Projects.GNAT);
--
--     --  If minor versions proved important they could be segregated with platform-specific knowledge
--
--     V_8 : constant Release :=
--             Project.Register
--               (V ("8"),
--                Native ((Debian | Ubuntu => Packaged_As ("gnat-8"),
--                         others          => Unavailable)));
--
--     V_7 : constant Release :=
--             Project.Register
--               (V ("7"),
--                Native ((Debian | Ubuntu => Packaged_As ("gnat-7"),
--                         others          => Unavailable)));

end Alire.Index.GNAT;
