with Alire.Index;

package Alire.Project renames Alire.Index;

--  Since the facilities used to register projects in the index are the same used
--  by a working project to state dependencies, instead of duplicating them or
--  forcing clients to with everything around, everything is done inside Index
