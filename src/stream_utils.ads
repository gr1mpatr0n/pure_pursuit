with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package Stream_Utils is
   type Mutable_File_Access is access all File_Type;

   function Standard_Output_Ptr return Mutable_File_Access;
end Stream_Utils;