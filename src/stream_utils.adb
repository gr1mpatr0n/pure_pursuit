package body Stream_Utils is
   function Standard_Output_Ptr return Mutable_File_Access is
      function To_Mutable is new Ada.Unchecked_Conversion
         (Source => File_Access, Target => Mutable_File_Access);
   begin
      return To_Mutable (Standard_Output);
   end Standard_Output_Ptr;
end Stream_Utils;
