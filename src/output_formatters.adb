-- Copyright (C) 2025 Benjamin Mordaunt
package body Output_Formatters is

   -----------------------
   -- Human_Readable
   -----------------------

   procedure Start_Log (Self : in out Human_Readable) is
   begin
      Put_Line ("Starting Pure Pursuit Simulation...");
      Put_Line ("Format: [X, Y, Theta] -> Steering_Angle");
      New_Line;
   end Start_Log;

   procedure Log_Step 
     (Self     : in out Human_Readable; 
      Step     : Integer; 
      Time     : Real; 
      Robot    : Pose; 
      Steering : Real) is
   begin
      Put ("Step" & Integer'Image(Step) & " (t=" & Real'Image(Time) & "): ");
      Put ("Pos [" & Real'Image(Robot.P.X) & ", " & Real'Image(Robot.P.Y) & "]");
      Put (" Head " & Real'Image(Robot.Theta));
      Put_Line (" -> Steer " & Real'Image(Steering));
   end Log_Step;

   procedure End_Log (Self : in out Human_Readable) is
   begin
      Put_Line ("End of path reached.");
   end End_Log;

   -----------------------
   -- JSON_Reporter
   -----------------------
   procedure Generic_Write_Pair
      (Self : in out JSON_Reporter;
       Key  : String;
       Value : Value_Type;
       Last  : Boolean := False) is
   begin
      Put (Self.JSON_File, """" & Key & """: " & Image (Value));
      if not Last then
         Put (Self.JSON_File, ", ");
      end if;
   end Generic_Write_Pair;

   procedure Write_Pair is new Generic_Write_Pair
      (Value_Type  => Integer,
       Image       => Integer'Image);

   procedure Write_Pair is new Generic_Write_Pair
      (Value_Type  => Real,
       Image       => Real'Image);

   procedure Write_Pair 
     (Self : in out JSON_Reporter; 
      Key  : String; 
      Item : Pose; 
      Last : Boolean := False) is
   begin
      Put (Self.JSON_File, """" & Key & """: {");

      Write_Pair (Self, "x",     Item.P.X);
      Write_Pair (Self, "y",     Item.P.Y);
      Write_Pair (Self, "theta", Item.Theta, Last => True);

      Put (Self.JSON_File, "}");

      if not Last then Put (Self.JSON_File, ", "); end if;
   end Write_Pair;

   procedure Start_Log (Self : in out JSON_Reporter) is
   begin
      Create (File => Self.JSON_File, Name => "out.json");
      Put_Line (Self.JSON_File, "[");
      Self.Is_First_Entry := True;
   end Start_Log;

   procedure Log_Step 
     (Self     : in out JSON_Reporter; 
      Step     : Integer; 
      Time     : Real; 
      Robot    : Pose; 
      Steering : Real) is
   begin
      if not Self.Is_First_Entry then
         Put_Line (Self.JSON_File, ",");
      end if;
      Self.Is_First_Entry := False;

      -- Manual JSON construction
      Put (Self.JSON_File, "  {");
      
      Write_Pair (Self, "step", Step);
      Write_Pair (Self, "time", Time);
      Write_Pair (Self, "pose", Robot);
      Write_Pair (Self, "steering", Steering, Last => True);

      Put (Self.JSON_File, "}");
   end Log_Step;

   procedure End_Log (Self : in out JSON_Reporter) is
   begin
      New_Line (Self.JSON_File);
      Put_Line (Self.JSON_File, "]");
   end End_Log;

end Output_Formatters;