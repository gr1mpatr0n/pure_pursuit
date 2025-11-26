-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO; use Ada.Text_IO;

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

   procedure Start_Log (Self : in out JSON_Reporter) is
   begin
      Put_Line ("[");
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
         Put_Line (",");
      end if;
      Self.Is_First_Entry := False;

      -- Manual JSON construction
      Put ("  {");
      Put ("""step"": " & Integer'Image(Step) & ", ");
      Put ("""time"": " & Real'Image(Time) & ", ");
      
      Put ("""pose"": {");
      Put ("""x"": " & Real'Image(Robot.P.X) & ", ");
      Put ("""y"": " & Real'Image(Robot.P.Y) & ", ");
      Put ("""theta"": " & Real'Image(Robot.Theta));
      Put ("}, ");
      
      Put ("""steering"": " & Real'Image(Steering));
      Put ("}");
   end Log_Step;

   procedure End_Log (Self : in out JSON_Reporter) is
   begin
      New_Line;
      Put_Line ("]");
   end End_Log;

end Output_Formatters;