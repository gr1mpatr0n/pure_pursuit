-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Pure_Pursuit;             use Pure_Pursuit;
with Output_Formatters;        use Output_Formatters;
with GNATCOLL.Opt_Parse;       use GNATCOLL.Opt_Parse;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

procedure Main is
   -- Simulation State
   Ctrl : Controller :=
     (Path           => Point_Vectors.Empty_Vector,
      Current_Idx    => 1,
      Lookahead_Dist => 3.0);

   Robot : Pose := (P => (0.0, 0.0), Theta => 0.0);

   Steering_Angle : Real;
   Has_Target     : Boolean;

   Velocity     : constant Real := 1.0;
   Dt           : constant Real := 0.1;
   Current_Time : Real := 0.0;

   Log_Disk_File : aliased File_Type;

   package Arg is
      Parser : Argument_Parser :=
        Create_Argument_Parser (Help => "Pure Pursuit in Ada.");

      package JSON is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--json",
           Help   => "Output pose and steering data in JSON format.");

      package File is new
        Parse_Positional_Arg
          (Parser   => Parser,
           Name     => "file",
           Arg_Type => Unbounded_String,
           Help     => "The name of a file to output, or @ for stdout.");
   end Arg;

   File_Name : Unbounded_String;
begin
   if not Arg.Parser.Parse then
      raise Program_Error with "Argument parsing failed";
   end if;

   File_Name := Arg.File.Get;

   if File_Name /= "@" then
      Create (Log_Disk_File, Out_File, To_String (File_Name));
   end if;

   declare
      Log_Target : Stream_Access :=
         (if File_Name = "@"
          then Stream (Standard_Output)
          else Stream (Log_Disk_File));

      Log : Formatter'Class :=
         (if Arg.JSON.Get
          then JSON_Reporter'(Is_First_Entry => True, Output_Handle => Log_Target)
          else Human_Readable'(Output_Handle => Log_Target));
   begin
      -- Setup Path
      for I in 0 .. 20 loop
         Add_Waypoint (Ctrl, Real (I) * 1.5, Real (I) * 1.0);
      end loop;

      -- Simulation Loop
      Log.Start_Log;

      for Step in 1 .. 30 loop
         Current_Time := Real (Step) * Dt;
         Compute_Steering (Ctrl, Robot, Steering_Angle, Has_Target);

         if not Has_Target then
            exit;
         end if;

         Log.Log_Step (Step, Current_Time, Robot, Steering_Angle);

         -- Update Physics
         Robot.P.X := @ + Velocity * Math.Cos (Robot.Theta) * Dt;
         Robot.P.Y := @ + Velocity * Math.Sin (Robot.Theta) * Dt;
         Robot.Theta :=
            @ + (Velocity / Wheelbase) * Math.Tan (Steering_Angle) * Dt;
      end loop;

      Log.End_Log;
   end;
   
   if Is_Open (Log_Disk_File) then
      Close (Log_Disk_File);
   end if;
end Main;
