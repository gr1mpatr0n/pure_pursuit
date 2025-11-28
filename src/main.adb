-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO;           use Ada.Text_IO;
with Pure_Pursuit;          use Pure_Pursuit;
with Output_Formatters;     use Output_Formatters;
with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Stream_Utils;

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
   Log_Target    : Stream_Utils.Mutable_File_Access;

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

   function Create_Formatter return Formatter'Class is
      File_Name : Unbounded_String;
   begin
      if not Arg.Parser.Parse then
         raise Program_Error with "Argument parsing failed";
      end if;

      File_Name := Arg.File.Get;

      -- GNATCOLL.Opt_Parse has a bug parsing `-` as a positional argument,
      -- so use `@` for the time being.
      if File_Name = "@" then
         Log_Target := Stream_Utils.Standard_Output_Ptr;
      else
         Create (Log_Disk_File, Out_File, To_String (File_Name));

         -- SAFETY: We know that Disk_File exists for the entire duration
         --         of Main, and so does the Formatter.
         Log_Target := Log_Disk_File'Unchecked_Access;
      end if;

      if Arg.JSON.Get then
         return
           JSON_Reporter'(Is_First_Entry => True, Output_Handle => Log_Target);
      end if;

      return Human_Readable'(Output_Handle => Log_Target);
   end Create_Formatter;

   Log : Formatter'Class := Create_Formatter;
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

      -- Polymorphic Call
      Log.Log_Step (Step, Current_Time, Robot, Steering_Angle);

      -- Update Physics
      Robot.P.X := @ + Velocity * Math.Cos (Robot.Theta) * Dt;
      Robot.P.Y := @ + Velocity * Math.Sin (Robot.Theta) * Dt;
      Robot.Theta :=
        @ + (Velocity / Wheelbase) * Math.Tan (Steering_Angle) * Dt;

   end loop;

   Log.End_Log;

   if Is_Open (Log_Disk_File) then
      Close (Log_Disk_File);
   end if;
end Main;
