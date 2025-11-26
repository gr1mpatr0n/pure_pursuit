-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Unchecked_Deallocation; 
with Pure_Pursuit;             use Pure_Pursuit;
with Output_Formatters;        use Output_Formatters;

procedure Main is
   -- Simulation State
   Ctrl : Controller := (Path           => Point_Vectors.Empty_Vector,
                         Current_Idx    => 1,
                         Lookahead_Dist => 3.0);
   
   Robot : Pose := (P => (0.0, 0.0), Theta => 0.0);
   
   Steering_Angle : Real;
   Has_Target     : Boolean;
   
   Velocity       : constant Real := 1.0; 
   Dt             : constant Real := 0.1;
   Current_Time   : Real := 0.0;

   type Formatter_Access is access all Formatter'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Formatter'Class, Name => Formatter_Access);

   Log : Formatter_Access;

   function Create_Formatter return Formatter_Access is
   begin
      if Argument_Count > 0 then
         declare
            Arg : constant String := Argument(1);
         begin
            if Arg = "JSON" or Arg = "json" or Arg = "--json" then
               return new JSON_Reporter'(Is_First_Entry => True);
            end if;
         end;
      end if;
      
      return new Human_Readable;
   end Create_Formatter;

begin
   -- Initialize logic
   Log := Create_Formatter;

   -- Setup Path
   for I in 0 .. 20 loop
      Add_Waypoint (Ctrl, Real(I) * 1.5, Real(I) * 1.0);
   end loop;

   -- Simulation Loop
   Log.Start_Log;

   for Step in 1 .. 30 loop
      Current_Time := Real(Step) * Dt;
      
      Compute_Steering (Ctrl, Robot, Steering_Angle, Has_Target);

      if not Has_Target then
         exit;
      end if;

      -- Polymorphic Call
      Log.Log_Step (Step, Current_Time, Robot, Steering_Angle);

      -- Update Physics
      Robot.P.X   := @ + Velocity * Math.Cos(Robot.Theta) * Dt;
      Robot.P.Y   := @ + Velocity * Math.Sin(Robot.Theta) * Dt;
      Robot.Theta := @ + (Velocity / Wheelbase) * Math.Tan(Steering_Angle) * Dt;

   end loop;

   Log.End_Log;
   Free (Log);

end Main;