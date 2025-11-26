-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Pure_Pursuit;

procedure Main is
    use Pure_Pursuit;

    -- Initialize Controller
    Ctrl : Controller := (Path           => Point_Vectors.Empty_Vector,
                          Current_Idx    => 1,
                          Lookahead_Dist => 3.0);

    -- Robot State
    Robot : Pose := (P => (0.0, 0.0), Theta => 0.0);

    Steering_Angle : Real;
    Has_Target     : Boolean;
    Velocity       : constant Real := 1.0; -- m/s
    Dt             : constant Real := 0.1; -- Time step
begin
    -- 1. Create a Path (A simple diagonal line for demo)
    for I in 0 .. 20 loop
        Add_Waypoint (Ctrl, Real(I) * 1.5, Real(I) * 1.0);
    end loop;

    Put_Line ("Starting Pure Pursuit Simulation...");
    Put_Line ("Format: [X, Y, Theta] -> Steering_Angle");

    -- 2. Simulation Loop
    for Step in 1 .. 30 loop

        -- Compute Control
        Compute_Steering (Ctrl, Robot, Steering_Angle, Has_Target);

        if not Has_Target then
            Put_Line ("End of path reached.");
            exit;
        end if;

        -- Output Status (Formatted manually for clarity)
        Put ("Pos: [" & Real'Image(Robot.P.X) & ", " & Real'Image(Robot.P.Y) & "]");
        Put (" Head: " & Real'Image(Robot.Theta));
        Put_Line (" -> Steer: " & Real'Image(Steering_Angle));

        -- 3. Update Robot Kinematics (Bicycle Model)
        -- Update X, Y
        Robot.P.X := @ + Velocity * Math.Cos(Robot.Theta) * Dt;
        Robot.P.Y := @ + Velocity * Math.Sin(Robot.Theta) * Dt;

        -- Update Heading
        -- Theta_new = Theta + (v / L) * tan(delta) * dt
        Robot.Theta := @ + (Velocity / Wheelbase) * Math.Tan(Steering_Angle) * Dt;

    end loop;

end Main;
