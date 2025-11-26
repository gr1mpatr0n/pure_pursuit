-- Copyright (C) 2025 Benjamin Mordaunt

package body Pure_Pursuit is
    procedure Add_Waypoint (Ctx : in out Controller; X, Y : Real) is
    begin
        Ctx.Path.Append (New_Item => Point'(X => X, Y => Y));
    end Add_Waypoint;

    function Distance (P1, P2 : Point) return Real is
    begin
        return Sqrt ((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
    end Distance;

    function Global_To_Robot (Robot : Pose; Goal : Point) return Point is
        DX : constant Real := Goal.X - Robot.P.X;
        DY : constant Real := Goal.Y - Robot.P.Y;

        Local_X : constant Real := DX * Cos(Robot.Theta) + DY * Sin(Robot.Theta);
        Local_Y : constant Real := -DX * Sin(Robot.Theta) + DY * Cos(Robot.Theta);
    begin
        return (X => Local_X, Y => Local_Y);
    end Global_To_Robot;

    procedure Compute_Steering
                  (Ctx : in out Controller;
                   Robot_Pos : Pose;
                   Steering : out Real;
                   Target_Found : out Boolean)
    is
        Best_Point : Point;
        Found : Boolean := False;
        Dist : Real;
    begin
        for I in Ctx.Current_Idx .. Ctx.Path.Last_Index loop
            Dist := Distance (Robot_Pos.P, Ctx.Path(I));

            if Dist >= Ctx.Lookahead_Dist then
                Best_Point := Ctx.Path(I);

                Ctx.Current_Idx := @;
                if I > 1 then
                    Ctx.Current_Idx := I - 1;
                end if;

                Found := True;
                exit;
            end if;
        end loop;

        Target_Found := Found;

        if not Found then
            Steering := 0.0;
            return;
        end if;

        declare
            Local_Goal : constant Point := Global_To_Robot (Robot_Pos, Best_Point);
            Curvature : Real;
        begin
            Curvature := (2.0 * Local_Goal.Y) / (Ctx.Lookahead_Dist ** 2);

            Steering := Arctan (Curvature * Wheelbase);
        end;
    end Compute_Steering;
end Pure_Pursuit;
