-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers.Vectors;

package Pure_Pursuit is
    type Real is new Float;

    package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
    use Math;

    Wheelbase : constant Real := 2.5;

    type Point is record
        X, Y : Real;
    end record;

    type Pose is record
        P     : Point;
        Theta : Real;
    end record;

    package Point_Vectors is new
       Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Point);

    use Point_Vectors;

    type Controller is tagged record
        Path           : Vector;
        Current_Idx    : Positive := 1;
        Lookahead_Dist : Real;
    end record;

    procedure Compute_Steering
       (Ctx          : in out Controller;
        Robot_Pos    : Pose;
        Steering     : out Real;
        Target_Found : out Boolean)
    with
       Pre  => Ctx.Lookahead_Dist > 0.0,
       Post => (if Target_Found then abs (Steering) <= Real (Ada.Numerics.Pi));

    procedure Add_Waypoint (Ctx : in out Controller; X, Y : Real);
end Pure_Pursuit;
