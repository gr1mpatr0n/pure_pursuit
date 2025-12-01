-- Copyright (C) 2025 Benjamin Mordaunt

with Ada.Text_IO;  use Ada.Text_IO;

package body Output_Formatters is

    -----------------------
    -- Human_Readable
    -----------------------
    procedure Start_Log (Self : in out Human_Readable) is
    begin
        String'Write
           (Self.Output_Handle, "Starting Pure Pursuit Simulation..." & ASCII.LF);
        String'Write
           (Self.Output_Handle, "Format: [X, Y, Theta] -> Steering_Angle" & ASCII.LF & ASCII.LF);
    end Start_Log;

    procedure Log_Step
       (Self     : in out Human_Readable;
        Step     : Integer;
        Time     : Real;
        Robot    : Pose;
        Steering : Real) is
    begin
        String'Write
           (Self.Output_Handle,
            "Step"
            & Integer'Image (Step)
            & " (t="
            & Real'Image (Time)
            & "): ");
        String'Write
           (Self.Output_Handle,
            "Pos ["
            & Real'Image (Robot.P.X)
            & ", "
            & Real'Image (Robot.P.Y)
            & "]");
        String'Write (Self.Output_Handle, " Head " & Real'Image (Robot.Theta));
        String'Write
           (Self.Output_Handle, " -> Steer " & Real'Image (Steering) & ASCII.LF);
    end Log_Step;

    procedure End_Log (Self : in out Human_Readable) is
    begin
        String'Write (Self.Output_Handle, "End of path reached." & ASCII.LF);
    end End_Log;

    -----------------------
    -- JSON_Reporter
    -----------------------
    procedure Generic_Write_Pair
       (Self  : in out JSON_Reporter;
        Key   : String;
        Value : Value_Type;
        Last  : Boolean := False) is
    begin
        String'Write (Self.Output_Handle, """" & Key & """: " & Image (Value));
        if not Last then
            String'Write (Self.Output_Handle, ", ");
        end if;
    end Generic_Write_Pair;

    procedure Write_Pair is new
       Generic_Write_Pair (Value_Type => Integer, Image => Integer'Image);

    procedure Write_Pair is new
       Generic_Write_Pair (Value_Type => Real, Image => Real'Image);

    procedure Write_Pair
       (Self : in out JSON_Reporter;
        Key  : String;
        Item : Pose;
        Last : Boolean := False) is
    begin
        String'Write (Self.Output_Handle, """" & Key & """: {");

        Write_Pair (Self, "x", Item.P.X);
        Write_Pair (Self, "y", Item.P.Y);
        Write_Pair (Self, "theta", Item.Theta, Last => True);

        String'Write (Self.Output_Handle, "}");

        if not Last then
            String'Write (Self.Output_Handle, ", ");
        end if;
    end Write_Pair;

    procedure Start_Log (Self : in out JSON_Reporter) is
    begin
        String'Write (Self.Output_Handle, "[" & ASCII.LF);
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
            String'Write (Self.Output_Handle, "," & ASCII.LF);
        end if;
        Self.Is_First_Entry := False;

        -- Manual JSON construction
        String'Write (Self.Output_Handle, "  {");

        Write_Pair (Self, "step", Step);
        Write_Pair (Self, "time", Time);
        Write_Pair (Self, "pose", Robot);
        Write_Pair (Self, "steering", Steering, Last => True);

        String'Write (Self.Output_Handle, "}");
    end Log_Step;

    procedure End_Log (Self : in out JSON_Reporter) is
    begin
        String'Write (Self.Output_Handle, ASCII.LF & "]" & ASCII.LF);
    end End_Log;

end Output_Formatters;
