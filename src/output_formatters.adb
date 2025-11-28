-- Copyright (C) 2025 Benjamin Mordaunt

package body Output_Formatters is

    -----------------------
    -- Human_Readable
    -----------------------
    procedure Start_Log (Self : in out Human_Readable) is
    begin
        Put_Line
           (Self.Output_Handle.all, "Starting Pure Pursuit Simulation...");
        Put_Line
           (Self.Output_Handle.all, "Format: [X, Y, Theta] -> Steering_Angle");
        New_Line (Self.Output_Handle.all);
    end Start_Log;

    procedure Log_Step
       (Self     : in out Human_Readable;
        Step     : Integer;
        Time     : Real;
        Robot    : Pose;
        Steering : Real) is
    begin
        Put
           (Self.Output_Handle.all,
            "Step"
            & Integer'Image (Step)
            & " (t="
            & Real'Image (Time)
            & "): ");
        Put
           (Self.Output_Handle.all,
            "Pos ["
            & Real'Image (Robot.P.X)
            & ", "
            & Real'Image (Robot.P.Y)
            & "]");
        Put (Self.Output_Handle.all, " Head " & Real'Image (Robot.Theta));
        Put_Line
           (Self.Output_Handle.all, " -> Steer " & Real'Image (Steering));
    end Log_Step;

    procedure End_Log (Self : in out Human_Readable) is
    begin
        Put_Line (Self.Output_Handle.all, "End of path reached.");
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
        Put (Self.Output_Handle.all, """" & Key & """: " & Image (Value));
        if not Last then
            Put (Self.Output_Handle.all, ", ");
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
        Put (Self.Output_Handle.all, """" & Key & """: {");

        Write_Pair (Self, "x", Item.P.X);
        Write_Pair (Self, "y", Item.P.Y);
        Write_Pair (Self, "theta", Item.Theta, Last => True);

        Put (Self.Output_Handle.all, "}");

        if not Last then
            Put (Self.Output_Handle.all, ", ");
        end if;
    end Write_Pair;

    procedure Start_Log (Self : in out JSON_Reporter) is
    begin
        Put_Line (Self.Output_Handle.all, "[");
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
            Put_Line (Self.Output_Handle.all, ",");
        end if;
        Self.Is_First_Entry := False;

        -- Manual JSON construction
        Put (Self.Output_Handle.all, "  {");

        Write_Pair (Self, "step", Step);
        Write_Pair (Self, "time", Time);
        Write_Pair (Self, "pose", Robot);
        Write_Pair (Self, "steering", Steering, Last => True);

        Put (Self.Output_Handle.all, "}");
    end Log_Step;

    procedure End_Log (Self : in out JSON_Reporter) is
    begin
        New_Line (Self.Output_Handle.all);
        Put_Line (Self.Output_Handle.all, "]");
    end End_Log;

end Output_Formatters;
