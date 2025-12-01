-- Copyright (C) 2025 Benjamin Mordaunt

with Pure_Pursuit; use Pure_Pursuit;
with Ada.Text_IO.Text_Streams;

package Output_Formatters is
    -- Abstract Interface
    type Formatter is abstract tagged record
        Output_Handle : Ada.Text_IO.Text_Streams.Stream_Access;
    end record;

    procedure Start_Log (Self : in out Formatter) is abstract;

    procedure Log_Step
       (Self     : in out Formatter;
        Step     : Integer;
        Time     : Real;
        Robot    : Pose;
        Steering : Real)
    is abstract;

    procedure End_Log (Self : in out Formatter) is abstract;

    -- 1. Human Readable Implementation (Original Style)
    type Human_Readable is new Formatter with null record;

    overriding
    procedure Start_Log (Self : in out Human_Readable);
    overriding
    procedure Log_Step
       (Self     : in out Human_Readable;
        Step     : Integer;
        Time     : Real;
        Robot    : Pose;
        Steering : Real);
    overriding
    procedure End_Log (Self : in out Human_Readable);

    -- 2. JSON Implementation
    type JSON_Reporter is new Formatter with record
        Is_First_Entry : Boolean := True;
    end record;

    overriding
    procedure Start_Log (Self : in out JSON_Reporter);
    overriding
    procedure Log_Step
       (Self     : in out JSON_Reporter;
        Step     : Integer;
        Time     : Real;
        Robot    : Pose;
        Steering : Real);
    overriding
    procedure End_Log (Self : in out JSON_Reporter);

private
    generic
        type Value_Type is private;
        with function Image (Item : Value_Type) return String is <>;
    procedure Generic_Write_Pair
       (Self  : in out JSON_Reporter;
        Key   : String;
        Value : Value_Type;
        Last  : Boolean := False);
end Output_Formatters;
