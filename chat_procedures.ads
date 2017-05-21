--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  Contiene procedimientos necesarios en el programa principal de            --
--  Chat-Peer v2.0, que se usan más de una vez                                --
--                                                                            --
--  Autor: Álvaro Moles Vinader                                               --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Gnat.Ctrl_C;
with Lower_Layer_UDP;
with Debug;
with Pantalla;
with Timed_Handlers;

with Chat_Handler;
with Chat_Messages;
With Image;

package Chat_Procedures is

    package T_IO renames Ada.Text_IO;
    package ASU  renames Ada.Strings.Unbounded;
    package A_C  renames Ada.Calendar;
    package LLU  renames Lower_Layer_UDP;
    package DB   renames Debug;
    package PNT  renames Pantalla;
    package TH   renames Timed_Handlers;
    
    package CH renames Chat_Handler;    
    package CM renames Chat_Messages;
    package IM renames Image;
    
    use type LLU.End_Point_Type;
    use type ASU.Unbounded_String;
    use type CM.Message_Type;
    use type A_C.Time;
    
    procedure Flood_Send_Init;
    procedure Flood_Send_Confirm;
    procedure Flood_Send_Writer;
    procedure Flood_Send_Logout (Confirm_Sent : Boolean);
    
end Chat_Procedures;
