--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Contiene las declaraciones de tipos y variables necesarios en el programa  --
-- Chat-Peer v2.0                                                             --
--                                                                            --        
-- Autor: Álvaro Moles Vinader                                                --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Calendar;
with Lower_Layer_UDP;

package Chat_Messages is

    package ASU renames Ada.Strings.Unbounded;
    package A_C renames Ada.Calendar;
    package LLU renames Lower_Layer_UDP;
    
    ----------------------------------------------------------------------------        
    --                                   Tipos                                --
    ----------------------------------------------------------------------------
    
    type Message_Type is (Init, Reject, Confirm, Writer, Logout, Ack, 
                                 SN_Query, SN_Response);
    type Time_Msg_T   is (New_Msg, Past_Msg, Future_Msg);
    type Seq_N_T      is mod Integer'Last;
    
    -- Sender Dests
    
    type Mess_Id_T is record
        EP_H_Creat : LLU.End_Point_Type;
        Seq_N      : Seq_N_T;
    end record;

    type Destination_T is record                                        
        EP_H_ACKer : LLU.End_Point_Type  := null;
        Retries    : Natural             := 0;
    end record;
    
    type Destinations_T is array (1..10) of Destination_T;  -- No se puede usar 1..CH.Neighbors.Max_Length_Value 
                                                            -- (CH.Neighbors_Map) porque se crea dependencia circular
    
    -- Sender Buffering    
                                             
    type Buffer_A_T is access LLU.Buffer_Type;
    
    type Value_T is record
        EP_H_Creat : LLU.End_Point_Type;
        Seq_N      : Seq_N_T;
        P_Buffer   : Buffer_A_T;
    end record;
    
    ----------------------------------------------------------------------------        
    --                     Procedimientos y funciones                         --
    ----------------------------------------------------------------------------
    
    function Seq_N_Increased (N: Seq_N_T) return Seq_N_T;
    
    ----------------------------------------------------------------------------        
    --                              Variables                                 --
    ----------------------------------------------------------------------------
    
    -- Campos de mensajes
    My_EP_R  : LLU.End_Point_Type;
    My_EP_H  : LLU.End_Point_Type;
    My_Nick  : ASU.Unbounded_String;
    My_Dests : Destinations_T;
    My_Seq_N : Seq_N_T := 0;                                          
    My_Text  : ASU.Unbounded_String;
       
    -- Temporales
     Resend_Time : Duration;

    -- Punteros a Buffers
    P_Buffer_Main    : Buffer_A_T;
    P_Buffer_Handler : Buffer_A_T;

end Chat_Messages;
