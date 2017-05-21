--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--    Contiene las instanciaciones de los mapas genéricos necesarios en       --
--    Chat-Peer y el procedimiento para el envío y recepción de mensajes      --
--    mediante Handler                                                        --
--                                                                            --
--    Autor: Álvaro Moles Vinader                                             --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Lower_Layer_UDP;
with Debug;
with Pantalla;
with Timed_Handlers;

with Maps_G;
with Maps_Protector_G;
with Ordered_Maps_G;
with Ordered_Maps_Protector_G;

with Chat_Messages;
with Image;
with Compare;

package Chat_Handler is

    package T_IO renames Ada.Text_IO;
    package A_C  renames Ada.Calendar;
    package ASU  renames Ada.Strings.Unbounded;
    package C_L  renames Ada.Command_Line;
    package LLU  renames Lower_Layer_UDP;
    package DB   renames Debug;
    package PNT  renames Pantalla;
    package TH   renames Timed_Handlers;
    
    package CM  renames Chat_Messages;
    package IM  renames Image;
    package CMP renames Compare;
    
    use type LLU.End_Point_Type;
    use type ASU.Unbounded_String;
    use type CM.Message_Type;
    use type CM.Time_Msg_T;
    use type CM.Seq_N_T;
    use type A_C.Time;

    ----------------------------------------------------------------------------        
    --                                  Mapas                                 --
    ----------------------------------------------------------------------------
                            
    package NP_Neighbors is new Maps_G (Key_Type          => LLU.End_Point_Type,
                                        Value_Type        => A_C.Time,
                                        "="               => LLU."=",
                                        Key_To_String     => IM.EP_Image,
                                        Value_To_String   => IM.Time_Image,
                                        Null_Key          => null,
                                        Null_Value        => A_C.Clock,
                                        Max_Length        => 10);
                                
    package NP_Latest_Msgs is new Maps_G (Key_Type        => LLU.End_Point_Type,
                                          Value_Type      => CM.Seq_N_T,
                                          "="             => LLU."=",
                                          Key_To_String   => IM.EP_Image,
                                          Value_To_String => CM.Seq_N_T'Image,
                                          Null_Key        => null,
                                          Null_Value      => 0,
                                          Max_Length      => 50);

    package NP_Sender_Dests is new 
                     Ordered_Maps_G (Key_Type         => CM.Mess_Id_T,
                                     Value_Type       => CM.Destinations_T,
                                     "="              => CMP.Mess_Id_Equal,
                                     "<"              => CMP.Mess_Id_Minor,
                                     ">"              => CMP.Mess_Id_Major,
                                     Key_To_String    => IM.Mess_Id_Image,
                                     Value_To_String  => IM.Destinations_Image);

    package NP_Sender_Buffering is new
                            Ordered_Maps_G (Key_Type         => A_C.Time,
                                            Value_Type       => CM.Value_T,
                                            "="              => A_C."=",
                                            "<"              => A_C."<",
                                            ">"              => A_C.">",
                                            Key_To_String    => IM.Time_Image,
                                            Value_To_String  => IM.Value_Image);
                                          
    package Neighbors        is new Maps_Protector_G (NP_Neighbors);
    package Latest_Msgs      is new Maps_Protector_G (NP_Latest_Msgs);
    package Sender_Dests     is new Ordered_Maps_Protector_G (NP_Sender_Dests);    
    package Sender_Buffering is new Ordered_Maps_Protector_G (NP_Sender_Buffering);
        

    
    ----------------------------------------------------------------------------        
    --                        Funciones y procedimientos                      --
    ----------------------------------------------------------------------------
    
    function Get_Destinations return CM.Destinations_T;
    procedure Timed_Handler (Time: in A_C.Time);
    procedure Reception_Handler (From        : in LLU.End_Point_Type; 
                                          To            : in LLU.End_Point_Type;
                                          P_Buffer    : access LLU.Buffer_Type);
    procedure Ctrl_C_Handler;
    procedure Free is new Ada.Unchecked_Deallocation (LLU.Buffer_Type, 
                                                      CM.Buffer_A_T);
                                                                      
    ----------------------------------------------------------------------------
    --                                 Variables                              --
    ----------------------------------------------------------------------------

    -- Mapas
    Neighbors_Map      : Neighbors.Prot_Map;
    Ltst_Msgs_Map      : Latest_Msgs.Prot_Map;
    Sndr_Dests_Map     : Sender_Dests.Prot_Map;
    Sndr_Buffering_Map : Sender_Buffering.Prot_Map;
   
    -- Arrays
    NB_Key_Array  : Neighbors.Keys_Array_Type;
    NB_Val_Array  : Neighbors.Values_Array_Type;
    LM_Key_Array  : Latest_Msgs.Keys_Array_Type;
    LM_Val_Array  : Latest_Msgs.Values_Array_Type;
    SN_Nodes_List : Latest_Msgs.Keys_Array_Type;
    
end Chat_Handler;
