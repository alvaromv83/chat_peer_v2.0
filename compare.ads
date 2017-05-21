--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Contiene funciones de comparación de dos elementos del tipo Mess_Id        --
-- para el programa Chat-Peer v2.0                                            --
--                                                                            --                            
-- Autor: Álvaro Moles Vinader                                                --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Lower_Layer_Udp;

with Chat_Messages;
with Image;

package Compare is

    package LLU renames Lower_Layer_UDP;
    
    package CM renames Chat_Messages;
    package IM renames Image;
    
    use type LLU.End_Point_Type;
    use type CM.Seq_N_T;

    function Mess_Id_Equal (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean;
    function Mess_Id_Minor (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean;
    function Mess_Id_Major (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean;

end Compare;
