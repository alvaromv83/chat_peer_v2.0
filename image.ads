--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Contiene funciones que devuelven un String dada una variable de            --
-- tipos declarados específicamente para Chat-Peer                            --
--                                                                            --
-- Autor: Álvaro Moles Vinader                                                --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Gnat.Calendar.Time_IO;
with Debug;
with Pantalla;

with Chat_Messages;

package Image is

    package A_C  renames Ada.Calendar;
    package ASU  renames Ada.Strings.Unbounded;
    package LLU  renames Lower_Layer_UDP;
    package C_IO renames Gnat.Calendar.Time_IO;
    package DB   renames Debug;
    package PNT  renames Pantalla;
    
    package CM renames Chat_Messages;
    
    use type LLU.End_Point_Type;
    use type ASU.Unbounded_String;
    use type CM.Seq_N_T;

    function EP_Image           (EP : LLU.End_Point_Type)  return String;    
    function Time_Image         (T  : Ada.Calendar.Time)   return String;
    function Mess_Id_Image      (M  : CM.Mess_Id_T)        return String;
    function Destinations_Image (D  : CM.Destinations_T)   return String;
    function Value_Image        (V  : CM.Value_T)          return String;
    function Duration_Image     (D  : Duration)            return String;

end Image;
