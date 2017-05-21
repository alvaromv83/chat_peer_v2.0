-- Cuerpo de Chat_Messages (ver especificación en chat_messages.ads)
-- Autor: Álvaro Moles Vinader

package body Chat_Messages is

    -- Función para incrementar el número de secuencia
    function Seq_N_Increased (N: Seq_N_T) return Seq_N_T is
    begin
        return N + 1;
    end Seq_N_Increased;
    
end Chat_Messages;
