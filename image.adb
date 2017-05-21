-- Cuerpo de Image (ver especificación en image.ads)
-- Autor: Álvaro Moles Vinader

package body Image is

    -- Función que devuelve un String con la info básica de un EP
    function EP_Image (EP: LLU.End_Point_Type) return string is
        I       : Positive := 1;
        Index   : Natural := 0;
        EP_Tail : ASU.Unbounded_String := ASU.To_Unbounded_String (LLU.Image(EP));
        IP      : ASU.Unbounded_String;
        Cut_EP  : ASU.Unbounded_String;
    begin
        for I in 1..4 loop
            Index     := ASU.Index (EP_Tail, " ");
            EP_Tail   := ASU.Tail (EP_Tail, ASU.Length(EP_Tail)-Index);
            if I = 2 then
                Index := ASU.Index (EP_Tail, " ");
                IP    := ASU.Head(EP_Tail, ASU.Length(EP_Tail)-Index-2);
            elsif I = 4 then
                Index    := ASU.Index (EP_Tail, " ");
                EP_Tail  := ASU.Tail (EP_Tail, ASU.Length(EP_Tail)-Index);
                Cut_EP   := IP & ":" & EP_Tail;
            end if;
        end loop;
        return ASU.To_String(Cut_EP);
    end EP_Image;    
    
    -- Función que devuelve un String con la fecha y hora
    function Time_Image (T: Ada.Calendar.Time) return String is
    begin
        return "Time: " & C_IO.Image (T, "%c");
    end Time_Image;
    
    -- Función que devuelve un String de Mess_Id_T
    function Mess_Id_Image (M: CM.Mess_Id_T) return String is
    begin
        return "EP_H_Creat: " & EP_Image(M.EP_H_Creat) & 
               " / Seq:" & CM.Seq_N_T'Image(M.Seq_N);
    end Mess_Id_Image;
    
    -- Función que devuelve un String de Destinations_T
    function Destinations_Image (D: CM.Destinations_T) return String is
        D_String    : ASU.Unbounded_String;
        I           : Positive := 1;
        First_Value : Boolean  := True;
    begin
        for I in 1..10 loop
            if D(I).EP_H_ACKer /= null then
                -- Si es el primer valor, no añadimos espacios
                if First_Value then
                    D_String := ASU.To_Unbounded_String("EP_H_ACKer: ") &
                                ASU.To_Unbounded_String(EP_Image(D(I).EP_H_ACKer)) & 
                                ASU.To_Unbounded_String(" / Retries:") & 
                                ASU.To_Unbounded_String(Natural'Image(D(I).Retries) &
                                ASCII.LF);
                    First_Value := False;
                    
                -- Si no es el primer valor, añadimos espacios
                else
                    D_String := D_String & "                              EP_H_ACKer: " 
                                & ASU.To_Unbounded_String(EP_Image(D(I).EP_H_ACKer)) & 
                                ASU.To_Unbounded_String(" / Retries:") & 
                                ASU.To_Unbounded_String(Natural'Image(D(I).Retries) &
                                ASCII.LF);
                end if;
            end if;
        end loop;
        return ASU.To_String(D_String);
    end Destinations_Image;

    -- Función que devuelve un String de Value_T    
    function Value_Image (V: CM.Value_T) return String is
    begin
        return "EP_H_Creat: " & EP_Image(V.EP_H_Creat) & " / Seq:" & 
               CM.Seq_N_T'Image(V.Seq_N);
    end Value_Image;
    
    -- Función que devuelve un String de Duration sin decimales
    function Duration_Image (D  : Duration) return String is
        Index  : Natural := 0;
        D_Head : ASU.Unbounded_String := ASU.To_Unbounded_String 
                                                      (Duration'Image (D));
    begin
        Index    := ASU.Index (D_Head, ".");
        D_Head   := ASU.Head  (D_Head, Index-1);
        return ASU.To_String  (D_Head);
    end Duration_Image;
    
end Image;
