-- Cuerpo de Chat_Handler (ver especificación en chat_handler.ads)
-- Autor: Álvaro Moles Vinader

package body Chat_Handler is

    ----------------------------------------------------------------------------        
    --            Funciones y procedimientos genéricos para Chat-Peer         --
    ----------------------------------------------------------------------------

    -- Función que devuelve el array Destinations inicial
    function Get_Destinations return CM.Destinations_T is
        Dests : CM.Destinations_T;
        I     : Positive := 1;
    begin
        for I in 1..Neighbors.Max_Length_Value (Neighbors_Map) loop
            if NB_Key_Array(I) /= null then
                Dests(I).EP_H_ACKer := NB_Key_Array(I);
            end if;
        end loop;
        return Dests;
    end Get_Destinations;
    
    -- Procedimiento para la captura de CTRL_C
    procedure Ctrl_C_Handler is
    begin
        raise Program_Error;
    end Ctrl_C_Handler;
    
    
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------        
    --                                                                        --
    --                       Manejador Temporizado                            --
    --                                                                        --
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    procedure Timed_Handler (Time: in A_C.Time) is
        Value   : CM.Value_T;
        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;
        
        New_Time : A_C.Time;
        
        Success     : Boolean;
        Empty_Dests : Boolean  := True;
        I           : Positive;
        Max_Retries : Natural  := 10;

    begin
    
        -- Obtenemos de Sender_Buffering el Value del mensaje a retransmitir
        Sender_Buffering.Get(Sndr_Buffering_Map, Time, Value, Success);
        Mess_Id := (Value.EP_H_Creat, Value.Seq_N);
        
        -- Eliminamos el elemento de Sender_Buffering 
        Sender_Buffering.Delete(Sndr_Buffering_Map, Time, Success);
        if Success then
            DB.Put_Line ("");
            DB.Put_Line ("Eliminamos de sender_buffering: Key   => " & 
                         IM.Time_Image(Time), PNT.Azul); 
            DB.Put_Line ("                                Value => " & 
                         IM.Value_Image(Value), PNT.Azul);
        end if;
                         
        -- Obtenemos de Sender_Dests el array Destinations del msg a retransmitir
        Sender_Dests.Get(Sndr_Dests_Map, Mess_Id, Dests, Success);
    
        -- Comprobamos si quedan vecinos por asentir...
        I := 1;
        while I <= Neighbors.Max_Length_Value (Neighbors_Map) and Empty_Dests loop
            if Dests(I).EP_H_ACKer /= null then
                Empty_Dests := False;
            else
                I := I + 1;
            end if;
        end loop;
        
        -------------------- NO quedan vecinos por asentir ---------------------
        
        if Empty_Dests then
        
            DB.Put ("MSG Asentido ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(Value.EP_H_Creat) & " " & 
                         CM.Seq_N_T'Image(Value.Seq_N), PNT.Azul);
        
            -- Borramos el mensaje de Sender_Dests
            Sender_Dests.Delete(Sndr_Dests_Map, Mess_Id, Success);
            if Success then
                DB.Put_Line ("    Eliminamos de sender_dests: ", PNT.Azul);
                DB.Put_Line ("                     Key   => " & 
                             IM.Mess_Id_Image(Mess_Id), PNT.Azul);
                DB.Put_Line ("                     Value => " & 
                             IM.Destinations_Image (Dests), PNT.Azul);
            end if;
            
            -- (Ya hemos eliminado de Sender_Buffering anteriormente)
            
            -- Liberamos la memoria apuntada por Value.P_Buffer
            Free (Value.P_Buffer);
            
        ------------------- SÍ quedan vecinos por asentir ----------------------
        
        else
        
            -- Reenviamos el mensaje a los vecinos que no hayan asentido
            DB.Put ("RESEND ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(Value.EP_H_Creat) & 
                         CM.Seq_N_T'Image(Value.Seq_N), PNT.Azul);
            for I in 1..Neighbors.Max_Length_Value (Neighbors_Map) loop
                if Dests(I).EP_H_ACKer /= null then
                
                    -- Si el número de reintentos es menor que Max_Retries...
                    if Dests(I).Retries < Max_Retries then                            
                        LLU.Send (Dests(I).EP_H_ACKer, Value.P_Buffer);
                                                                                                 
                        -- Incrementamos Retries
                        Dests(I).Retries := Dests(I).Retries + 1;

                        DB.Put_Line ("    resend to " & 
                                     IM.EP_Image(Dests(I).EP_H_ACKer) & " / Retries:" 
                                     & Natural'Image(Dests(I).Retries), PNT.Azul);
                        
                    -- Si el número de reintentos es mayor o igual que 10...                                        
                    else
                                                                                                        
                        -- Eliminamos el nodo del array Destinations, por lo que se da        
                        -- el mensaje como asentido. (BUG contemplado en el enunciado)
                        DB.Put_Line ("    Máximo número de reintentos alcanzado para " 
                                     & IM.EP_Image(Dests(I).EP_H_ACKer) & 
                                     ". Lo eliminamos de Destinations.", PNT.Rojo);                        
                        
                        Dests(I).EP_H_ACKer := null;
                        Dests(I).Retries    := 0;
                                        
                    end if;
                end if;
            end loop;
            
            -- Actualizamos Sender_Dests con los nuevos valores de Destinations
            Sender_Dests.Put (Sndr_Dests_Map, Mess_Id, Dests);

            -- Añadimos a sender_buffering con nueva hora de retransmisión            
            New_Time    := A_C.Clock + CM.Resend_Time;        
            Sender_Buffering.Put(Sndr_Buffering_Map, New_Time, Value);
            
            DB.Put_Line ("    Añadimos a sender_buffering: Key   => " & 
                         IM.Time_Image(New_Time), PNT.Azul); 
            DB.Put_Line ("                                 Value => " & 
                         IM.Value_Image(Value), PNT.Azul);
                
            -- Programamos nuevo reenvío del mensaje con nueva hora de retransmisión
            TH.Set_Timed_Handler (New_Time, Timed_Handler'Access);
            
        end if;
        
    end Timed_Handler;
    
    
    ----------------------------------------------------------------------------    
    ----------------------------------------------------------------------------
    --                                                                        --
    --     Funciones y procedimientos auxiliares al Manejador de Recepción    --
    --                                                                        --
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------

    --------------   Procedimiento para añadir a Latest_Messages ---------------
    
    procedure Add_To_Latest_Msgs (EP_H_Creat : LLU.End_Point_Type; 
                                  Seq_N      : CM.Seq_N_T) is
        Success : Boolean;
    begin
        -- Si NO se ha sobrepasado el tamaño máximo de la tabla Latest_Messages...
        if Latest_Msgs.Map_Length (Ltst_Msgs_Map) < Latest_Msgs.Max_Length_Value 
                                                                 (Ltst_Msgs_Map) then
            Latest_Msgs.Put (Ltst_Msgs_Map, EP_H_Creat, Seq_N, Success);
            LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
            LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
            if Success then
                DB.Put_Line ("    Añadimos a latest_messages " & IM.EP_Image(EP_H_Creat) 
                             & CM.Seq_N_T'Image(Seq_N), PNT.Azul_Claro);
            end if;
            -- Si se ha alcanzado el tamaño máximo de la tabla Latest_Messages...
            if Latest_Msgs.Map_Length (Ltst_Msgs_Map) = Latest_Msgs.Max_Length_Value 
                                                                    (Ltst_Msgs_Map) then
                 -- Mostramos mensaje en pantalla (no lo pide el enunciado)
                 T_IO.Put_Line ("Tabla de últimos mensajes completa");
             end if;
            
         -- Si SÍ se ha sobrepasado el tamaño máximo de la tabla Latest_Messages...
         else
             DB.Put_Line ("    No se puede añadir a latest_messages porque la tabla " 
                          & "está completa", PNT.Rojo); -- (Contemplado en el enunciado)
         end if;
    end Add_To_Latest_Msgs;

    ------------------ Procedimiento para añadir a Neighbors --------------------
    
    procedure Add_To_Neighbors (EP_H_Creat : LLU.End_Point_Type;
                                EP_H_Rsnd  : LLU.End_Point_Type) is
        Success : Boolean;
    begin
        if EP_H_Creat = EP_H_Rsnd then
            -- Si NO se ha sobrepasado el tamaño máximo de la tabla Neighbors...
            if Neighbors.Map_Length (Neighbors_Map) < Neighbors.Max_Length_Value 
                                                                     (Neighbors_Map) then
                Neighbors.Put (Neighbors_Map, EP_H_Creat, A_C.Clock, Success);
                NB_Key_Array := Neighbors.Get_Keys   (Neighbors_Map);
                NB_Val_Array := Neighbors.Get_Values (Neighbors_Map);
                CM.My_Dests  := Get_Destinations;
                if Success then
                     DB.Put_Line ("    Añadimos a neighbors " & IM.EP_Image(EP_H_Creat), 
                                  PNT.Azul_Claro);
                 end if;
                 
             -- Si SÍ se ha sobrepasado el tamaño máximo de la tabla Neighbors...
             else
                 DB.Put_Line ("    No se puede añadir a neighbors porque la tabla "
                              & "está completa", PNT.Rojo); -- (Contemplado en el enunciado)
             end if;
        end if;
    end Add_To_Neighbors;

    ------------------ Procedimiento para borrar de Neighbors ------------------
    
    procedure Delete_From_Neighbors (EP_H_Creat : LLU.End_Point_Type;
                                     EP_H_Rsnd  : LLU.End_Point_Type) is
        Success : Boolean;
    begin    
        if EP_H_Creat = EP_H_Rsnd then
            Neighbors.Delete (Neighbors_Map, EP_H_Creat, Success);
            NB_Key_Array := Neighbors.Get_Keys   (Neighbors_Map);
            NB_Val_Array := Neighbors.Get_Values (Neighbors_Map);
            CM.My_Dests  := Get_Destinations;
            if Success then
                 DB.Put_Line ("    Borramos de neighbors " & 
                              IM.EP_Image(EP_H_Creat), PNT.Azul_Claro);
             end if;
        end if;
    end Delete_From_Neighbors;                    
    
    --------------- Procedimiento para añadir a Sender_Buffering ---------------
    
    procedure Add_To_Sender_Buffering (Time  : A_C.Time; 
                                       Value : CM.Value_T) is
    begin
        Sender_Buffering.Put(Sndr_Buffering_Map, Time, Value);
        DB.Put_Line ("    Añadimos a sender_buffering: Key   => " & 
                     IM.Time_Image(Time), PNT.Azul_Claro); 
        DB.Put_Line ("                                 Value => " & 
                     IM.Value_Image(Value), PNT.Azul_Claro);
    end Add_To_Sender_Buffering;
    
    ----------------- Procedimiento para añadir a Sender_Dests -----------------
    
    procedure Add_To_Sender_Dests (Mess_Id : CM.Mess_Id_T;
                                   Dests   : CM.Destinations_T) is
    begin
        Sender_Dests.Put (Sndr_Dests_Map, Mess_Id, Dests);
        DB.Put_Line ("    Añadimos a sender_dests: ", PNT.Azul_Claro);
        DB.Put_Line ("                     Key   => " & 
                     IM.Mess_Id_Image(Mess_Id), PNT.Azul_Claro); 
        DB.Put_Line ("                     Value => " & 
                     IM.Destinations_Image (Dests), PNT.Azul_Claro);
    end Add_To_Sender_Dests;
    
    ----- Función que devuelve Destinations sin el nodo que reenvió el msg -----
    
    function Get_Flood_Dests (EP_H_Rsnd: LLU.End_Point_Type) 
                return CM.Destinations_T is
        Dests  : CM.Destinations_T;
        Finish : Boolean  := False;
        I      : Positive := 1;
    begin
        for I in 1..Neighbors.Max_Length_Value (Neighbors_Map) loop
            if NB_Key_Array(I) /= null and NB_Key_Array(I) /= EP_H_Rsnd then
                Dests(I).EP_H_ACKer := NB_Key_Array(I);
            end if;
        end loop;
        return Dests;
    end Get_Flood_Dests;
    
    ---------------- Función para comprobar si un nodo es vecino ---------------
    
    function Neighbor (EP: LLU.End_Point_Type) return Boolean is
        Dests    : CM.Destinations_T;
        I        : Positive  := 1;
        Finish   : Boolean   := False;
        Neighbor : Boolean   := False;
    begin
        while I <= Neighbors.Max_Length_Value (Neighbors_Map) and not Finish loop
            if NB_Key_Array(I) = EP then
                Neighbor := True;
                Finish   := True;
            else
                I := I + 1;
            end if;
        end loop;
        return Neighbor;
    end Neighbor;
    
    ---- Procedimiento para comprobar la temporalidad del mensaje recibido -----
    
    procedure Check_Msg_Time (Msg_Mode   : in CM.Message_Type;
                              EP_H_Creat : in LLU.End_Point_Type;
                              Seq_N      : in CM.Seq_N_T; 
                              Time_Msg   : out CM.Time_Msg_T) is
    Finish : Boolean  := False;
    Found  : Boolean  := False;
    I      : Positive := 1;
    begin
        
        while not Finish and I <= Latest_Msgs.Map_Length(Ltst_Msgs_Map) loop
        
            -- Si EP_H_Creat SÍ está en latest_messages...
            if EP_H_Creat = LM_Key_Array(I) then
                Finish := True;
                Found     := True;
        
                -- Evaluamos Seq_N del msg recibido:
                
                -- Si es inmediatamente consecutivo...
                if Seq_N = LM_Val_Array(I) + 1 then
                    Time_Msg := CM.New_Msg;                
                -- Si es menor o igual...
                elsif Seq_N <= LM_Val_Array(I) then
                    Time_Msg := CM.Past_Msg;                    
                -- Si es mayor en 2 o más unidades...
                else
                    Time_Msg := CM.Future_Msg;
                end if;

            end if;
            I := I + 1;
        end loop;
        
        -- Si EP_H_Creat NO está en latest_messages...
        if not Found then
        
            -- Si el mensaje es un Confirm...
            if Msg_Mode = CM.Confirm then
                -- Se considera futuro, ya que aún no hemos recibido el Init
                Time_Msg := CM.Future_Msg;
            -- Si el mensaje es un Logout...
            elsif Msg_Mode = CM.Logout then
                -- Se considera pasado para que sea ignorado (evita bucle infinito):
                Time_Msg := CM.Past_Msg;
            -- Cualquier otro tipo de mensaje...
            else
                Time_Msg := CM.New_Msg;
            end if;
        end if;

    end Check_Msg_Time;
    
    ------ Función para comprobar si debemos hacer reenvío por inundación ------
    
    function Should_Resend (EP_H_Creat : LLU.End_Point_Type; 
                            EP_H_Rsnd  : LLU.End_Point_Type) return Boolean is
        Resend : Boolean := False;
    begin
        
        for I in 1..Neighbors.Map_Length(Neighbors_Map) loop
            -- Debemos hacer reenvío siempre que haya algún vecino que no sea el 
            -- creador del mensaje, ni el que nos lo ha reenviado.
            if  NB_Key_Array(I) /= null 
            and NB_Key_Array(I) /= EP_H_Rsnd
            and NB_Key_Array(I) /= EP_H_Creat then                 
                Resend := True;
            end if;
        end loop;
        return Resend;
    end Should_Resend;
    
    --------- Función para comprobar si un nodo está en latest_messages --------    
    
    function Node_In_Lm (EP_H_Creat : LLU.End_Point_Type) return Boolean is
        Found : Boolean  := False;
        I     : Positive := 1;
    begin
        while I <= Latest_Msgs.Max_Length_Value (Ltst_Msgs_Map) and not Found loop
            if LM_Key_Array (I) = EP_H_Creat then
                Found := True;
            else
                I := I + 1;
            end if;
        end loop;
        return Found;
    end Node_In_Lm;

    ---- Función para comprobar si un nodo se encuentra en latest_messages -----    
    
    function Found_In_Lm (EP_H_Creat : LLU.End_Point_Type) return Boolean is
        Found : Boolean  := False;
        I     : Positive := 1;
    begin
        while I <= Latest_Msgs.Max_Length_Value (Ltst_Msgs_Map) and 
                not Found loop
            if LM_Key_Array(I) = EP_H_Creat then
                Found := True;
            else
                I := I + 1;
            end if;
        end loop;
        return Found;
    end;
    
    ---------- Procedimiento para reenviar mensajes por inundación -------------
    
    procedure Flood_Rsnd_Msg (EP_H_Creat : LLU.End_Point_Type; 
                              EP_H_Rsnd  : LLU.End_Point_Type) is
    I : Positive := 1;
    begin
        -- Reenviamos a los vecinos que no sean ni el creador del mensaje ni el
        -- que nos ha hecho el reenvío.
        for I in 1..Neighbors.Map_Length(Neighbors_Map) loop
            if  NB_Key_Array(I) /= null 
            and NB_Key_Array(I) /= EP_H_Rsnd 
            and NB_Key_Array(I) /= EP_H_Creat then                                                                    
                    DB.Put_Line ("        send to " & IM.EP_Image(NB_Key_Array(I)), 
                                 PNT.Azul_Claro);
                    LLU.Send (NB_Key_Array(I), CM.P_Buffer_Handler);
            end if;
        end loop;
        
    end Flood_Rsnd_Msg;
    
    ---------------------- Procedimiento para enviar Ack -----------------------
    
    procedure Send_Ack (EP_H_Creat: LLU.End_Point_Type; 
                        EP_H_Rsnd : LLU.End_Point_Type; 
                        Seq_N     : CM.Seq_N_T;
                        P_Buffer  : access LLU.Buffer_Type) is
    begin
            LLU.Reset (P_Buffer.All);
            
            CM.Message_Type'Output    (P_Buffer, CM.Ack);     -- Ack
            LLU.End_Point_Type'Output (P_Buffer, CM.My_EP_H); -- EP_H_ACKer
            LLU.End_Point_Type'Output (P_Buffer, EP_H_Creat); -- EP_H_Creat
            CM.Seq_N_T'Output         (P_Buffer, Seq_N);      -- Seq_N

            LLU.Send (EP_H_Rsnd, P_Buffer);

            DB.Put ("    SND Ack ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Rsnd), PNT.Azul_Claro);
            
    end Send_Ack;
    
    
    ----------------------------------------------------------------------------        
    --             Procedimiento para el Reenvío de Mensaje INIT              --
    ----------------------------------------------------------------------------
    procedure Flood_Rsnd_Init (EP_H_Creat : LLU.End_Point_Type; 
                               Seq_N      : CM.Seq_N_T; 
                               EP_R_Creat : LLU.End_Point_Type; 
                               Nick       : ASU.Unbounded_String; 
                               EP_H_Rsnd  : LLU.End_Point_Type) is 

        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;
        Time    : A_C.Time;
        Value   : CM.Value_T;      
                                          
    begin

        -- Si SÍ hay vecinos a los que reenviar...
        if Should_Resend (EP_H_Creat, EP_H_Rsnd) then
        
            -- Introducimos en el buffer
            CM.P_Buffer_Handler := new LLU.Buffer_Type(1024);                
                
            CM.Message_Type'Output      (CM.P_Buffer_Handler, CM.Init);    -- Init
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, EP_H_Creat); -- EP_H_Creat
            CM.Seq_N_T'Output           (CM.P_Buffer_Handler, Seq_N);      -- Seq_N
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, CM.My_EP_H); -- EP_H_Rsnd
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, EP_R_Creat); -- EP_R_Creat
            ASU.Unbounded_String'Output (CM.P_Buffer_Handler, Nick);          -- Nick
                
            -- Construimos las variables de Sender_buffering y Sender_Dests
            Mess_Id := (EP_H_Creat, Seq_N);
            Dests   := Get_Flood_Dests (EP_H_Rsnd);
            Value   := (EP_H_Creat, Seq_N, CM.P_Buffer_Handler);
            Time    := A_C.Clock + CM.Resend_Time;

            -- Añadimos a sender_buffering
            Add_To_Sender_Buffering (Time, Value);

            -- Añadimos a sender_dests (sin incluir el nodo que nos ha enviado)
            Add_To_Sender_Dests (Mess_Id, Dests);

            -- Enviamos el mensaje                                                                        
            DB.Put ("    FLOOD Init ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(CM.My_EP_H) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
            Flood_Rsnd_Msg (EP_H_Creat, EP_H_Rsnd);
            
            -- Programamos reenvío del mensaje
            TH.Set_Timed_Handler (Time, Timed_Handler'Access);
    
        -- Si NO hay vecinos a los que reenviar...
        else
            DB.Put ("    NOFLOOD Init ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
        end if;    
    
    end Flood_Rsnd_Init;
    
    
    ----------------------------------------------------------------------------        
    --             Procedimiento para el Reenvío de Mensaje CONFIRM           --
    ----------------------------------------------------------------------------
    procedure Flood_Rsnd_Confirm (EP_H_Creat : LLU.End_Point_Type; 
                                  Seq_N      : CM.Seq_N_T; 
                                  Nick       : ASU.Unbounded_String;
                                  EP_H_Rsnd  : LLU.End_Point_Type) is 

        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;
        Time    : A_C.Time;
        Value   : CM.Value_T;      

    begin            
    
        ----------------- SÍ hay vecinos a los que reenviar --------------------
        
        if Should_Resend (EP_H_Creat, EP_H_Rsnd) then
    
            -- Introducimos en el buffer                
            CM.P_Buffer_Handler := new LLU.Buffer_Type(1024);            
                        
            CM.Message_Type'Output      (CM.P_Buffer_Handler, CM.Confirm); -- Confirm
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, EP_H_Creat); -- EP_H_Creat
            CM.Seq_N_T'Output           (CM.P_Buffer_Handler, Seq_N);      -- Seq_N
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, CM.My_EP_H); -- EP_H_Rsnd
            ASU.Unbounded_String'Output (CM.P_Buffer_Handler, Nick);       -- Nick
                
            -- Construimos las variables de Sender_buffering y Sender_Dests
            Mess_Id := (EP_H_Creat, Seq_N);
            Dests   := Get_Flood_Dests (EP_H_Rsnd);
            Value   := (EP_H_Creat, Seq_N, CM.P_Buffer_Handler);
            Time    := A_C.Clock + CM.Resend_Time;                

            -- Añadimos a sender_buffering                
            Add_To_Sender_Buffering (Time, Value);

            -- Añadimos a sender_dests (sin incluir el nodo que nos ha enviado)
            Add_To_Sender_Dests (Mess_Id, Dests);
            
            -- Enviamos el mensaje
            DB.Put ("    FLOOD Confirm ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(CM.My_EP_H) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
            Flood_Rsnd_Msg (EP_H_Creat, EP_H_Rsnd);            

            -- Programamos reenvío del mensaje
            TH.Set_Timed_Handler (Time, Timed_Handler'Access);
            
        ------------------- NO hay vecinos a los que reenviar ------------------
        
        else
            DB.Put ("    NOFLOOD Confirm ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
        end if;            
    
    end Flood_Rsnd_Confirm;    
    

    ----------------------------------------------------------------------------        
    --              Procedimiento para el Reenvío de Mensaje LOGOUT           --
    ----------------------------------------------------------------------------    
    procedure Flood_Rsnd_Logout (EP_H_Creat   : LLU.End_Point_Type; 
                                 Seq_N        : CM.Seq_N_T;
                                 EP_H_Rsnd    : LLU.End_Point_Type;
                                 Nick         : ASU.Unbounded_String;
                                 Confirm_Sent : Boolean) is
                                            
        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;
        Time    : A_C.Time;
        Value   : CM.Value_T;
    
    begin

        ----------------- SÍ hay vecinos a los que reenviar --------------------
        
        if Should_Resend (EP_H_Creat, EP_H_Rsnd) then

            -- Introducimos en el buffer
            CM.P_Buffer_Handler := new LLU.Buffer_Type(1024);                
                    
            CM.Message_Type'Output      (CM.P_Buffer_Handler, CM.Logout);    -- Logout
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, EP_H_Creat);   -- EP_H_Creat
            CM.Seq_N_T'Output           (CM.P_Buffer_Handler, Seq_N);        -- Seq_N
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, CM.My_EP_H);   -- EP_H_Rsnd
            ASU.Unbounded_String'Output (CM.P_Buffer_Handler, Nick);         -- Nick
            Boolean'Output              (CM.P_Buffer_Handler, Confirm_Sent); -- Confirm_Sent
                
            -- Construimos las variables de Sender_buffering y Sender_Dests
            Mess_Id := (EP_H_Creat, Seq_N);
            Dests   := Get_Flood_Dests (EP_H_Rsnd);
            Value   := (EP_H_Creat, Seq_N, CM.P_Buffer_Handler);
            Time    := A_C.Clock + CM.Resend_Time;                

            -- Añadimos a sender_buffering                
            Add_To_Sender_Buffering (Time, Value);                

            -- Añadimos a sender_dests (sin incluir el nodo que nos ha enviado)
            Add_To_Sender_Dests (Mess_Id, Dests);
                    
            -- Enviamos el mensaje
            DB.Put ("    FLOOD Logout ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(CM.My_EP_H) & " ... " & ASU.To_String(Nick) & " " 
                         & Boolean'Image (Confirm_Sent), PNT.Azul_Claro);
            Flood_Rsnd_Msg (EP_H_Creat, EP_H_Rsnd);            
            
            -- Programamos reenvío del mensaje
            TH.Set_Timed_Handler (Time, Timed_Handler'Access);
            
        ------------------- NO hay vecinos a los que reenviar ------------------
        
        else
            DB.Put ("    NOFLOOD Logout ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
        end if;
                
    end Flood_Rsnd_Logout;
    
    
    ----------------------------------------------------------------------------       
    --             Procedimiento para el Reenvío de Mensaje WRITER            --
    ----------------------------------------------------------------------------
    procedure Flood_Rsnd_Writer (EP_H_Creat  : LLU.End_Point_Type; 
                                 Seq_N       : CM.Seq_N_T; 
                                 Nick        : ASU.Unbounded_String;
                                 EP_H_Rsnd   : LLU.End_Point_Type;
                                 Text        : ASU.Unbounded_String) is 

        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;
        Time    : A_C.Time;
        Value   : CM.Value_T;      

    begin            
    
        ------------------- SÍ hay vecinos a los que reenviar -------------------
        
        if Should_Resend (EP_H_Creat, EP_H_Rsnd) then
        
            -- Introducimos en el buffer
            CM.P_Buffer_Handler := new LLU.Buffer_Type(1024);                                
                                                       
            CM.Message_Type'Output      (CM.P_Buffer_Handler, CM.Writer);  -- Writer
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, EP_H_Creat); -- EP_H_Creat
            CM.Seq_N_T'Output           (CM.P_Buffer_Handler, Seq_N);      -- Seq_N
            LLU.End_Point_Type'Output   (CM.P_Buffer_Handler, CM.My_EP_H); -- EP_H_Rsnd
            ASU.Unbounded_String'Output (CM.P_Buffer_Handler, Nick);       -- Nick
            ASU.Unbounded_String'Output (CM.P_Buffer_Handler, Text);       -- Text

            -- Construimos las variables de Sender_buffering y Sender_Dests
            Mess_Id := (EP_H_Creat, Seq_N);
            Dests   := Get_Flood_Dests (EP_H_Rsnd);
            Value   := (EP_H_Creat, Seq_N, CM.P_Buffer_Handler);
            Time    := A_C.Clock + CM.Resend_Time;                

            -- Añadimos a sender_buffering                
            Add_To_Sender_Buffering (Time, Value);

            -- Añadimos a sender_dests (sin incluir el nodo que nos ha enviado)
            Add_To_Sender_Dests (Mess_Id, Dests);
            
            -- Enviamos el mensaje
            DB.Put ("    FLOOD Writer ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(CM.My_EP_H) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
            Flood_Rsnd_Msg (EP_H_Creat, EP_H_Rsnd);            

            -- Programamos reenvío del mensaje
            TH.Set_Timed_Handler (Time, Timed_Handler'Access);
            
        ------------------- NO hay vecinos a los que reenviar ------------------
        
        else
            DB.Put ("    NOFLOOD Writer ", PNT.Amarillo);
            DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                         IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                         PNT.Azul_Claro);
        end if;

    
    end Flood_Rsnd_Writer;
    

    ----------------------------------------------------------------------------    
    ----------------------------------------------------------------------------        
    --                                                                        --
    --                           Manejador de Recepción                       --
    --                                                                        --
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------    
    procedure Reception_Handler (From        : in LLU.End_Point_Type; 
                                 To          : in LLU.End_Point_Type;
                                 P_Buffer    : access LLU.Buffer_Type) is
        
        -- Variables para campos de los mensajes
        Msg_Mode     : CM.Message_Type;
        Seq_N        : CM.Seq_N_T;
        EP_H_Creat   : LLU.End_Point_Type;
        EP_H_Rsnd    : LLU.End_Point_Type;
        EP_H_ACKer   : LLU.End_Point_Type;
        EP_R_Creat   : LLU.End_Point_Type;
        Nick         : ASU.Unbounded_String;
        Text         : ASU.Unbounded_String;
        Confirm_Sent : Boolean;
        
        -- Variables para Ordered Maps
        Mess_Id : CM.Mess_Id_T;
        Dests   : CM.Destinations_T;        
        
        -- Otras variables
        Success     : Boolean;
        Found       : Boolean;
        First_Value : Boolean;
        I           : Positive;
        Time_Msg    : CM.Time_Msg_T;

    begin
        
        -- Extraemos del buffer el primer campo: Msg_Mode
        Msg_Mode := CM.Message_Type'Input (P_Buffer);
        
        
        ------------------------------------------------------------------------        
        --                       Recepción de mensaje INIT                    --
        ------------------------------------------------------------------------
        if Msg_Mode = CM.Init then
        
            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat  := LLU.End_Point_Type'Input   (P_Buffer);
            Seq_N       := CM.Seq_N_T'Input                (P_Buffer);
            EP_H_Rsnd   := LLU.End_Point_Type'Input   (P_Buffer);
            EP_R_Creat  := LLU.End_Point_Type'Input   (P_Buffer);
            Nick        := ASU.Unbounded_String'Input (P_Buffer);
            
            DB.Put_Line ("");
            DB.Put ("RCV Init ", PNT.Amarillo);
            DB.Put_Line ("EP_H_Creat: "     & IM.EP_Image(EP_H_Creat) & 
                         " / Seq:"          & CM.Seq_N_T'Image(Seq_N) &
                         " / EP_H_Resend: " & IM.EP_Image(EP_H_Rsnd) & 
                         " / Nick_Creat: "  & ASU.To_String(Nick), PNT.Azul_Claro);
            
            -- Comprobamos la temporalidad del mensaje (nuevo, pasado o futuro)
            Check_Msg_Time (Msg_Mode, EP_H_Creat, Seq_N, Time_Msg);
            
            --------------------------- Mensaje NUEVO -----------------------------
                                                                        
            if Time_Msg = CM.New_Msg then
            
                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);                

                -- Añadimos a latest_messages
                Add_To_Latest_Msgs (EP_H_Creat, Seq_N);            
                
                ------------ Nick igual al mío: envío de mensaje REJECT ------------
                
                if Nick = CM.My_Nick then    
        
                    -- Enviamos mensaje Reject al EP_R del nodo que creó el mensaje
                    LLU.Reset (P_Buffer.All);                    
                    
                    CM.Message_Type'Output      (P_Buffer, CM.Reject);
                    LLU.End_Point_Type'Output   (P_Buffer, CM.My_EP_H);
                    ASU.Unbounded_String'Output (P_Buffer, CM.My_Nick);
                
                    LLU.Send (EP_R_Creat, P_Buffer);

                    DB.Put ("    SND Reject ", PNT.Amarillo);
                    DB.Put_Line (IM.EP_Image(EP_H_Creat) & " " & ASU.To_String(Nick), 
                                 PNT.Azul_Claro);
                    
                    -- Reject no se envía de forma fiable, por lo que si se pierde en
                    -- el camino, el receptor no tiene en cuenta que el nick está 
                    -- cogido, lo que provoca que puedan haber nicknames duplicados
                    -- (BUG contemplado en el enunciado).
                    
                ------------------ Nick diferente al mío -----------------------
                
                else
                
                    -- Si es vecino lo añadimos a neighbors
                    Add_To_Neighbors (EP_H_Creat, EP_H_Rsnd);

                    -- Reenviamos mensaje INIT
                    Flood_Rsnd_Init (EP_H_Creat, Seq_N, EP_R_Creat, Nick, EP_H_Rsnd);
                                    
                end if;            

            ---------------------- Mensaje del PASADO --------------------------
            
            elsif Time_Msg = CM.Past_Msg then
                DB.Put_Line ("    (Past Message)", PNT.Azul_Claro);
            
                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje                    
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);

                -- Si es vecino lo añadimos a neighbors (podíamos haber recibido
                -- el msg Init previamente por reenvío de otro nodo)
                if Nick /= CM.My_Nick then
                    Add_To_Neighbors (EP_H_Creat, EP_H_Rsnd);
                end if;
                
                DB.Put ("    NOFLOOD Init ", PNT.Amarillo);
                DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                             IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                             PNT.Azul_Claro);
            
            ---------------------- Mensaje del FUTURO --------------------------

            -- INIT nunca va a ser del futuro, ya que seq_n siempre es 1. Por tanto
            -- no consideramos este paso.
                            
            end if;


        ------------------------------------------------------------------------        
        --                   Recepción de mensaje CONFIRM                     --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Confirm then

            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N      := CM.Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd  := LLU.End_Point_Type'Input (P_Buffer);
            Nick       := ASU.Unbounded_String'Input (P_Buffer);
            
            DB.Put_Line ("");
            DB.Put ("RCV Confirm ", PNT.Amarillo);
            DB.Put_Line ("EP_H_Creat: "     & IM.EP_Image(EP_H_Creat) & 
                         " / Seq:"            & CM.Seq_N_T'Image(Seq_N) &
                         " / EP_H_Resend: " & IM.EP_Image(EP_H_Rsnd) & 
                         " / Nick_Creat: "  & ASU.To_String(Nick), PNT.Azul_Claro);
                                 
            -- Comprobamos la temporalidad del mensaje (nuevo, pasado o futuro)
            Check_Msg_Time (Msg_Mode, EP_H_Creat, Seq_N, Time_Msg);
            
            --------------------------- Mensaje NUEVO --------------------------
            
            if Time_Msg = CM.New_Msg then

                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);
            
                -- Añadimos a latest_messages (actualizamos en caso de Seq_N mayor)
                Add_To_Latest_Msgs (EP_H_Creat, Seq_N);

                -- Reenviamos mensaje CONFIRM
                Flood_Rsnd_Confirm (EP_H_Creat, Seq_N, Nick, EP_H_Rsnd);
                
                -- Mostramos mensaje en pantalla
                DB.Put_Line ("");                           
                T_IO.Put_Line (ASU.To_String(Nick) & " ha entrado en el chat");

                -- Si se ha alcanzado el tamaño máximo de la tabla Neighbors...
                if Neighbors.Map_Length (Neighbors_Map) = Neighbors.Max_Length_Value 
                                                          (Neighbors_Map) then
                    -- Mostramos mensaje en pantalla (no lo pide el enunciado)
                    T_IO.Put_Line ("Tabla de vecinos completa");
                end if;

            ------------------------- Mensaje del PASADO -----------------------
            
            elsif Time_Msg = CM.Past_Msg then        
                DB.Put_Line ("    (Past Message)", PNT.Azul_Claro);
            
                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje                    
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);
                
                DB.Put ("    NOFLOOD Confirm ", PNT.Amarillo);
                DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                             IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick), 
                             PNT.Azul_Claro);
            
            ------------------------- Mensaje del FUTURO -----------------------
            
            elsif Time_Msg = CM.Future_Msg then
                DB.Put_Line ("    (Future Message)", PNT.Azul_Claro);

                -- Reenviamos mensaje CONFIRM
                Flood_Rsnd_Confirm (EP_H_Creat, Seq_N, Nick, EP_H_Rsnd);                        
            
            end if;
            
            
        ------------------------------------------------------------------------        
        --                    Recepción de mensaje WRITER                     --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Writer then
            
            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat  := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N       := CM.Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd   := LLU.End_Point_Type'Input (P_Buffer);
            Nick        := ASU.Unbounded_String'Input (P_Buffer);
            Text        := ASU.Unbounded_String'Input (P_Buffer);
            
            DB.Put_Line ("");
            DB.Put ("RCV Writer ", PNT.Amarillo);
            DB.Put_Line ("EP_H_Creat: "     & IM.EP_Image(EP_H_Creat) & 
                         " / Seq:"          & CM.Seq_N_T'Image(Seq_N) &
                         " / EP_H_Resend: " & IM.EP_Image(EP_H_Rsnd) & 
                         " / Nick_Creat: "  & ASU.To_String(Nick) &
                         " / Text: "        & ASU.To_String(Text), PNT.Azul_Claro);
                               
            -- Comprobamos la temporalidad del mensaje (nuevo, pasado o futuro)
            Check_Msg_Time (Msg_Mode, EP_H_Creat, Seq_N, Time_Msg);
            
            --------------------------- Mensaje NUEVO --------------------------
            
            if Time_Msg = CM.New_Msg then

                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);

                -- Añadimos a latest_messages (actualizamos en caso de Seq_N mayor)
                Add_To_Latest_Msgs (EP_H_Creat, Seq_N);
        
                -- Reenviamos mensaje WRITER
                Flood_Rsnd_Writer (EP_H_Creat, Seq_N, Nick, EP_H_Rsnd, Text);
            
                -- Mostramos nick y mensaje en pantalla
                DB.Put_Line ("");        
                T_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Text));
            
            ------------------------- Mensaje del PASADO -----------------------
            
            elsif Time_Msg = CM.Past_Msg then
                DB.Put_Line ("    (Past Message)", PNT.Azul_Claro);

                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje                    
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);
                    
                DB.Put ("    NOFLOOD Writer ", PNT.Amarillo);
                DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) 
                             & " " & IM.EP_Image(EP_H_Rsnd) & " ... " & 
                             ASU.To_String(Nick), PNT.Azul_Claro);

            ------------------------- Mensaje del FUTURO -----------------------
            
            elsif Time_Msg = CM.Future_Msg then
                DB.Put_Line ("    (Future Message)", PNT.Azul_Claro);
                
                -- Reenviamos mensaje WRITER
                Flood_Rsnd_Writer (EP_H_Creat, Seq_N, Nick, EP_H_Rsnd, Text);
                
            end if;
            
            
        ------------------------------------------------------------------------        
        --                       Recepción de mensaje LOGOUT                  --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Logout then

            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat   := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N        := CM.Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd    := LLU.End_Point_Type'Input (P_Buffer);
            Nick         := ASU.Unbounded_String'Input (P_Buffer);
            Confirm_Sent := Boolean'Input (P_Buffer);
            
            DB.Put_Line ("");
            DB.Put ("RCV Logout ", PNT.Amarillo);
            DB.Put_Line ("EP_H_Creat: "      & IM.EP_Image(EP_H_Creat) & 
                         " / Seq:"           & CM.Seq_N_T'Image(Seq_N) &
                         " / EP_H_Resend: "  & IM.EP_Image(EP_H_Rsnd) & 
                         " / Nick_Creat: "     & ASU.To_String(Nick) & 
                         " / Confirm_Sent: " & Boolean'Image (Confirm_Sent), 
                         PNT.Azul_Claro);                   

            -- Comprobamos la temporalidad del mensaje (nuevo, pasado o futuro)
            Check_Msg_Time (Msg_Mode, EP_H_Creat, Seq_N, Time_Msg);
                
            --------------------------- Mensaje NUEVO --------------------------
            
            if Time_Msg = CM.New_Msg then

                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);

                -- Reenviamos mensaje LOGOUT
                Flood_Rsnd_Logout (EP_H_Creat, Seq_N, EP_H_Rsnd, Nick, Confirm_Sent);
            
                -- Borramos de latest_msgs
                Latest_Msgs.Delete (Ltst_Msgs_Map, EP_H_Creat, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                if Success then
                    DB.Put_Line ("    Borramos de latest_messages " & 
                                 IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N), 
                                 PNT.Azul_Claro);
                end if;
                
                -- Si es vecino lo borramos de neighbors
                Delete_From_Neighbors (EP_H_Creat, EP_H_Rsnd);
                                 
                -- Si el Logout es por salida voluntaria...
                if Confirm_Sent then
        
                    -- Mostramos mensaje en pantalla
                    DB.Put_Line ("");        
                    T_IO.Put_Line (ASU.To_String(Nick) & " ha abandonado el chat");
                    
                end if;
                
            ------------------------- Mensaje del PASADO -----------------------
                            
            elsif Time_Msg = CM.Past_Msg then
                DB.Put_Line ("    (Past Message)", PNT.Azul_Claro);

                -- Enviamos mensaje ACK al nodo que nos ha enviado el mensaje                    
                Send_Ack (EP_H_Creat, EP_H_Rsnd, Seq_N, P_Buffer);
                
                -- Si el nodo SÍ está en latest_msgs, lo borramos
                if Node_In_Lm (EP_H_Creat) then
                    Latest_Msgs.Delete (Ltst_Msgs_Map, EP_H_Creat, Success);
                    LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                    LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                    if Success then
                        DB.Put_Line ("    Borramos de latest_messages " & 
                                     IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N), 
                                     PNT.Azul_Claro);
                    end if;    

                -- Si el nodo NO está en latest_msgs, lo ignoramos
                -- (contemplado en el enunciado)                               
                else
                    DB.Put_Line ("    Mensaje Logout ignorado porque " 
                                 & IM.EP_Image(EP_H_Creat) & 
                                 " no se encuentra en latest_messages.", PNT.Rojo);
                end if;
                
                -- Si es vecino lo borramos de neighbors (podíamos haber recibido
                -- el msg Logout previamente por reenvío de otro nodo)
                if Neighbor (EP_H_Creat) then
                    Delete_From_Neighbors (EP_H_Creat, EP_H_Rsnd);
                end if;
            
                DB.Put ("    NOFLOOD Logout ", PNT.Amarillo);
                DB.Put_Line (IM.EP_Image(EP_H_Creat) & CM.Seq_N_T'Image(Seq_N) & " " &
                             IM.EP_Image(EP_H_Rsnd) & " ... " & ASU.To_String(Nick) & " " 
                             & Boolean'Image (Confirm_Sent), PNT.Azul_Claro);            
                
            ------------------------ Mensaje del FUTURO ------------------------
            
            elsif Time_Msg = CM.Future_Msg then
                DB.Put_Line ("    (Future Message)", PNT.Azul_Claro);
                
                -- Reenviamos mensaje LOGOUT
                Flood_Rsnd_Logout (EP_H_Creat, Seq_N, EP_H_Rsnd, Nick, Confirm_Sent);
    
            end if;
            
            
        ------------------------------------------------------------------------        
        --                       Recepción de mensaje ACK                     --
        ------------------------------------------------------------------------            
        elsif Msg_Mode = CM.Ack then

            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_ACKer  := LLU.End_Point_Type'Input (P_Buffer);
            EP_H_Creat  := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N       := CM.Seq_N_T'Input (P_Buffer);
        
            DB.Put_Line ("");
            DB.Put ("RCV Ack ", PNT.Amarillo);
            DB.Put_Line ("from "    & IM.EP_Image(EP_H_ACKer) & 
                         " / msg: " & IM.EP_Image(EP_H_Creat) & " " &
                         CM.Seq_N_T'Image(Seq_N), PNT.Azul_Claro);
            
            -- Obtenemos el array Destinations para el mensaje asentido
            Mess_Id := (EP_H_Creat, Seq_N);
            Sender_Dests.Get(Sndr_Dests_Map, Mess_Id, Dests, Success);
            
            -- Si el mensaje SÍ estaba en Sender Dests...
            if Success then
            
                -- Buscamos el nodo en el array Destinations del mensaje        
                I      := 1;
                Found  := False;
                while I  <= Neighbors.Max_Length_Value (Neighbors_Map) and 
                        not Found loop
                    if Dests(I).EP_H_ACKer = EP_H_ACKer then
                        Found := True;
                        Dests(I).EP_H_ACKer := null;
                        Dests(I).Retries    := 0;
            
                        -- Actualizamos Sender_Dests con el nuevo array Destinations
                        Sender_Dests.Put(Sndr_Dests_Map, Mess_Id, Dests);
                        DB.Put_Line("    Eliminamos " & IM.EP_Image(EP_H_ACKer) & 
                                    " como Destination del msg " & IM.EP_Image(EP_H_Creat) & 
                                    " " & CM.Seq_N_T'Image(Seq_N), PNT.Azul_Claro);                            
                    else
                        I := I + 1;                        
                    end if;
                end loop;
                
                -- Si el nodo NO estaba en Destinations...                    
                if not Found then
                    -- El Ack de este nodo ya ha sido recibido anteriormente, 
                    -- por tanto lo ignoramos
                    DB.Put_Line("    (Past Ack)", PNT.Azul_Claro);
                end if;
        
            -- Si el mensaje NO estaba en Sender Dests, ya ha sido asentido
            -- anteriormente, por tanto ignoramos el Ack...
            else
                DB.Put_Line("    (Past Ack)", PNT.Azul_Claro);
            end if;
            
            
        ------------------------------------------------------------------------        
        --                   Recepción de mensaje SN_Query                    --
        ------------------------------------------------------------------------            
        elsif Msg_Mode = CM.SN_Query then
        
            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat  := LLU.End_Point_Type'Input (P_Buffer);  -- EP_H_Creat
            EP_R_Creat  := LLU.End_Point_Type'Input (P_Buffer);  -- EP_R_Creat

            DB.Put_Line ("");
            DB.Put ("RCV SN_Query ", PNT.Amarillo);
            DB.Put_Line ("from " & IM.EP_Image(EP_H_Creat), PNT.Azul_Claro);

            
            --------------------- Envío de mensaje SN_Response -----------------
            
            LLU.Reset(P_Buffer.All);
                    
            CM.Message_Type'Output             (P_Buffer, CM.SN_Response); -- SN_Query
            Latest_Msgs.Keys_Array_Type'Output (P_Buffer, LM_Key_Array);   -- SN_Nodes_List
            LLU.End_Point_Type'Output          (P_Buffer, CM.My_EP_H);     -- EP_H_Creat
                
            LLU.Send (EP_R_Creat, P_Buffer);

            DB.Put ("    SND SN_Response ", PNT.Amarillo);
            DB.Put ("to " & IM.EP_Image(EP_H_Creat) & " / SN_Nodes_List: ", 
                    PNT.Azul_Claro);
                             
            -- Mensaje de depuración
            I := 1;
            First_Value := True;
            for I in 1..Latest_Msgs.Max_Length_Value (Ltst_Msgs_Map) loop
                if LM_Key_Array (I) /= null then
                    -- Si es el primer valor del array NO mostramos espacios
                    if First_Value then
                        DB.Put_Line (IM.EP_Image(LM_Key_Array(I)), PNT.Azul_Claro);
                        First_Value := False;
                    -- Si no es el primer valor del array SÍ mostramos espacios
                    else
                        DB.Put_Line ("                                                       "
                                     & IM.EP_Image(LM_Key_Array(I)), PNT.Azul_Claro);
                    end if;
                end if;
            end loop;
            
        end if;
        
    end Reception_Handler;
    
end Chat_Handler;
