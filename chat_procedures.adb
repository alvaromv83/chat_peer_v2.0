-- Cuerpo de Chat_Procedures (ver especificación en chat_procedures.ads)
-- Autor: Álvaro Moles Vinader

package body Chat_Procedures is

    -------------- Procedimiento para añadir a Latest_Messages -----------------
    
    procedure Add_To_Latest_Msgs is
        Success : Boolean;
    begin
        -- Si NO se ha sobrepasado el tamaño máximo de la tabla Latest_Messages...
        if CH.Latest_Msgs.Map_Length (CH.Ltst_Msgs_Map) < 
                            CH.Latest_Msgs.Max_Length_Value (CH.Ltst_Msgs_Map) then
                                                                    
            CH.Latest_Msgs.Put (CH.Ltst_Msgs_Map, CM.My_EP_H, CM.My_Seq_N, Success);
            CH.LM_Key_Array := CH.Latest_Msgs.Get_Keys (CH.Ltst_Msgs_Map);
            CH.LM_Val_Array := CH.Latest_Msgs.Get_Values (CH.Ltst_Msgs_Map);
            if Success then
                DB.Put_Line("");
                DB.Put_Line ("Añadimos a latest_messages " & IM.EP_Image(CM.My_EP_H) 
                             & CM.Seq_N_T'Image(CM.My_Seq_N));
            end if;
                             
            -- Si se ha alcanzado el tamaño máximo de la tabla Latest_Messages...
            if CH.Latest_Msgs.Map_Length (CH.Ltst_Msgs_Map) =
                            CH. Latest_Msgs.Max_Length_Value (CH.Ltst_Msgs_Map) then
                 -- Mostramos mensaje en pantalla (no lo pide el enunciado)
                 T_IO.Put_Line ("Tabla de últimos mensajes completa");
             end if;
             
         -- Si SÍ se ha sobrepasado el tamaño máximo de la tabla Latest_Messages...
         else
             DB.Put_Line ("No se puede añadir a latest_messages porque la tabla "            
                          & "está completa", PNT.Rojo);  -- (Contemplado en el enunciado)
         end if;
    end Add_To_Latest_Msgs;

    -------------- Procedimiento para añadir a Sender_Buffering ----------------    
    
    procedure Add_To_Sender_Buffering (Time : A_C.Time) is
        Value : CM.Value_T := (CM.My_EP_H, CM.My_Seq_N, CM.P_Buffer_Main);
    begin                 
        CH.Sender_Buffering.Put(CH.Sndr_Buffering_Map, Time, Value);
        DB.Put_Line ("Añadimos a sender_buffering: Key   => " & 
                     IM.Time_Image(Time)); 
        DB.Put_Line ("                             Value => " & 
                     IM.Value_Image(Value));

        
    end Add_To_Sender_Buffering;
    
    ---------------- Procedimiento para añadir a Sender_Dests ------------------
    
    procedure Add_To_Sender_Dests is
        Mess_Id : CM.Mess_Id_T := (CM.My_EP_H, CM.My_Seq_N);
    begin
        CH.Sender_Dests.Put (CH.Sndr_Dests_Map, Mess_Id, CM.My_Dests);
        DB.Put_Line ("Añadimos a sender_dests: ");
        DB.Put_Line ("                     Key   => " & 
                     IM.Mess_Id_Image(Mess_Id)); 
        DB.Put_Line ("                     Value => " & 
                     IM.Destinations_Image (CM.My_Dests));
    end Add_To_Sender_Dests;

    --------- Procedimiento para el envío de mensajes por inundación -----------
    
    procedure Flood_Send_Msg is
        I: Positive := 1;
    begin
        -- Enviamos a todos los vecinos
        for I in 1..CH.Neighbors.Map_Length(CH.Neighbors_Map) loop
            if CH.NB_Key_Array(I) /= null then
                LLU.Send (CH.NB_Key_Array(I), CM.P_Buffer_Main);
                DB.Put_Line("        send to " & IM.EP_Image(CH.NB_Key_Array(I)));
            end if;
        end loop;

    end Flood_Send_Msg;
    
    
    ----------------------------------------------------------------------------        
    --                 Envío por inundación de mensaje INIT                   --
    ----------------------------------------------------------------------------

    procedure Flood_Send_Init is
        Time : A_C.Time;
    begin
        -- Incrementamos Seq_N
        CM.My_Seq_N := CM.Seq_N_Increased(CM.My_Seq_N);
        
        -- Añadimos a latest_messages
        Add_To_Latest_Msgs;
        
        -- Introducimos en el buffer
        CM.P_Buffer_Main := new LLU.Buffer_Type(1024);
        
        CM.Message_Type'Output      (CM.P_Buffer_Main, CM.Init);     -- Init
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Creat
        CM.Seq_N_T'Output           (CM.P_Buffer_Main, CM.My_Seq_N); -- Seq_N
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Rsnd
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_R);  -- EP_R
        ASU.Unbounded_String'Output (CM.P_Buffer_Main, CM.My_Nick);  -- Nick
        
        -- Construimos la variable time
        Time := A_C.Clock + CM.Resend_Time;            
        
        -- Añadimos a sender_buffering
        Add_To_Sender_Buffering (Time);
        
        -- Añadimos a sender_dests
        Add_To_Sender_Dests;

        -- Enviamos el mensaje
        DB.Put ("FLOOD Init ", PNT.Amarillo);
        DB.Put_Line (IM.EP_Image(CM.My_EP_H) & CM.Seq_N_T'Image(CM.My_Seq_N) & " " &
                     IM.EP_Image(CM.My_EP_H) & " ... " & ASU.To_String(CM.My_Nick));
        Flood_Send_Msg;
                           
        -- Programamos el reenvío del mensaje                                                    
        TH.Set_Timed_Handler (Time, CH.Timed_Handler'Access);

    end Flood_Send_Init;
        
        
    ----------------------------------------------------------------------------        
    --                Envío por inundación de mensaje CONFIRM                 --
    ----------------------------------------------------------------------------

    procedure Flood_Send_Confirm is
        Time : A_C.Time;
    begin            
        -- Incrementamos Seq_N
        CM.My_Seq_N := CM.Seq_N_Increased(CM.My_Seq_N);
            
        -- Añadimos a latest_messages
        Add_To_Latest_Msgs;
            
        -- Introducimos en el buffer
        CM.P_Buffer_Main := new LLU.Buffer_Type(1024);
                             
        CM.Message_Type'Output      (CM.P_Buffer_Main, CM.Confirm);  -- Confirm
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Creat
        CM.Seq_N_T'Output           (CM.P_Buffer_Main, CM.My_Seq_N); -- Seq_N
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Rsnd
        ASU.Unbounded_String'Output (CM.P_Buffer_Main, CM.My_Nick);  -- Nick
            
        -- Construimos la variable Time
        Time := A_C.Clock + CM.Resend_Time;                
        
        -- Añadimos a sender_buffering
        Add_To_Sender_Buffering (Time);            

        -- Añadimos a sender_dests
        Add_To_Sender_Dests;
            
        -- Enviamos el mensaje
        DB.Put ("FLOOD Confirm ", PNT.Amarillo);
        DB.Put_Line (IM.EP_Image(CM.My_EP_H) & CM.Seq_N_T'Image(CM.My_Seq_N) & " " 
                     & IM.EP_Image(CM.My_EP_H) & " ... " & 
                     ASU.To_String(CM.My_Nick));
        Flood_Send_Msg;

        -- Programamos el reenvío del mensaje
        TH.Set_Timed_Handler (Time, CH.Timed_Handler'Access);
        
    end Flood_Send_Confirm;
            
            
    ----------------------------------------------------------------------------        
    --                   Envío por inundación de mensaje WRITER               --
    ----------------------------------------------------------------------------

    procedure Flood_Send_Writer is
        Time : A_C.Time;
    begin                    
        -- Incrementamos Seq_N
        CM.My_Seq_N := CM.Seq_N_Increased(CM.My_Seq_N);
            
        -- Añadimos a latest_messages
        Add_To_Latest_Msgs;
            
        -- Introducimos en el buffer                 
        CM.P_Buffer_Main := new LLU.Buffer_Type(1024);                                    
                             
        CM.Message_Type'Output      (CM.P_Buffer_Main, CM.Writer);   -- Writer
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Creat
        CM.Seq_N_T'Output           (CM.P_Buffer_Main, CM.My_Seq_N); -- Seq_N
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);  -- EP_H_Rsnd
        ASU.Unbounded_String'Output (CM.P_Buffer_Main, CM.My_Nick);  -- Nick
        ASU.Unbounded_String'Output (CM.P_Buffer_Main, CM.My_Text);  -- Text
            
        -- Construimos la variable Time
        Time := A_C.Clock + CM.Resend_Time;            
        
        -- Añadimos a sender_buffering
        Add_To_Sender_Buffering (Time);    
                             
        -- Añadimos a sender_dests
        Add_To_Sender_Dests;        
            
        -- Enviamos el mensaje
        DB.Put ("FLOOD Writer ", PNT.Amarillo);
        DB.Put_Line (IM.EP_Image(CM.My_EP_H) & CM.Seq_N_T'Image(CM.My_Seq_N) & " " 
                     & IM.EP_Image(CM.My_EP_H) & " ... " & 
                     ASU.To_String(CM.My_Nick));
        Flood_Send_Msg;

        -- Programamos el reenvío del mensaje
        TH.Set_Timed_Handler (Time, CH.Timed_Handler'Access);

    end Flood_Send_Writer;
    

    ----------------------------------------------------------------------------        
    --                Envío por inundación de Mensaje LOGOUT                  --
    ----------------------------------------------------------------------------    
    
    procedure Flood_Send_Logout (Confirm_Sent : Boolean) is
        Time : A_C.Time;
        Finish : Boolean := False;
    begin    
        DB.Put_Line ("");    
        DB.Put_Line ("---------------- Iniciando de Protocolo de Salida... ----------------");
        DB.Put_Line ("");
                                            
        -- Incrementamos Seq_N
        CM.My_Seq_N := CM.Seq_N_Increased (CM.My_Seq_N);
            
        -- Introducimos en el buffer
        CM.P_Buffer_Main := new LLU.Buffer_Type(1024);                            
            
        CM.Message_Type'Output      (CM.P_Buffer_Main, CM.Logout);    -- Logout
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);   -- EP_H_Creat
        CM.Seq_N_T'Output           (CM.P_Buffer_Main, CM.My_Seq_N);  -- Seq_N
        LLU.End_Point_Type'Output   (CM.P_Buffer_Main, CM.My_EP_H);   -- EP_H_Rsnd
        ASU.Unbounded_String'Output (CM.P_Buffer_Main, CM.My_Nick);   -- Nick
        Boolean'Output              (CM.P_Buffer_Main, Confirm_Sent); -- Confirm_Sent
            
        -- Construimos la variable Time
        Time := A_C.Clock + CM.Resend_Time;            
        
        -- Añadimos a sender_buffering
        Add_To_Sender_Buffering (Time);    
                
        -- Añadimos a sender_dests
        Add_To_Sender_Dests;        
                         
        -- Enviamos el mensaje
        DB.Put ("FLOOD Logout ", PNT.Amarillo);
        DB.Put_Line (IM.EP_Image(CM.My_EP_H) & CM.Seq_N_T'Image(CM.My_Seq_N) & 
                     " " & IM.EP_Image(CM.My_EP_H) & " ... " & 
                     ASU.To_String(CM.My_Nick) & " " & Boolean'Image (Confirm_Sent));
                             
        Flood_Send_Msg;        

        -- Programamos el reenvío del mensaje
        TH.Set_Timed_Handler (Time, CH.Timed_Handler'Access);

        -- Mostramos mensaje en pantalla (no lo pide el enunciado)
        DB.Put_Line ("");
        T_IO.Put ("Finalizando el programa... ");
        DB.Put_Line ("");
        
        -- Esperamos con un delay para asegurarnos que todos los nodos han
        -- recibido el mensaje logout
        while not Finish loop        
            
            -- Si Sender Buffering aún no está vacío...
            if CH.Sender_Buffering.Map_Length (CH.Sndr_Buffering_Map) /= 0 then
                -- Esperamos un plazo de retransmisión
                delay CM.Resend_Time;
                
            -- Si Sender Buffering está vacío, finalizamos el programa
            else
                Finish := True;
            end if;
        end loop;
            
        -- P_Buffer_Main y P_Buffer_Handler están a null en este punto, por lo
        -- que no hay que liberar ninguna memoria.
        
        DB.Put_Line ("");
        DB.Put_Line ("----------------- Fin de Protocolo de Salida ------------------");
        DB.Put_Line ("");

    end Flood_Send_Logout;
    
end Chat_Procedures;
