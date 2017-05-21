--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                                                                            --
--                             C h a t - P e e r                              --    
--                                   v 2.0                                    --    
--                                                                            --
-- Ofrece un servicio de chat entre usuarios utilizando el modelo P2P         --
-- descentralizado                                                            --                --                                                                            --
-- Mejora la versión v1.0 de Chat-Peer para tolerar las pérdidas y            --
-- desorden de los mensajes que intercambian los nodos.                       --
-- Opciones añadidas:                                                         --   
--     - Finalizar programa con CTRL+C                                        --
--     - Supernodo                                                            --
--                                                                            --
-- Autor: Álvaro Moles Vinader                                                --
--                                                                            --
-- Asignatura "Informática II", Grado en Ingeniería en Sistemas Audiovisuales --
-- y Multimedia. Universidad Rey Juan Carlos. Madrid. Curso 2012-2013.        --
--                                                                            --            
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Gnat.Calendar.Time_IO;
with Gnat.Ctrl_C;
with Lower_Layer_UDP;
with Debug;
with Pantalla;
with Timed_Handlers;

with Chat_Handler;
with Chat_Messages;
with Chat_Procedures;
With Image;

procedure Chat_Peer_2 is
--------------------------------------------------------------------------------        
--                                  Declaraciones                             --
--------------------------------------------------------------------------------

    package T_IO    renames Ada.Text_IO;
    package ASU     renames Ada.Strings.Unbounded;
    package ASU_IO  renames Ada.Strings.Unbounded.Text_IO;
    package A_C     renames Ada.Calendar;
    package C_L     renames Ada.Command_Line;
    package C_IO    renames Gnat.Calendar.Time_IO;
    package LLU     renames Lower_Layer_UDP;
    package DB      renames Debug;
    package PNT     renames Pantalla;
    package TH      renames Timed_Handlers;
    
    package CH renames Chat_Handler;    
    package CM renames Chat_Messages;
    package CP renames Chat_Procedures;
    package IM renames Image;
    
    use type LLU.End_Point_Type;
    use type ASU.Unbounded_String;
    use type CM.Message_Type;
    use type A_C.Time;
    
    procedure Free is new Ada.Unchecked_Deallocation (LLU.Buffer_Type,
                                                      CM.Buffer_A_T);
    
    Usage_Error: exception;
    
    ----------------------------------------------------------------------------        
    --                                 Variables                              --
    ----------------------------------------------------------------------------
    
    -- Endpoints                    
    NB_1_EP : LLU.End_Point_Type := null;
    NB_2_EP : LLU.End_Point_Type := null;
    SN_EP   : LLU.End_Point_Type := null;
    
    -- Máquinas y puertos
    My_Port   : Integer;
    NB_1_Host : ASU.Unbounded_String := ASU.Null_Unbounded_String;
    NB_1_Port : Integer              := 0;
    NB_2_Host : ASU.Unbounded_String := ASU.Null_Unbounded_String;
    NB_2_Port : Integer              := 0;
    SN_Host   : ASU.Unbounded_String := ASU.Null_Unbounded_String;
    SN_Port   : Integer              := 0;    
    
    -- Retardo y pérdidas
    Min_Delay    : Natural;
    Max_Delay    : Natural;
    Fault_Pct    : Natural;

    -- Campos de mensajes que recibo
    Msg_Mode    : CM.Message_Type;
    EP_H_Creat  : LLU.End_Point_Type;
    Nick        : ASU.Unbounded_String;
    
    -- Otros
    Buffer      : aliased LLU.Buffer_Type (1024);
    Expired     : Boolean;
    Success     : Boolean;
    Wait_Time   : Duration;
    Prefix      : ASU.Unbounded_String;
    I           : Positive;
    
    -- Flags
    End_Program : Boolean := False;
    Finish      : Boolean;
    
    -- Variable para Debug. True: aparecen msgs de depuración por defecto.
    Status : Boolean := True;
    

begin
--------------------------------------------------------------------------------        
--                             Programa principal                             --
--------------------------------------------------------------------------------

    -- Establecemos Debug por defecto
    DB.Set_Status(Status);

    -- Establecemos que "CTRL-C" se ejecute en el manejador
    Gnat.Ctrl_C.Install_Handler (Chat_Handler.Ctrl_C_Handler'Access);
    
    
    ----------------------------------------------------------------------------        
    --                       Fase de Inicialización                           --
    ----------------------------------------------------------------------------

    -- Evaluamos los argumentos de la línea de comandos
    My_Port    := Integer'Value            (C_L.Argument(1));
    CM.My_Nick := ASU.To_Unbounded_String  (C_L.Argument(2));
    Min_Delay  := Natural'Value            (C_L.Argument(3));
    Max_Delay  := Natural'Value            (C_L.Argument(4));
    Fault_Pct  := Natural'Value            (C_L.Argument(5));

    -------------------- Inicialización sin vecinos iniciales ------------------
    
    if C_L.Argument_Count = 5 then
        DB.Put_Line ("NO hacemos protocolo de admisión porque no tenemos " & 
                         "contactos iniciales...");
    
    -------------------- Inicialización con un vecino inicial ------------------
    
    elsif C_L.Argument_Count = 7 then
        -- Evaluamos los argumentos de la línea de comandos
        NB_1_Host     := ASU.To_Unbounded_String (C_L.Argument(6));
        NB_1_Port     := Integer'Value           (C_L.Argument(7));

        -- Construímos el EP del vecino
        NB_1_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_1_Host)), NB_1_Port);
        
        -- Añadimos el vecino a la tabla neighbors
        CH.Neighbors.Put (CH.Neighbors_Map, NB_1_EP, A_C.Clock, Success);
        if Success then
            DB.Put_Line ("Añadimos a neighbors " & IM.EP_Image(NB_1_EP));
        end if;

    ----------------- Inicialización con dos vecinos iniciales ------------------
    
    elsif C_L.Argument_Count = 9 then
        -- Evaluamos los argumentos de la línea de comandos
        NB_1_Host     := ASU.To_Unbounded_String (C_L.Argument(6));
        NB_1_Port     := Integer'Value           (C_L.Argument(7));
        NB_2_Host     := ASU.To_Unbounded_String (C_L.Argument(8));
        NB_2_Port     := Integer'Value           (C_L.Argument(9));

        -- Construímos los EP de los vecinos
        NB_1_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_1_Host)), NB_1_Port);
        NB_2_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_2_Host)), NB_2_Port);
        
        -- Añadimos los vecinos a la tabla neighbors
        CH.Neighbors.Put (CH.Neighbors_Map, NB_1_EP, A_C.Clock, Success);
        if Success then
            DB.Put_Line ("Añadimos a neighbors " & IM.EP_Image(NB_1_EP));
        end if;
           
        CH.Neighbors.Put (CH.Neighbors_Map, NB_2_EP, A_C.Clock, Success);
        if Success then
            DB.Put_Line ("Añadimos a neighbors " & IM.EP_Image(NB_2_EP));
        end if;

    ------------ Inicialización con un vecino inicial y un supernodo ------------        
        
    elsif C_L.Argument_Count = 10 then
        -- Evaluamos los argumentos de la línea de comandos
        NB_1_Host   := ASU.To_Unbounded_String (C_L.Argument(6));
        NB_1_Port   := Integer'Value           (C_L.Argument(7));
        Prefix      := ASU.To_Unbounded_String (C_L.Argument(8));
        SN_Host     := ASU.To_Unbounded_String (C_L.Argument(9));
        SN_Port     := Integer'Value           (C_L.Argument(10));
        
        if Prefix = "-s" then
            -- Construímos los EP del vecino y el supernodo
            NB_1_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_1_Host)), NB_1_Port);
            SN_EP   := LLU.Build (LLU.To_IP(ASU.To_String(SN_Host)), SN_Port);
        
            -- Añadimos el vecino y el supernodo a la tabla neighbors
            CH.Neighbors.Put (CH.Neighbors_Map, NB_1_EP, A_C.Clock, Success);
            if Success then
                DB.Put_Line ("Añadimos a neighbors " & IM.EP_Image(NB_1_EP));
            end if;

            CH.Neighbors.Put (CH.Neighbors_Map, SN_EP, A_C.Clock, Success);
            if Success then
                DB.Put_Line ("Añadimos a neighbors " & IM.EP_Image(SN_EP));
            end if;

        else
            raise Usage_Error;
        end if;
        
    else
        raise Usage_Error;
    end if;

    ----------------------------------------------------------------------------

    -- Actualizamos los arrays de vecinos y Destinations
    CH.NB_Key_Array := CH.Neighbors.Get_Keys   (CH.Neighbors_Map);
    CH.NB_Val_Array := CH.Neighbors.Get_Values (CH.Neighbors_Map);
    CM.My_Dests     := CH.Get_Destinations;
    
    -- Construimos nuestros EP_R y EP_H y nos atamos
    LLU.Bind_Any (CM.My_EP_R);
    CM.My_EP_H := LLU.Build (LLU.To_IP(LLU.Get_Host_Name), My_Port);
    LLU.Bind (CM.My_EP_H, CH.Reception_Handler'Access); -- Recibimos mensajes en
                                                        -- el manejador de recepción
                                                        -- (el programa continúa)
    
    -- Establecemos valores de retardos, pérdidas y retransmisión
    LLU.Set_Faults_Percent           (Fault_Pct);
    LLU.Set_Random_Propagation_Delay (Min_Delay, Max_Delay);
    CM.Resend_Time                   := 2 * Duration(Max_Delay) / 1000;
    Wait_Time                        := 2.0;


    ----------------------------------------------------------------------------        
    --                         Protocolo de Admisión                          --
    ----------------------------------------------------------------------------
    
    -- Si tenemos vecinos iniciales...
    if C_L.Argument_Count /= 5 then
    
        DB.Put_Line("");    
        DB.Put_Line ("---------------- Iniciando de Protocolo de Admisión... ----------------");
        
        ------------------------- Inicio con supernodo -------------------------
        
        if C_L.Argument_Count = 10 then
        
            -- Enviamos petición de lista de vecinos al supernodo 
            LLU.Reset (Buffer);
        
            CM.Message_Type'Output    (Buffer'Access, CM.SN_Query); -- SN_Query
            LLU.End_Point_Type'Output (Buffer'Access, CM.My_EP_H);  -- EP_H_Creat
            LLU.End_Point_Type'Output (Buffer'Access, CM.My_EP_R);  -- EP_R_Creat
        
            LLU.Send (SN_EP, Buffer'Access);
            
            DB.Put_Line("");
            DB.Put ("SND SN_Query ", PNT.Amarillo);
            DB.Put_Line ("to " & IM.EP_Image(SN_EP));
            
            -- Esperamos la respuesta en My_EP_R
            LLU.Reset (Buffer);
            LLU.Receive (CM.My_EP_R, Buffer'Access, Wait_Time, Expired);
            
            -- Si no expira el plazo de espera...
            if not Expired then
                -- Extraemos los campos del mensaje
                Msg_Mode := CM.Message_Type'Input (Buffer'Access);
                if Msg_Mode = CM.SN_Response then
                    CH.SN_Nodes_List := CH.Latest_Msgs.Keys_Array_Type'Input (Buffer'Access);
                    EP_H_Creat       := LLU.End_Point_Type'Input             (Buffer'Access);
                end if;
                DB.Put ("RCV SN_Response ", PNT.Amarillo);
                DB.Put_Line ("from " & IM.EP_Image(EP_H_Creat));
            end if;
            
            -- Si nuestro vecino o yo estamos en la lista de nodos, los borramos
            I := 1;
            Finish := False;
            while I < CH.Latest_Msgs.Max_Length_Value (CH.Ltst_Msgs_Map) and 
                    not Finish loop
                if CH.SN_Nodes_List (I) = NB_1_EP then
                    CH.SN_Nodes_List (I) := null;
                    Finish := True;
                else
                    I := I + 1;
                end if;
            end loop;
            
            I := 1;
            Finish := False;
            while I < CH.Latest_Msgs.Max_Length_Value (CH.Ltst_Msgs_Map) and 
                    not Finish loop
                if CH.SN_Nodes_List (I) = CM.My_EP_H then
                    CH.SN_Nodes_List (I) := null;
                    Finish := True;
                else
                    I := I + 1;
                end if;
            end loop;
            
            -- Añadimos los nodos de la lista a neighbors
            for I in 1..CH.Latest_Msgs.Max_Length_Value (CH.Ltst_Msgs_Map) loop
                if CH.SN_Nodes_List (I) /= null then
                    CH.Neighbors.Put (CH.Neighbors_Map, CH.SN_Nodes_List (I),
                                      A_C.Clock, Success);
                    CH.NB_Key_Array := CH.Neighbors.Get_Keys   (CH.Neighbors_Map);
                    CH.NB_Val_Array := CH.Neighbors.Get_Values (CH.Neighbors_Map);
                    if Success then
                          DB.Put_Line ("    Añadimos a neighbors " 
                                       & IM.EP_Image(CH.SN_Nodes_List(I)));
                    end if;
                end if;
            end loop;
            
        end if;
        
        ------------------------------------------------------------------------
            
        -- Enviamos por inundación mensaje INIT
        CP.Flood_Send_Init;

        -- Mostramos mensaje en pantalla (no lo pide el enunciado)
        DB.Put_Line("");
        T_IO.Put ("Esperando confirmación... ");
        DB.Put_Line("");
        
        -- Esperamos la respuesta en My_EP_R
        LLU.Reset (Buffer);
        LLU.Receive (CM.My_EP_R, Buffer'Access, Wait_Time, Expired);
        
        -------------------------- Aceptado en el chat -------------------------
    
        if Expired then
        
            -- Mostramos mensaje en pantalla (no lo pide el enunciado)    
            DB.Put_Line("");
            T_IO.Put_Line ("Aceptado!");
            DB.Put_Line("");

            -- Enviamos por inundación mensaje CONFIRM
            CP.Flood_Send_Confirm;
        
        --------------------------- Rechazado del chat -------------------------    
        
        else
        
            -- Extraemos los campos del mensaje
            Msg_Mode     := CM.Message_Type'Input      (Buffer'Access);
            EP_H_Creat   := LLU.End_Point_Type'Input   (Buffer'Access);
            Nick         := ASU.Unbounded_String'Input (Buffer'Access);
            
            -- Si se recibe mensaje REJECT
            if Msg_Mode = CM.Reject then
                DB.Put_Line ("");
                DB.Put("RCV Reject ", PNT.Amarillo);
                DB.Put_Line (IM.EP_Image(EP_H_Creat) & " " & ASU.To_String(Nick));
                
                -- Mostramos mensaje en pantalla
                T_IO.New_Line;
                T_IO.Put_Line ("Lo siento, pero " & IM.EP_Image(EP_H_Creat) & 
                               " está usando el mismo nick. Prueba con uno diferente.");
                        
                -- Enviamos por inundación mensaje LOGOUT
                CP.Flood_Send_Logout (False);
            
                -- Finalizamos el programa
                TH.Finalize;
                LLU.Finalize;
                return;
            end if;
            
        end if;
        DB.Put_Line("");
        DB.Put_Line ("----------------- Fin de Protocolo de Admisión ------------------");
        
    end if;


    ----------------------------------------------------------------------------        
    --             Protocolo de envío y recepción de mensajes Writer          --
    ----------------------------------------------------------------------------

    -- Entramos en el chat; mostramos mensaje de bienvenida.
    T_IO.New_Line;
    T_IO.Put_Line ("Chat-Peer v2.0");
    T_IO.Put_Line ("==============");
    T_IO.New_Line;    
    T_IO.Put_Line ("Hola " & ASU.To_String(CM.My_Nick) & ", bienvenid@ al chat!");
    T_IO.Put_Line ("Escribe .h para obtener ayuda.");
    T_IO.New_Line;
    
    while not End_Program loop
    
        ------------------------------------------------------------------------        
        --                  Evaluación de texto en la entrada                 --
        ------------------------------------------------------------------------
        
        -- Leemos mensajes de la entrada
        CM.My_Text := ASU_IO.Get_Line;
            
        -------------------------------- Comandos ------------------------------
        
        -- Mostrar ayuda
        if CM.My_Text = ".h" or CM.My_Text = ".help" then
            T_IO.Put_Line("         Comandos                     Efectos");
            T_IO.Put_Line("         =================            =================");
            T_IO.Put_Line("         .nb      .neighbors          lista de vecinos");
            T_IO.Put_Line("         .lm      .latest_msgs        lista de últimos mensajes recibidos");
            T_IO.Put_Line("         .sd      .sender_dests       lista Sender_Dests");
            T_IO.Put_Line("         .sb      .sender_buffering   lista Sender_Buffering");
            T_IO.Put_Line("         .nb_l    .nb_length          tamaño actual de la lista de vecinos");
            T_IO.Put_Line("         .lm_l    .lm_length          tamaño actual de la lista de últimos mensajes recibidos");
            T_IO.Put_Line("         .nb_max  .nb_max_length      tamaño máximo de la lista de vecinos");
            T_IO.Put_Line("         .lm_max  .lm_max_length      tamaño máximo de la lista de últimos mensajes recibidos");
            T_IO.Put_Line("         .db      .debug              toggle para info de debug");
            T_IO.Put_Line("         .h       .help               muestra esta información de ayuda");
            T_IO.Put_Line("         .salir   CTRL+C              termina el programa");
        
        -- Activar / desactivar información de debug    
        elsif CM.My_Text = ".db" or CM.My_Text = ".debug" then
            Status := not Status;
            DB.Set_Status(Status);
            
            if Status then
                T_IO.Put_Line("Activada información de debug");
            else
                T_IO.Put_Line("Desactivada información de debug");
            end if;
        
        -- Mostrar la tabla neighbors
        elsif CM.My_Text = ".nb" or CM.My_Text = ".neighbors" then
            DB.Put_Line("");
           CH.Neighbors.Print_Map (CH.Neighbors_Map);
       
       -- Mostrar la tabla latest_messages        
        elsif CM.My_Text = ".lm" or CM.My_Text = ".latest_msgs" then
            DB.Put_Line("");
           CH.Latest_Msgs.Print_Map (CH.Ltst_Msgs_Map);

       -- Mostrar la tabla sender_dests    
        elsif CM.My_Text = ".sd" or CM.My_Text = ".sender_dests" then
            DB.Put_Line("");
           CH.Sender_Dests.Print_Map (CH.Sndr_Dests_Map);
           
       -- Mostrar la tabla sender_buffering    
        elsif CM.My_Text = ".sb" or CM.My_Text = ".sender_buffering" then
            DB.Put_Line("");
           CH.Sender_Buffering.Print_Map (CH.Sndr_Buffering_Map);

          -- Mostrar tamaño actual de la tabla neighbors
        elsif CM.My_Text = ".nb_l" or CM.My_Text = ".nb_length" then
            DB.Put_Line("");
           T_IO.Put_Line ("Tamaño actual de la tabla Neighbors:" & 
                               Natural'Image (CH.Neighbors.Map_Length 
                                                   (CH.Neighbors_Map)));
           
          -- Mostrar tamaño actual de la tabla latest_messages
        elsif CM.My_Text = ".lm_l" or CM.My_Text = ".lm_length" then
            DB.Put_Line("");
           T_IO.Put_Line ("Tamaño actual de la tabla Latest Messages:" & 
                               Natural'Image (CH.Latest_Msgs.Map_Length 
                                                   (CH.Ltst_Msgs_Map)));
                                                   
          -- Mostrar tamaño máximo de la tabla neighbors
        elsif CM.My_Text = ".nb_max" or CM.My_Text = ".nb_max_length" then
            DB.Put_Line("");
           T_IO.Put_Line ("Tamaño máximo de la tabla Neighbors:" & 
                               Natural'Image (CH.Neighbors.Max_Length_Value 
                                                   (CH.Neighbors_Map)));

          -- Mostrar tamaño máximo de la tabla latest_messages
        elsif CM.My_Text = ".lm_max" or CM.My_Text = ".lm_max_length" then
            DB.Put_Line("");
           T_IO.Put_Line ("Tamaño máximo de la tabla Latest Messages:" & 
                               Natural'Image (CH.Latest_Msgs.Max_Length_Value 
                                                   (CH.Ltst_Msgs_Map)));
        
        --------------------- Salida voluntaria del chat -----------------------
        
        elsif CM.My_Text = ".salir" then

            Finish := False;
            while not Finish loop
                -- Mostramos mensaje en pantalla
                T_IO.Put_Line ("¿Seguro que quieres salir del chat? SI / NO");
            
                -- Leemos respuesta
                CM.My_Text := ASU_IO.Get_Line;
            
                -- Respuesta: SI
                if CM.My_Text = "SI" or CM.My_Text = "Si" or 
                    CM.My_Text = "sI" or CM.My_Text = "si" then

                    if CH.Neighbors.Map_Length (CH.Neighbors_Map) > 0 then
                        -- Enviamos por inundación mensaje LOGOUT
                        CP.Flood_Send_Logout (True);
                    else
                        DB.Put_Line ("NO hacemos protocolo de salida porque no " &
                                         "tenemos vecinos...");
                    end if;

                    -- Mostramos mensaje en pantalla (no lo pide el enunciado)    
                    T_IO.Put_Line ("Hasta pronto!");
            
                    -- Finalizamos el programa
                    TH.Finalize;
                    LLU.Finalize;
                    End_Program := True;
                    Finish        := True;
                                    
                -- Respuesta: NO
                elsif CM.My_Text = "NO" or CM.My_Text = "No" or 
                        CM.My_Text = "nO" or CM.My_Text = "no" then
                    Finish := True;    
                                    
                -- Respuesta: otra
                else
                    T_IO.New_Line;
                    T_IO.Put_Line ("Por favor, contesta SI o NO.");
                end if;
            end loop;
        
        ------------------------------ Conversación ----------------------------
        
        else
            -- Si tenemos algún vecino...
            if CH.Neighbors.Map_Length (CH.Neighbors_Map) > 0 then        
                -- Enviamos por inundación mensaje WRITER
                CP.Flood_Send_Writer;
            -- Si NO tenemeos vecinos...
            else
                DB.Put_Line ("NO hacemos protocolo de envío porque no " &
                                  "tenemos vecinos...");
            end if;
        end if;

    end loop;
    
    
--------------------------------------------------------------------------------        
--                             Excepciones                                    --
--------------------------------------------------------------------------------
exception
    when Usage_Error =>
        T_IO.Put_Line ("Uso sin supernodo: ./chat_peer_2 port nickname min_delay max_delay" &
                       " fault_pct [[nb_host nb_port] [nb_host nb_port]]");
        T_IO.Put_Line ("Uso con supernodo: ./chat_peer_2 port nickname min_delay max_delay" &
                       " fault_pct [[nb_host nb_port] -s [sn_host sn_port]]");
        TH.Finalize;
        LLU.Finalize;
        
    when Program_Error =>
        T_IO.New_Line;

        -- Si tenemos algún vecino...            
        if CH.Neighbors.Map_Length (CH.Neighbors_Map) > 0 then
            -- Enviamos por inundación mensaje LOGOUT
            CP.Flood_Send_Logout (True);
        -- Si NO tenemeos vecinos...
        else
            DB.Put_Line ("NO hacemos protocolo de salida porque no " &
                         "tenemos vecinos...");
        end if;

        -- Mostramos mensaje en pantalla (no lo pide el enunciado)    
        T_IO.Put_Line ("Hasta pronto!");
            
        -- Finalizamos el programa
        TH.Finalize;
        LLU.Finalize;
            
    when Except:others =>
        T_IO.Put_Line ("Excepción imprevista: " &
                       Ada.Exceptions.Exception_Name(Except) & " en: " &
                       Ada.Exceptions.Exception_Message (Except));
        TH.Finalize;
        LLU.Finalize;
        
end Chat_Peer_2;
