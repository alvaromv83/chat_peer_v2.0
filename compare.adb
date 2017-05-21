-- Cuerpo de Compare (ver especificación en compare.ads)
-- Autor: Álvaro Moles Vinader

package body Compare is

	-- Función que devuelve True si el 1º elemento Mess_Id es IGUAL que el 2º
	function Mess_Id_Equal (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean is
		Equal : Boolean := False;
	begin
		if M1.EP_H_Creat = M2.EP_H_Creat and M1.Seq_N = M2.Seq_N then
			Equal := True;
		end if;
		return Equal;
	end Mess_Id_Equal;

	-- Función que devuelve True si el 1º elemento Mess_Id es MENOR que el 2º
	function Mess_Id_Minor (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean is
		Minor : Boolean := False;
	begin
		if IM.EP_Image(M1.EP_H_Creat) < IM.EP_Image(M2.EP_H_Creat) then
			Minor := True;
		elsif IM.EP_Image(M1.EP_H_Creat) = IM.EP_Image(M2.EP_H_Creat) then
			if M1.Seq_N < M2.Seq_N then
				Minor := True;
			end if;
		end if;
		
		return Minor;
	end Mess_Id_Minor;

	-- Función que devuelve True si el 1º elemento Mess_Id es MAYOR que el 2º
	function Mess_Id_Major (M1: CM.Mess_Id_T; M2: CM.Mess_Id_T) return Boolean is
		Major : Boolean := False;
	begin
		if IM.EP_Image(M1.EP_H_Creat) > IM.EP_Image(M2.EP_H_Creat) then
			Major := True;
		elsif IM.EP_Image(M1.EP_H_Creat) = IM.EP_Image(M2.EP_H_Creat) then
			if M1.Seq_N > M2.Seq_N then
				Major := True;
			end if;
		end if;
		
		return Major;
	end Mess_Id_Major;
	
end Compare;
