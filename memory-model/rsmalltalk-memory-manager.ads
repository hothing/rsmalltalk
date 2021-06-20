package RSmalltalk.Memory.Manager is

   function allocate(mem: in out T_Memory; size : T_Word) return T_Pointer;
   
   procedure deallocate(mem: in out T_Memory; objectPtr : T_Pointer); 
   
   procedure free(mem: in out T_Memory; objectPtr : T_Pointer); 

end RSmalltalk.Memory.Manager;
