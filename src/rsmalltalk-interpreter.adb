-------------------------------------------------------------------------------
--                                                                           --
--                R S M A L L T A L K . I N T E R P R E T E R                --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
-------------------------------------------------------------------------------
with RSmalltalk.Objects.Stack; use RSmalltalk.Objects.Stack;

package body RSmalltalk.Interpreter is

   type SEResult is (Next, NewContext, Stop);

   type SubExecute is access procedure(ctx : PtrToContextCompiled;
                                       sr : out SEResult);

   type InstructionsMap is array (Byte range <>) of SubExecute;

   -------------
   -- Execute --
   -------------

   procedure Execute (tsk : in out ITask) is
      cmd   : Byte;
      arg1  : Byte;
      arg2  : Byte;
      -------
      ac    : PtrToContextCompiled;
      mc    : PtrToMethodC;
      op    : OpCodes;
   begin
      ac := PtrToContextCompiled(tsk.actx);
      mc := PtrToMethodC(ac.method);
      cmd := mc.codes(ac.codePointer);
      ac.codePointer := ac.codePointer  + 1;
      arg1 := mc.codes(ac.codePointer);
      ac.codePointer := ac.codePointer  + 1;
      arg2 := mc.codes(ac.codePointer);
      -- Convert CMD to OpCodes
      -- TODO
      op := ByteToOpCode(cmd);
      -- interpreter
      case op is
         when OC_NOP =>
            null;
         when OC_SMI_ADD =>
            null;
         when OC_SMI_SUB =>
            null;
         when OC_SMI_MUL =>
            null;
         when others =>
            null;
      end case;

   end Execute;

      -- Если использовать таблицу функций каждай из которых имеет доступ
   -- к контексту, можно намного упростить инетепретатор, при следующих
   -- условиях:
   --  * каждая процедура сама меняет codePointer
   --  * каждая процедура проверяет последующие инструкции, для определения
   --    последовательности схожих инструкций,
   --    чтобы можно было опимизировать выполнение.
   --  * каждая процедура выставялет флаг обработки, для смены контекста
      --    или останова интерпретации

   procedure Execute2 (tsk : in out ITask; im : InstructionsMap) is
      cmd   : Byte;
      -------
      ac    : PtrToContextCompiled;
      mc    : PtrToMethodC;
      er    : SEResult;
   begin
      ac := PtrToContextCompiled(tsk.actx);
      mc := PtrToMethodC(ac.method);
      cmd := mc.codes(ac.codePointer);
      er := Next;
      im(cmd)(ac, er);
      if (er = NewContext) then
         null;
      elsif (er = Stop) then
         null;
      end if;

   end Execute2;



   procedure Exec_Smi_Op(ctx : PtrToContextCompiled;
                         sr : out SEResult) is
      arg1, arg2 : PtrToObject;
      res : PtrToInteger;
   begin
      Pop(ctx.stack, arg1);
      Pop(ctx.stack, arg2);
      res := new SIntegerObject;
      res.val := PtrToInteger(arg1).val + PtrToInteger(arg2).val;
      Push(ctx.stack, PtrToObject(res));
      ctx.codePointer := ctx.codePointer + 1;
      sr := Next;
   end Exec_Smi_Op;


end RSmalltalk.Interpreter;
