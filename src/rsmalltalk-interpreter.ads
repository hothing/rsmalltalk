with RSmalltalk.Objects; use RSmalltalk.Objects;
with RSmalltalk.Memory; use RSmalltalk.Memory;

with Ada.Unchecked_Conversion;

package RSmalltalk.Interpreter is

   type OpCodes is (OC_NOP,
                    OC_SMI_ADD,
                    OC_SMI_SUB,
                    OC_SMI_LESS,
                    OC_SMI_GREAT,
                    OC_SMI_LEQ,
                    OC_SMI_GEQ,
                    OC_SMI_NEQ,
                    OC_SMI_EQU,
                    OC_SMI_MUL,
                    OC_SMI_DIV,
                    OC_SMI_MOD,
                    OC_SMI_REM,
                    OC_SMI_QUO,
                    OC_SMI_AND,
                    OC_SMI_OR,
                    OC_SMI_XOR,
                    OC_SMI_SHF,

                    OC_NUMBER_TO_POINT,

                    OC_INT_ADD,
                    OC_INT_SUB,
                    OC_INT_LESS,
                    OC_INT_GREAT,
                    OC_INT_LEQ,
                    OC_INT_GEQ,
                    OC_INT_NEQ,
                    OC_INT_EQU,
                    OC_INT_MUL,
                    OC_INT_DIV,
                    OC_INT_MOD,
                    OC_INT_REM,
                    OC_INT_QUO,
                    OC_INT_AND,
                    OC_INT_OR,
                    OC_INT_XOR,
                    OC_INT_SHF,

                    OC_SMI_ASFLOAT,

                    OC_FLOAT_ADD,
                    OC_FLOAT_SUB,
                    OC_FLOAT_LESS,
                    OC_FLOAT_GREAT,
                    OC_FLOAT_LEQ,
                    OC_FLOAT_GEQ,
                    OC_FLOAT_NEQ,
                    OC_FLOAT_EQU,
                    OC_FLOAT_MUL,
                    OC_FLOAT_DIV,
                    OC_FLOAT_TRUNC,
                    OC_FLOAT_FRACT,
                    OC_FLOAT_EXP,
                    OC_FLOAT_POW,

                    OC_OBJ_AT,
                    OC_OBJ_ATPUT,
                    OC_OBJ_SIZE,

                    OC_STRING_AT,
                    OC_STRING_ATPUT,

                    OC_XSTREAM_NEXT,
                    OC_WSTREAM_PUT,
                    OC_XSTREAM_ATEND,

                    OC_CMETHOD_AT,
                    OC_CMETHOD_ATPUT,

                    OC_BEHAVIOR_NEW,
                    OC_BEHAVIOR_NEWVAL,

                    OC_OBJ_BECOME,
                    OC_OBJ_IVAR_AT,
                    OC_OBJ_IVAR_ATPUT,

                    OC_OBJ_HASH,
                    OC_SMI_ASOBJ,

                    OC_BEHAVIOR_SOME_INST,
                    OC_OBJ_NEXTINST,

                    OC_CMETHOD_NEW,
                    OC_BCONTEXT_VALUEVAL,
                    OC_BCONTEXT_VALUEARR,

                    OC_OBJ_PERFORM,
                    OC_OBJ_PERFORM_WITH,

                    OC_SEM_SIG,
                    OC_SEM_WAIT,

                    OC_PROC_RESUME,
                    OC_PROC_SUSPEND,

                    OC_BEHAVIOR_FLUSH,

                    OC_ISTATE_MOUSEPT,
                    OC_ISTATE_CURSORPUT,
                    OC_CURSOR_LINK,
                    OC_ISTATE_SEM,
                    OC_ISTATE_INTERVAL,
                    OC_ISTATE_INPUTW,

                    OC_BITBLT_COPY,

                    OC_SYSDICT_SNAPSHOT,

                    OC_TIME_SETSEC,
                    OC_TIME_SETMSEC,

                    OC_PSCHEDULER_SIG_AT,

                    OC_CURSOR_BECURSOR,

                    OC_DISPLAY_BEDISPLAY,

                    OC_CSCANNER_SCAN,

                    OC_BITBLT_DRAWLOOP,

                    OC_BYTEARRAY_REPLACE,

                    OC_OBJECT_EQA,

                    OC_OBJECT_CLASS,

                    OC_SYSDICT_CORE_LEFT,
                    OC_SYSDICT_QUIT_PRIM,
                    OC_SYSDICT_EXIT_DBG,
                    OC_SYSDICT_OOPS_LEFT,
                    OC_SYSDICT_SIGNAL
                   );
   for OpCodes'Size use 8;

   for OpCodes use (OC_NOP => 0,
                    OC_SMI_ADD   => 1,
                    OC_SMI_SUB   => 2,
                    OC_SMI_LESS  => 3,
                    OC_SMI_GREAT => 4,
                    OC_SMI_LEQ => 5,
                    OC_SMI_GEQ => 6,
                    OC_SMI_NEQ => 7,
                    OC_SMI_EQU => 8,
                    OC_SMI_MUL => 9,
                    OC_SMI_DIV => 10,
                    OC_SMI_MOD => 11,
                    OC_SMI_REM => 12,
                    OC_SMI_QUO => 13,
                    OC_SMI_AND => 14,
                    OC_SMI_OR => 15,
                    OC_SMI_XOR => 16,
                    OC_SMI_SHF => 17,

                    OC_NUMBER_TO_POINT => 18,

                    OC_INT_ADD   => 21,
                    OC_INT_SUB   => 22,
                    OC_INT_LESS  => 23,
                    OC_INT_GREAT => 24,
                    OC_INT_LEQ => 25,
                    OC_INT_GEQ => 26,
                    OC_INT_NEQ => 27,
                    OC_INT_EQU => 28,
                    OC_INT_MUL => 29,
                    OC_INT_DIV => 30,
                    OC_INT_MOD => 31,
                    OC_INT_REM => 32,
                    OC_INT_QUO => 33,
                    OC_INT_AND => 34,
                    OC_INT_OR  => 35,
                    OC_INT_XOR => 36,
                    OC_INT_SHF => 37,

                    OC_SMI_ASFLOAT => 40,

                    OC_FLOAT_ADD => 41,
                    OC_FLOAT_SUB => 42,
                    OC_FLOAT_LESS => 43,
                    OC_FLOAT_GREAT => 44,
                    OC_FLOAT_LEQ => 45,
                    OC_FLOAT_GEQ => 46,
                    OC_FLOAT_NEQ => 47,
                    OC_FLOAT_EQU => 48,
                    OC_FLOAT_MUL => 49,
                    OC_FLOAT_DIV => 50,
                    OC_FLOAT_TRUNC => 51,
                    OC_FLOAT_FRACT => 52,
                    OC_FLOAT_EXP => 53,
                    OC_FLOAT_POW => 54,

                    OC_OBJ_AT => 60,
                    OC_OBJ_ATPUT => 61,
                    OC_OBJ_SIZE => 62,

                    OC_STRING_AT => 63,
                    OC_STRING_ATPUT => 64,

                    OC_XSTREAM_NEXT => 65,
                    OC_WSTREAM_PUT => 66,
                    OC_XSTREAM_ATEND => 67,

                    OC_CMETHOD_AT => 68,
                    OC_CMETHOD_ATPUT => 69,

                    OC_BEHAVIOR_NEW => 70,
                    OC_BEHAVIOR_NEWVAL => 71,

                    OC_OBJ_BECOME => 72,
                    OC_OBJ_IVAR_AT => 73,
                    OC_OBJ_IVAR_ATPUT => 74,

                    OC_OBJ_HASH => 75,
                    OC_SMI_ASOBJ => 76,

                    OC_BEHAVIOR_SOME_INST => 77,
                    OC_OBJ_NEXTINST => 78,

                    OC_CMETHOD_NEW => 79,
                    OC_BCONTEXT_VALUEVAL => 80,
                    OC_BCONTEXT_VALUEARR => 81,

                    OC_OBJ_PERFORM => 82,
                    OC_OBJ_PERFORM_WITH => 83,

                    OC_SEM_SIG => 84,
                    OC_SEM_WAIT => 85,

                    OC_PROC_RESUME => 86,
                    OC_PROC_SUSPEND => 87,

                    OC_BEHAVIOR_FLUSH => 88,

                    OC_ISTATE_MOUSEPT => 89,
                    OC_ISTATE_CURSORPUT => 90,
                    OC_CURSOR_LINK => 91,
                    OC_ISTATE_SEM => 92,
                    OC_ISTATE_INTERVAL => 93,
                    OC_ISTATE_INPUTW => 94,

                    OC_BITBLT_COPY => 95,

                    OC_SYSDICT_SNAPSHOT => 97,

                    OC_TIME_SETSEC => 98,
                    OC_TIME_SETMSEC => 99,

                    OC_PSCHEDULER_SIG_AT => 100,

                    OC_CURSOR_BECURSOR => 101,

                    OC_DISPLAY_BEDISPLAY => 102,

                    OC_CSCANNER_SCAN => 103,

                    OC_BITBLT_DRAWLOOP => 104,

                    OC_BYTEARRAY_REPLACE => 105,

                    OC_OBJECT_EQA => 110,

                    OC_OBJECT_CLASS => 111,

                    OC_SYSDICT_CORE_LEFT => 112,
                    OC_SYSDICT_QUIT_PRIM => 113,
                    OC_SYSDICT_EXIT_DBG => 114,
                    OC_SYSDICT_OOPS_LEFT => 115,
                    OC_SYSDICT_SIGNAL => 116
                   );
   function OpCodeToByte is
     new Ada.Unchecked_Conversion ( OpCodes , Byte ) ;
   function ByteToOpCode is
     new Ada.Unchecked_Conversion ( Byte , OpCodes ) ;

   type Interpreter is record
      mem: STMemory;
   end record;

   procedure fetchByte(intp : in Interpreter);
   procedure doCycle(intp : in Interpreter);
   procedure interpret(intp : in Interpreter);
   procedure checkProcessSwitch(intp : in Interpreter);
   procedure dispatchOnThisBytecode(intp : in Interpreter);
   procedure execStackBytecode(intp : in Interpreter);
   procedure execReturnBytecode(intp : in Interpreter);
   procedure execSendBytecode(intp : in Interpreter);
   procedure execJumpBytecode(intp : in Interpreter);

   -- interpreters for Stack bytecodes
   procedure pushReceiverVariableBytecode(intp : in Interpreter);
   procedure pushTemporaryVariableBytecode(intp : in Interpreter);
   procedure pushLiteratConstantBytecode(intp : in Interpreter);
   procedure pushLiteralVariableBytecode(intp : in Interpreter);
   procedure storeAndPopReceiverVariableBytecode(intp : in Interpreter);
   procedure storeAndPopTemporaryVariabteBytecode(intp : in Interpreter);
   procedure pushReceiverBytecode(intp : in Interpreter);
   procedure pushConstantBytecode(intp : in Interpreter);
   procedure extendedPushBytecode(intp : in Interpreter);
   procedure extendedStoreBytecode(intp : in Interpreter);
   procedure extendedStoreAndPopBytecode(intp : in Interpreter);
   procedure popStackBytecode(intp : in Interpreter);
   procedure dupticateTopBytecode(intp : in Interpreter);
   procedure pushActiveContextBytecode(intp : in Interpreter);

   -- interpreters for Jump bytecodes
   procedure shortUnconditionalJump(intp : in Interpreter);
   procedure shortConditionalJump(intp : in Interpreter);
   procedure longUnconditionalJump(intp : in Interpreter);
   procedure longConditionalJump(intp : in Interpreter);

   -- interpreters for Send bytecodes
   procedure extendedSendBytecode(intp : in Interpreter);
   procedure sendSpecialSelectorBytecode(intp : in Interpreter);
   procedure sendLiteralSelectorBytecode(intp : in Interpreter);
   procedure sendSelector(intp : in Interpreter; selector : in Word; argumentCount : in Word);
   procedure sendSelectorToClass(intp : in Interpreter; classPointer : in Word);

   procedure singleExtendedSendBytecode(intp : in Interpreter);
   procedure doubleExtendedSendBytecode(intp : in Interpreter);

   procedure singleExtendedSuperBytecode(intp : in Interpreter);
   procedure doublelxtendedSuperBytecode(intp : in Interpreter);

   procedure findNewMethodlnClass(intp : in Interpreter; class : in Word);
   procedure initializeMethodCache(intp : in Interpreter);
   procedure executeNewMethod(intp : in Interpreter);
   procedure activateNewMethod(intp : in Interpreter);

   -- interpreters for Return bytecodes
   procedure returnBytecode(intp : in Interpreter);
   procedure simpleReturnValue(intp : in Interpreter; resultPointer : in Word; contextPointer: Word);
   procedure returnValue(intp : in Interpreter; resultPointer : in Word; contextPointer: Word);
   procedure returnToActiveContext(intp : in Interpreter; contextPointer: Word);
   procedure nilContextFields(intp : in Interpreter);

end RSmalltalk.Interpreter;
