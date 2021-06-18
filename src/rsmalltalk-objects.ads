with Interfaces; use Interfaces;
with Ada.Strings;
with Ada.Containers;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package RSmalltalk.Objects is

   type IndexS is range 1 .. 2**15; -- small index
   for IndexS'Size use 16;

   subtype Word is Unsigned_16;
   subtype Byte is Unsigned_8;

   type ArrayOfWChar is array(IndexS range <>) of Wide_Character;

   type ArrayOfByte is array (IndexS range <>) of Byte;
   type PtrArrayOfByte is access all ArrayOfByte;

   ----------------------------------------------------

   type SClass;
   type SObject;

   type PtrToClass is access all SClass'Class;
   type PtrToObject is access all SObject'Class;

   type SObject is abstract tagged record
      class : PtrToClass;
   end record;

   type ArrayOfPtrClass is array(IndexS range <>) of PtrToClass;

   type ArrayOfPtrObject is array(IndexS range <>) of PtrToObject;

   type LongArrayOfPtrObject is array(Positive range <>) of PtrToObject;

   type PtrToArrayOfPtrObject is access all ArrayOfPtrObject;

   --------------------------------------------

   type AClass is abstract new SObject with null record;

   type SIntegerClass is new AClass with
     null record;

   type SFloatClass is new AClass with
     null record;

   type SCharClass is new AClass with
     null record;

   type SArrayClass is new AClass with
     null record;

   type SDictionaryClass is new AClass with
     null record;

   type SBlockClass is new AClass with
     null record;

   type SContext;
   type PtrToContext is access all SContext'Class;
   -----------------------------------------

   type AMethod is abstract new SObject with null record;

   type SMethodGetter(fid : IndexS) is new AMethod with null record;

   type SMethodReturnSelf is new AMethod with null record;

   type SMethodCompiled(llen :IndexS; clen :IndexS) is new AMethod with
      record
         literals     : ArrayOfPtrObject(1 .. llen);
         codes        : ArrayOfByte(1 .. clen);
      end record;

   type IMethodNative is access procedure(self : PtrToObject;
                                         ctx : PtrToContext);
   type SMethodInternal is new AMethod with
      record
         iproc        : IMethodNative;
      end record;

   type PtrToMethod is access all AMethod'Class;
   type PtrToMethodC is access all SMethodCompiled'Class;
   type PtrToMethodI is access all SMethodInternal'Class;

   subtype KeyStr is Ada.Strings.Unbounded.Unbounded_String;

   package MapSO is
        new Ada.Containers.Hashed_Maps
          (KeyStr,
           PtrToObject,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=");

   package MapSI is
        new Ada.Containers.Hashed_Maps
          (KeyStr,
           IndexS,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=");

   package MapSM is
        new Ada.Containers.Hashed_Maps
          (KeyStr,
           PtrToMethod,
           Ada.Strings.Unbounded.Hash,
           Ada.Strings.Unbounded."=");

   type SClass is new AClass with
      record
         method  : MapSM.Map;
         cfield  : MapSI.Map;
         ifield  : MapSI.Map;
      end record;
   --
   ---------------
   -- Instances --
   ---------------
   type ASimpleObject is abstract new SObject with null record;

   type SCharObject is
      new ASimpleObject with
      record
         char : Wide_Character;
      end record;
   type PtrToChar is access all SCharObject;

   type SIntegerObject is
     new ASimpleObject with
      record
         val : Integer;
      end record;
   type PtrToInteger is access all SIntegerObject'Class;

   type SFloatObject is
     new ASimpleObject with
      record
         val : Float;
      end record;
   type PtrToFloat is access all SFloatObject;

   type SArrayObject(len : Positive) is
     new SObject with
      record
         elem  : LongArrayOfPtrObject(1 .. len);
      end record;
   type PtrToArray is access all SArrayObject;

   type SStringObject(len : Positive) is
     new SObject with
      record
         str   : Wide_String(1 .. len);
      end record;
   type PtrToString is access all SStringObject;

   type SStructObject(len : IndexS) is
     new SObject with
      record
         field : ArrayOfPtrObject  (1 .. len);
      end record;
   type PtrToStruct is access all SStructObject;

   type SInternalObject is
     new SObject with null record;
   type PtrToInternal is access all SInternalObject;

   type SDictionaryObject is
     new SObject with
      record
         dict : MapSO.Map;
      end record;

   type SStack(size : IndexS)  is record
      top  : IndexS;
      elem : ArrayOfPtrObject(1 .. size);
   end record;

   type SContext(asize : IndexS; ssize : IndexS) is
     new SObject with
      record
         sender           : PtrToContext;
         arguments        : ArrayOfPtrObject(1 .. asize);
         stack            : SStack(ssize);
         codePointer      : IndexS;
      end record;

   type SContextCompiled(asize : IndexS; tsize : IndexS; ssize : IndexS) is
     new SContext(asize, ssize) with
      record
         reciever         : PtrToObject;
         method           : PtrToMethod;
         temporaries      : ArrayOfPtrObject(1 .. tsize);
      end record;

   type PtrToContextCompiled is access all SContextCompiled'Class;

   type SBlockContext(asize : IndexS; ssize : IndexS) is
     new SContext(asize, ssize) with
      record
         codeStart        : IndexS;
         home             : PtrToContext;
      end record;

   type PtrToBlockContext is access all SBlockContext'Class;

private

end RSmalltalk.Objects;
