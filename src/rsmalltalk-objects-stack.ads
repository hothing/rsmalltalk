package RSmalltalk.Objects.Stack is

   procedure Push         (self :  in out SStack; e : in PtrToObject);
   procedure PushCopy     (self :  in out SStack; e : in PtrToObject);
   procedure Pop          (self :  in out SStack; e : out PtrToObject);
   procedure Top          (self :  in out SStack; e : out PtrToObject);
   procedure Peek         (self :  in out SStack; e : out PtrToObject;
                           n : in IndexS);

   procedure Empty        (self :  in out SStack; dispose : Boolean);
   function isFull        (self :  in out SStack) return Boolean;
   function isEmpty       (self :  in out SStack) return Boolean;

   procedure Down         (self :  in out SStack; n : in IndexS);
   procedure Up           (self :  in out SStack; n : in IndexS);

private

end RSmalltalk.Objects.Stack;
