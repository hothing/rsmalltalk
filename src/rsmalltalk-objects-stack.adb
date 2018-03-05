package body RSmalltalk.Objects.Stack is

   ------------
   -- isFull --
   ------------

   function isFull (self :  in out SStack) return Boolean is
   begin
      return self.top >= self.elem'Last;
   end isFull;

   -------------
   -- isEmpty --
   -------------

   function isEmpty (self :  in out SStack) return Boolean is
   begin
      return self.top <= self.elem'First;
   end isEmpty;

   ----------
   -- Down --
   ----------

   function DownCheck (self :  in out SStack; n : in IndexS) return Boolean is
   begin
      return self.top > n;
   end DownCheck;


   procedure Down (self :  in out SStack; n : in IndexS) is
   begin
      if self.top > n then
         self.top := self.top - n;
      else
         self.top := self.elem'First;
      end if;
   end Down;

   --------
   -- Up --
   --------

   function UpCheck (self :  in out SStack; n : in IndexS) return Boolean is
   begin
      return self.top < self.elem'Last - n;
   end UpCheck;

   procedure Up (self :  in out SStack; n : in IndexS) is
   begin
      if self.top < self.elem'Last - n then
         self.top := self.top + n;
      else
         self.top := self.elem'Last;
      end if;
   end Up;

   ----------
   -- Push --
   ----------

   procedure Push (self :  in out SStack; e : in PtrToObject) is
   begin
      -- check if stack isn't intialized and fix it
      if not DownCheck(self, 1) then Down(self, 1); end if;

      if UpCheck(self, 1) then
         Up(self, 1);
         self.elem(self.top) := e;
      end if;
   end Push;

   -----------------
   -- CopyAndPush --
   -----------------

   procedure PushCopy (self :  in out SStack; e : in PtrToObject) is
   begin
      -- new SObject and than copy
      Push(self, e);
   end PushCopy;

   ---------
   -- Pop --
   ---------

   procedure Pop (self :  in out SStack; e : out PtrToObject) is
   begin
      if DownCheck(self, 1) then
         e := self.elem(self.top);
         Down(self, 1);
      end if;
   end Pop;

   ---------
   -- Top --
   ---------

   procedure Top (self :  in out SStack; e : out PtrToObject) is
   begin
      if not isEmpty(self) then
         e := self.elem(self.top);
      end if;
   end Top;


   ----------
   -- Peek --
   ----------

   procedure Peek (self :  in out SStack;
                   e : out PtrToObject;
                   n : in IndexS) is
   begin
      if DownCheck(self, n) then
         e := self.elem(self.top);
      end if;
   end Peek;

   -----------
   -- Empty --
   -----------

   procedure Empty (self :  in out SStack; dispose : Boolean) is
   begin
      if dispose then
         null;
         self.top := IndexS'First;
      else
         self.top := IndexS'First;
      end if;
   end Empty;


end RSmalltalk.Objects.Stack;
