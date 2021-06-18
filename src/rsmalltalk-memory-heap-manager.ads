package RSmalltalk.Memory.Heap.Manager is

   function getHeadOfFreeChunkList
     (mem     : T_Memory;
      size    : T_Word;
      segment : T_SegmentIndex)
      return T_Word;

end RSmalltalk.Memory.Heap.Manager;
