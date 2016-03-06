A Heap
------

Hi.  I needed a heap I could use with Lambdanative, to support a pathfinding library. 

This is that heap.  It is simple to use: 

    make-heap*, heap-add!, heap-pop!

You can provide your own comparison function.  `make-heap*` takes a single argument, the `<` operation for the heap.  You add items using `heap-add!` and remove them using `heap-pop!`.

When the heap is empty, that function returns a unique value which is true under `heap-null-value?` (friends don't let friends pun #f).

