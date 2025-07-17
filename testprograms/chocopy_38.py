# max heap implementation

class Heap(object):
    heap: [int] = None

    def __init__(self: "Heap"):
        pass
    
    def heap_size(self: "Heap") -> int:
        return len(self.heap)

    def set_heap(self: "Heap", heap: [int]):
        self.heap = heap
        
    def left(self: "Heap", i: int) -> int:
        return 2 * i + 1
        
    def right(self: "Heap", i: int) -> int:
        return 2 * i + 2
        
    def parent(self: "Heap", i: int) -> int:
        return (i - 1) // 2
    
    def print_heap(self: "Heap"):
        for e in self.heap:
            print(e)
        
class MaxHeap(Heap):
    def max_heapify(self: "MaxHeap", i: int):
        l: int = 0
        r: int = 0
        largest: int = 0
        temp: int = 0
        l = self.left(i)
        r = self.right(i)
        largest = i
        if l < self.heap_size(): 
            if self.heap[l] > self.heap[i]:
                largest = l
        if r < self.heap_size():
            if self.heap[r] > self.heap[largest]:
                largest = r
        if largest < self.heap_size() and largest != i:
            # swap heap[i] with heap[largest]
            temp = self.heap[i]
            self.heap[i] = self.heap[largest]
            self.heap[largest] = temp
            self.max_heapify(largest)
            
    def build_max_heap(self: "MaxHeap"):
        n: int = 0
        n = self.heap_size() // 2 - 1
        while n >= 0:
            self.max_heapify(n)
            n = n - 1
        
class MinHeap(Heap):
    def min_heapify(self: "MinHeap", i: int):
        l: int = 0
        r: int = 0
        largest: int = 0
        temp: int = 0
        l = self.left(i)
        r = self.right(i)
        largest = i
        if l < self.heap_size(): 
            if self.heap[l] < self.heap[i]:
                largest = l
        if r < self.heap_size():
            if self.heap[r] < self.heap[largest]:
                largest = r
        if largest < self.heap_size() and largest != i:
            # swap heap[i] with heap[largest]
            temp = self.heap[i]
            self.heap[i] = self.heap[largest]
            self.heap[largest] = temp
            self.min_heapify(largest)
            
    def build_min_heap(self: "MinHeap"):
        n: int = 0
        n = self.heap_size() // 2 - 1
        while n >= 0:
            self.min_heapify(n)
            n = n - 1

data: [int] = None
minh: MinHeap = None
maxh: MaxHeap = None
data = [23, 43, 56, 23, 111, 6, 78, 926]

#               23
#             /    \
#           43       56
#          /  \      / \
#        23   111   6   78
#        /
#      926

print("=== Min Heap ===")
minh = MinHeap()
minh.set_heap(data)

minh.build_min_heap()
minh.print_heap()

#               6
#             /    \
#           23      23
#          /  \      / \
#        43   111   56   78
#        /
#      926 


print("=== Max Heap ===")
maxh = MaxHeap()
maxh.set_heap(data)

maxh.build_max_heap()
maxh.print_heap()

#               926
#             /    \
#           111      78
#          /  \      / \
#        43   6    56   23
#        /
#      23 
