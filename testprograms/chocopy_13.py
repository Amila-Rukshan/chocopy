# stack implementation using a singly linked list

class list_node(object):
    val: str = ""
    next: "list_node" = None

    def set_value(self: "list_node", v: str):
        self.val = v

class singly_linked_list(object):
    head: list_node = None

    def insert(self: "singly_linked_list", v: str):
        new_node: list_node = None
        new_node = list_node()
        new_node.set_value(v)
        
        if not (self.head is None):
            new_node.next = self.head
        self.head = new_node

    def print(self: "singly_linked_list"):
        curr: list_node = None
        curr = self.head
        while not (curr is None):
            print(curr.val)
            curr = curr.next
            
class stack(singly_linked_list):
    def push(self: "stack", v: str):
        self.insert(v)

    def pop(self: "stack") -> str:
        popped: str = 0
        if self.head is None:
            return ""
        popped = self.head.val
        self.head = self.head.next
        return popped

s: stack = None
s = stack()
s.push("Mojo")
s.push("C++")
s.push("Rust")
print(s.pop())
print(s.pop())
s.print()
