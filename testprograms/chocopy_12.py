# singly linked list

class list_node(object):
    val: int = 0
    next: "list_node" = None

    def set_value(self: "list_node", v: int):
        self.val = v

class singly_linked_list(object):
    head: list_node = None

    def insert(self: "singly_linked_list", v: int):
        new_node: list_node = None
        new_node = list_node()
        new_node.set_value(v)
        
        if not (self.head is None):
            new_node.next = self.head
        self.head = new_node

    def search(self: "singly_linked_list", v: int) -> bool:
        curr: list_node = None
        curr = self.head
        while not (curr is None):
            if curr.val == v:
                return True
            curr = curr.next
        return False

    def print(self: "singly_linked_list"):
        curr: list_node = None
        curr = self.head
        while not (curr is None):
            print(curr.val)
            curr = curr.next

sl: singly_linked_list = None
sl = singly_linked_list()
sl.insert(10)
sl.insert(45)
sl.insert(5)
sl.insert(20)

sl.print()

print(sl.search(11))
print(sl.search(5))
