# recursive types, binary tree

class node(object):
    left: "node" = None
    right: "node" = None
    value: int = 0
    
    def set_value(self: "node", val: int):
        self.value = val
        
    def insert(self: "node", val: int):
        if val < self.value:
            if self.left is None:
                self.left = node()
                self.left.set_value(val)
            else:
                self.left.insert(val)
        else:
            if self.right is None:
                self.right = node()
                self.right.set_value(val)
            else:
                self.right.insert(val)

    def find(self: "node", val: int) -> bool:
        if self.value == val:
            return True
        else:
            if val < self.value:
                if self.left is None:
                    return False
                return self.left.find(val)
            else:
                if self.right is None:
                    return False
                return self.right.find(val)
    
    def inorder(self: "node"):
        if not (self.left is None):
            self.left.inorder()
        print(self.value)
        if not (self.right is None):
            self.right.inorder()

root: node = None
root = node()

root.set_value(100)
root.insert(150)
root.insert(40)
root.insert(60)

print(root.find(60))
print(root.find(20))

root.inorder()
