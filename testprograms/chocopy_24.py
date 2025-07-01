# class instances in a list, dispatch while enumerating a list

class base(object):
    def test(self: "base"):
        pass
    
class A(base):
    def test(self: "A"):
        print("test A")

class B(base):
    def test(self: "B"):
        print("test B")
        
arr: [[base]] = None
arr = [[A()], [B()]]

for inner in arr:
    for obj in inner:
        obj.test()

# index into list and call a method
arr[1][0].test()
