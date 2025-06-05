# if-else expression

class A(object):
    a: bool = True
    i: int = 0
    def foo(self: "A", val: int) -> int:
        print(val)
        print(self.i)
        if not self.a:
            print("Hello")
            return 7
        else:
            if self.a:
                print("Inner If")
                return 5
            print("Goodbye")
    
a: A = None
a = A()

print(a.foo(23))
