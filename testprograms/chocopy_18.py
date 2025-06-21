# constructor calls when ___init__ dunder methods present

class A(object):
    i : int = 10
    w: str = "Aa"
    
    def __init__(self: "A"):
        self.i = 77
        self.w = self.w + "aA"
    
    def foo(self: "A") -> int:
        return self.i + 2

class B(A):
    def __init__(self: "B"):
        pass
    
class C(A):
    pass

class D(A):
    def __init__(self: "D"):
        self.i = 99
        self.w = self.w + "dD"
    
b: A = None
c: A = None
d: A = None

b = B()
c = C()
d = D()

print(b.foo())
print(b.w)

print(c.foo())
print(c.w)

print(d.foo())
print(d.w)
