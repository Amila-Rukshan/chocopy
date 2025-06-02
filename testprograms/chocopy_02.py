# class definition with type annotation and method

class A(object):
    i : int = 10
    def foo(self: "A") -> int:
        return self.i + 2

a: A = None
a = A()

print(a.foo())
