# call fn with different with lvalue or rvalue

class A(object):
    def test(self: "A"):
        print("AA")
        
class B(object):
    def hey(self: "B", aa: A):
        aa.test()

def accept(a_arg: A):
    a_arg.test()

a: A = None
b: B = None

a = A()
b = B()

# call with l-value
b.hey(a)
accept(a)

# call with r-value
b.hey(A())
accept(A())
