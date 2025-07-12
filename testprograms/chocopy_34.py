# nested functions with a closure variable access

class C(object):
    a: [int] = None
    
    def __init__(self: "C"):
        self.a = [12, 34, 55]

def A(c: bool):
    x: int = 0
    y: bool = True
    a: C = None
        
    def B():
        x = 5
        y = False
        print(a.a[0])
        a.a[1] = a.a[1] // 2
    
    a = C()
    B()
    print(x)
    print(y)
    print(a.a[1])

A(True)
