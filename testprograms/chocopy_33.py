# nested functions inside methods

class G(object):
    def go(self: "G") -> int:
        return 44

class C(object):

    def f(self:"C") -> int:
        
        def g(a: int) -> int:
            x:int = 1
            return a // x
    
        return g(1) + G().go()

print(C().f())
