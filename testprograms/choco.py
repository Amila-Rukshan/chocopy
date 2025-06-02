class myint(object):
    val: int = 72
    
    def p(self: "myint"):
        print(self.val + 5)

class wrapper(object):
    m: myint = None
    s: str = "hello"
    
    def init(self: "wrapper"):
        self.m = myint()
        self.m.p()

    def get_m(self: "wrapper") -> myint:
        return self.m
    
class subwrapper(wrapper):
    pass
        
class container(object):
    w: wrapper = None
    i: int = 34
    
    def init(self: "container"):
        self.w = subwrapper()
        self.w.init()
        
        self.w.get_m().val = 201
        
        print(self.w.m.val)
        

c: container = None
c = container()

c.init()
c.i = 100
print(c.i)

# support chained lookups - done
