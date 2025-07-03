# lists as class attributes

class I(object):
    ints: [[int]] = None
    
    def __init__(self: "I"):
        self.ints = [[12, 23], [45], [21]]
        
    def test(self: "I"):
        print(len(self.ints[0]))
        
    def get(self: "I", idx: int) -> int:
        return (self.ints[idx][0])

class B(object):
    bools: [[[[bool]]]] = None
    
    def __init__(self: "B"):
        self.bools = [[[[True]]],[[[]], [[False, True]]]]
        
    def test(self: "B"):
        print(len(self.bools[1][1]))
        
    def get(self: "B") -> bool:
        return self.bools[1][1][0][1]

class S(object):
    strs: [[str]] = None
    
    def __init__(self: "S"):
        self.strs = [["", "Foo"], ["Bar", "Baz"]]
        
    def test(self: "S"):
        print(len(self.strs[1]))
        
    def get(self: "S") -> str:
        return self.strs[0][1]
    
class Base(object):
    val: int = 0
    
    def check(self: "Base"):
        pass
    
class T1(Base):
    def __init__(self: "T1"):
        self.val = 6
    
    def check(self: "T1"):
        print("T1")
        
class T2(Base):
    def __init__(self: "T2"):
        self.val = 7
    
    def check(self: "T2"):
        print("T2")
        
class Holder(object):
    bases: [Base] = None
    
    def __init__(self: "Holder"):
        self.bases = [T1(), T2()]
        
    def test(self: "Holder"):
        for bs in self.bases:
            bs.check()
            print(bs.val)

i: I = None
b: B = None
s: S = None
h: Holder = None

i = I()
i.test()
print(i.get(2))
print(i.ints[0][1])

b = B()
b.test()
print(b.get())
print(b.bools[1][1][0][0])

s = S()
s.test()
print(s.get())
print(s.strs[1][0])

h = Holder()
h.test()
