# class attribute property access

class component(object):
    x: int = 100
    flag: bool = True
    name: str = "test"
    

c: component = None
c = component()    

print(c.x)
print(c.flag)
print(c.name)
