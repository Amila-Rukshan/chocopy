# str comparison

class str_container(object):
    s: str = ""
    
    def setter(self: "str_container", string: str):
        self.s = string
        print(self.s == string)
        
    def getter(self: "str_container") -> str:
        return self.s

sc: str_container = None

a: str = "a"
b: str = "b"
c: str = "wrap me, please"

print("a" == "b")
print("a" == "a")
print(a == "a")
print("c" == b)

sc = str_container()

sc.setter(c)
print(sc.getter() != c)
