class animal(object):
    ss: str = "mute"  
    alien: bool = False
    def make_noise(self: "animal"):
        print(self.sound())
  
    def sound(self: "animal") -> str:
        return self.ss
 
class cow(animal):
    # def sound(self: "cow") -> str:
    #     return "moo\n"
    cow_sound: str = "MOO..!!"
    def make_noise(self: "cow"):
        print(self.cow_sound)
    
class dog(animal):
    dd: str = "dsfw"
    
    def make_noise(self: "dog"):
        print(self.dd)

c:animal = None
d:animal = None
cc:cow = None
# dd:dog = None
# gold: goldenretriever = None

# dd = goldenretriever()

cc = cow()

cc.make_noise()
# dd.make_noise()

# c = cow()
d = dog()

d.make_noise()

# print(c.tag)
