class animal(object):
    encephalization_level: int = 0

    def make_noise(self: "animal"):
        print(self.sound())

    def sound(self: "animal") -> str:
        return "???"

class dinosaur(animal):
    can_fly: bool = False
    
    def breathe(self: "dinosaur"):
        pass
    
    def sound(self: "dinosaur") -> str:
        return "RAWR! [Translation: 'I miss the good old days.']"
    
class human(animal):
    greet_text: str = "Hello, world!"
    
    def sound(self: "human") -> str:
        return self.greet_text
    
h:animal = None
d:animal = None

h = human()
d = dinosaur()

print("===================== Human =======================")
h.make_noise()
print("===================================================\n")
print("=================== Dinosaur ======================")
d.make_noise()
print("===================================================\n")
