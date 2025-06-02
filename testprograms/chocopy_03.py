# dynamic dispatch test using template pattern

class vehicle(object):
    def drive(self: "vehicle"):
        self.shift()
    
    def shift(self: "vehicle"):
        pass
    
class car(vehicle):
    def shift(self: "car"):
        print("shift using rear wheels")
    
class suv(vehicle):
    def shift(self: "suv"):
        print("shift using all wheels")

hondacity: car = None
nissanxtrail: suv = None

hondacity = car()
nissanxtrail = suv()

hondacity.drive()
nissanxtrail.drive()
