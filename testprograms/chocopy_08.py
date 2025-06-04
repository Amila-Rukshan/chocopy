# if-else expression

class vehicle(object):
    def drive(self: "vehicle"):
        pass

class suv(vehicle):
    def drive(self: "suv"):
        print("Driving SUV")
        
class car(vehicle):
    def drive(self: "car"):
        print("Driving Car")
    
is_off_road: bool = True
selected_vehicle: vehicle = None

a: int = 10
b: int = 5

print(a if a > b else b)

# if-else expression with type union in the class hierarchy
selected_vehicle = suv() if is_off_road else car()
selected_vehicle.drive()
