# list concatination

class R(object):
    def do(self: "R"):
        pass

class P(R):
    def do(self: "P"):
        print("PPpPP")
        
class Q(R):
    def do(self: "Q"):
        print("QQqQQ")
        
#    R
#   / \
#  P   Q

arr1: [str] = None
arr2: [str] = None
arr3: [str] = None

boolArr: [bool] = None
ints: [int] = None

objs: [R] = None

arr1 = ["foo"]
arr2 = ["bar", "baz"]

arr3 = arr1 + arr2 + ["new"]

print(arr3[3])
print(len(arr3))

boolArr = [True] + [False]

print(len(boolArr))

ints = []
ints = ints + [0, 0, 0, 0, 0, 0, 0, 0, 0]

print(ints[7])
print(len(ints))

objs = [Q(), P()]
objs = objs + objs

print(len(objs))
objs[2].do()
objs[3].do()
