# boolean arithmetic

a: bool = True
b: int = 10
c: int = 0

d: bool = False

print(a and b < c)

print(not a)
print(a == True or b >= c and c != 0)           # default: a == True or (b >= c and c != 0)
print((a == True or b >= c) and c != 0)         # parentheses: (a == True or b >= c) and c != 0
