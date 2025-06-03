# interger arithmetic with associativity (equal precedence then left associativity) and precedence
# use parantheses to avoid precedence and being explicit about associativity

x: int = 34
y: int = 56
z: int = 12

print(x - y + z)

print(x + y * z)
print((x + y) * z)

print(y % z // 2)
print(y % (z // 2))
