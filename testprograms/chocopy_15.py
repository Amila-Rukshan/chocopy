# global funcs defintion and usage

def nested_while():
    x: int = 8
    y: int = 0
    z: int = 0
    while x > 0:
        x = x // 2
        y = x
        print("x")
        print(x)
        while y > 0:
            y = y // 4
            z = y
            print("y")
            print(y)
            while z > 0:
                z = z // 8
                print("z")
                print(z)

def fib(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

i: int = 0

nested_while()

print("===== fibonacci numbers =====")
while i < 10:
    print(fib(i))
    i = i + 1
