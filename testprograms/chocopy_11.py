# while loop

class FizzBuzz(object):
    def run(self: "FizzBuzz", n: int) -> int:
        count: int = 0
        while n >= 1:
            if n % 3 == 0 and n % 5 == 0:
                count = count + 2
                print("FizzBuzz")
            else:
                if n % 3 == 0:
                    count = count + 1
                    print("Fizz")
                else:
                    if n % 5 == 0:
                        count = count + 1
                        print("Buzz")
                    else:
                        print(n)
            n = n - 1
        return count
    
result: int = 0
fb: FizzBuzz = None
fb = FizzBuzz()

result= fb.run(20)
print("RESULT:")
print(result)
