# string len and indexing

def is_palindrome(string: str) -> bool:
    left: int = 0
    length: int = 0
    length = len(string)
    
    while left < length // 2:
        if string[left] != string[length - left - 1]:
            return False
        left = left + 1
    return True

vowels: str = "AEIOU"
index: int = 0

pal_test: str = "anutforajaroftuna"

print(is_palindrome(pal_test))

while index < len(vowels):
    print(vowels[index])
    index = index + 1
