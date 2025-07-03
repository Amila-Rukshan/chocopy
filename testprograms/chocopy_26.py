# Search in a list
def contains(items:[int], x:int) -> bool:
    i:int = 0
    for item in items:
        if item == x:
            return True

ints:[int] = None
ints = [4, 8, 15, 16, 23]

# pass list by r-value
if contains([4, 8, 15, 16, 23], 15):
    print("Item found!")    # Prints this
else:
    print("Item not found.")

# pass list by l-value
if contains(ints, 15):
    print("Item found!")    # Prints this
else:
    print("Item not found.")
