# empty list literal 

arr: [[int]] = None

arr = [[], [], [1, 2, 3], []]

print(len(arr))
print(len(arr[2]))
print(len(arr[3]))

arr[3] = [4, 5, -6, 7]

print(len(arr[3]))
print(arr[3][2])

arr[2] = []

print(len(arr[2]))
