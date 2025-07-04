# shallow copy surprise

arr: [[int]] = None
arr2: [[int]] = None

arr = [[0, 1], [3, 54]]

arr2 = arr + arr

print(len(arr2))

arr2[2][1] = 77

# yes it's also 77, because we opened a portal by shallow copy in line 8
print(arr2[2][1])
print(arr2[0][1])
print(arr[0][1])
