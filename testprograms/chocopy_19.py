# simple list manipulation program, game of life

arr: [int] = None
i: int = 0

arr = [12, 1, 23, 11, 4, 5, 6, 7, 8, 9, 10]
    
arr[0] = 100
arr[5] = 200
arr[10] = 300

while i < len(arr):
    print(arr[i])
    i = i + 1
