# multi dimensional array, matrix transpose

i: int = 0
j: int = 0
temp: int = 0
A: [[int]] = None
A = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

print(A[1][2])
print(A[2][1])
print(A[1][1] == 5)

while i < len(A):
    while j < len(A[i]):
        if i < j:
            temp = A[i][j]
            A[i][j] = A[j][i]
            A[j][i] = temp
        j = j + 1
    i = i + 1
    j = 0   
    
print(A[1][2])
print(A[2][1])
print(A[1][1] == 5)

