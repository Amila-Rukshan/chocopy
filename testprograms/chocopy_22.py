# for loop tests

int_arr: [int] = None
str_arr: [str] = None
bool_arr: [bool] = None
choco_str: str = "chocopy"

int_arr = [12, 45, 23]
str_arr = ["this", "is", "cool"]
bool_arr = [True, False, True]


for n in int_arr:
    print(n) 

for s in str_arr:
    print(s) 

for b in bool_arr:
    print(b)

for c in choco_str:
    print(c)
