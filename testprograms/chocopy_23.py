# nested for looping: find a str in a deeply nested list

arr: [[[str]]] = None
arr = [[["se7en", "Matrix"], ["LoTR"]], [["", "Gladiator"]], [["Fight Club"]]]

def find_gg(input: [[[str]]], search: str) -> bool:
    for l2 in input:
        for l3 in l2:
            for m in l3:
                if m == search:
                    return True

print(find_gg(arr, "Fight Club"))
print(find_gg(arr, "Inception"))
