# game of life

def initilize(grid: [[int]]):
    i: int = 0
    while i < len(grid):
        grid[i] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        i = i + 1

def print_grid(grid: [[int]]):
    s: str = ""
    for row in grid:
        for col in row:
            s = s + ("*" if col == 1 else "-")
        s = s + "\n"
    print(s)
    
def clean_gird(grid: [[int]]):
    i: int = 0
    j: int = 0
    rows: int = 0
    cols: int = 0
    rows = len(grid)
    cols = len(grid[0])
    while i < rows:
        while j < cols:
            grid[i][j] = 0
            j = j + 1
        i = i + 1
        j = 0
        
def calc_next_population(grid: [[int]], next: [[int]]) -> [[int]]:
    rows: int = 0
    cols: int = 0
    i: int = 0
    j: int = 0
    count: int = 0
    cell: int = 0
    
    def live_count() -> int:
        nonlocal count
        nonlocal grid
        nonlocal i
        nonlocal j
        count = 0
        if grid[(i - 1) % rows][(j - 1) % cols] == 1:
            count = count + 1
        if grid[(i - 1) % rows][j] == 1:
            count = count + 1
        if grid[(i - 1) % rows][(j + 1) % cols] == 1:
            count = count + 1
        if grid[i][(j - 1) % cols] == 1:
            count = count + 1
        if grid[i][(j + 1) % cols] == 1:
            count = count + 1
        if grid[(i + 1) % rows][(j - 1) % cols] == 1:
            count = count + 1
        if grid[(i + 1) % rows][j] == 1:
            count = count + 1
        if grid[(i + 1) % rows][(j + 1) % cols] == 1:
            count = count + 1
        return count
    
    rows = len(grid)
    cols = len(grid[0])
    while i < rows:
        j = 0
        while j < cols:
            count = live_count()
            cell = grid[i][j]
            if cell == 1:
                if count == 2 or count == 3:
                    next[i][j] = 1
                else:
                    next[i][j] = 0
            else:
                if count == 3:
                    next[i][j] = 1
                else:
                    next[i][j] = 0
            j = j + 1
        i = i + 1
    clean_gird(grid)
    return next

prev: [[int]] = None
next: [[int]] = None
temp: [[int]] = None
rounds: int = 22

prev = [[], [], [], [], [], [], [], [], [], []]
next = [[], [], [], [], [], [], [], [], [], []]

initilize(prev)
initilize(next)

# Specifically configure the initial population state
#    *
#   **
#    **
prev[5][10] = 1
prev[4][10] = 1
prev[6][10] = 1
prev[5][9] = 1
prev[6][11] = 1

while rounds > 0:
    temp = calc_next_population(prev, next)
    next = prev
    prev = temp
    rounds = rounds - 1

print_grid(prev)
