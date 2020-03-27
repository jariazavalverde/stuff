def jump():
    p = [-2,-1,1,2]
    xs = []
    for x in p:
        for y in p:
            if 3 == abs(x) + abs(y):
                xs.append((x,y))
    return xs

def onBoard(pos):
    (x,y) = pos
    return x in range(1,8) and y in range(1,8)

def moveKnight(pos):
    (x,y) = pos
    xs = []
    for (a,b) in jump():
        if onBoard((x+a,b+y)):
            xs.append((x+a,b+y))
    return xs

def in3(pos0):
    xs = []
    for pos1 in moveKnight(pos0):
        for pos2 in moveKnight(pos1):
            for pos3 in moveKnight(pos2):
                xs.append(pos3)
    return xs

def reachIn3(init, to):
    return to in in3(init)