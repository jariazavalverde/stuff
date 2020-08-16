from function import Function

succ = Function(lambda x: x+1)
double = Function(lambda x: x*2)

# returnA 2
print( Function.returnA()(2) )
# ((+1) <<< (*2)) 2
print( (succ << double)(2) )
# ((+1) >>> (*2)) 2
print( (succ >> double)(2) )
# ((+1) *** (*2)) (2,3)
print( (succ * double)((2,3)) )
# ((+1) &&& (*2)) 2
print( (succ & double)(2) )