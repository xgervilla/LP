import math


def absValue(x):
    if (x >= 0):
        return x
    return -x


def power(x, p):
    if(p == 0):
        return 1
    elif (p < 0):
        return (float) (1 / power(x, absValue(p)))
    if(p == 1):
        return x
    return x * power(x, p-1)


def isPrime(x):
    if(x<=0):
        return False
    elif(x == 1 or x == 2):
        return True
    for i in range(2, (int)(math.sqrt(x))+1):
        if((x % i) == 0):
            return False
    return True


def slowFib(n):
    if(n == 0):
        return 0
    elif (n == 1):
        return 1
    else:
        return slowFib(n-1)+slowFib(n-2)


def quickFib(n):
    if (n == 0):
        return 0
    elif (n == 1):
        return 1
    else:
        def quickFibAux(nAux, prev, act):
            if(nAux == 0):
                return prev+act
            else:
                return quickFibAux(nAux-1, act, (prev+act))
        return quickFibAux(n-2, 0, 1)

