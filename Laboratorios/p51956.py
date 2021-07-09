import math


def myLength(L):
    myLen = 0
    for i in L:
        myLen += 1
    return myLen


def myMaximum(L):
    myMax = L[0]
    for i in L:
        if(i > myMax):
            myMax = i
    return myMax


def average(L):
    mySum = 0
    for i in L:
        mySum += i
    return mySum / float(myLength(L))


def buildPalindrome(L):
    res = [L[-i] for i in range(1, myLength(L)+1)]
    res += L
    return res


def flatten(L):
    res = []
    if(not isinstance(L, list)):
        return [L]
    else:
        for i in L:
            res += flatten(i)
    return res


def remove(L1, L2):
    res = [x for x in L1 if not (x in L2)]
    return res


def oddsNevens(L):
    ods = [x for x in L if((x % 2) == 1)]
    evens = [x for x in L if((x % 2) == 0)]
    return ods, evens


def primeDivisors(n):
    def isPrime(x):
        if(x == 1 or x == 2):
            return True
        for i in range(2, (int)(math.sqrt(x))+1):
            if((x % i) == 0):
                return False
        return True

    divs = [x for x in range(3, n) if(((n % x) == 0) and isPrime(x))]
    return divs
