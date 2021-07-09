import math


# numeros de fibonacci
def fibs():
    a = 0
    yield a
    b = 1
    while True:
        yield b
        a, b = b, a+b


# f1(x) = x
# fn(x) = 1/2 * (fn-1(x) + x/fn-1(x))
def roots(x):
    a = x
    while True:
        yield a
        a = 1/2 * (a + x / a)


def isPrime(x):
    if(x <= 0):
        return False
    elif(x == 1 or x == 2):
        return True
    for i in range(2, (int)(math.sqrt(x))+1):
        if((x % i) == 0):
            return False
    return True


# numeros primos
def primes():
    a = 2
    yield a
    a = 3
    while True:
        if(isPrime(a)):
            yield a
        a += 2


# numeros de hamming (tienen 2, 3 y 5 como unicos divisores primos)
def hammings():
    a = 1
    yield a
    a = 2
    while True:
        if(a == 2 or a == 3 or a == 5):
            yield a
        else:
            divs = [x for x in range(2, a) if((isPrime(x) and (a % x) == 0))]
            # si tiene longitud entre 1 y 3 (no es primo y como maximo tiene 2, 3 y 5 como divisores primos)
            if(len(divs) > 0 and checkDivs(divs)):
                yield a
        a += 1


def checkDivs(d):
    for x in d:
        if (isPrime(x) and (x != 2) and (x != 3) and (x != 5)):
            return False
    return True
