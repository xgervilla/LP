def my_map(f, L):
    return [f(x) for x in L]


def my_filter(f, L):
    return [x for x in L if f(x)]


def factors(n):
    return [x for x in range (1, n+1) if ((n % x) == 0)]

def triplets(n):
    res = []
    lista = [i for i in range (1, n+1)]
    for a in lista:
        for b in lista:
            for c in lista:
                if( ((a * a) + (b * b)) == (c * c) ):
                    res+=[(a,b,c)]
    return res