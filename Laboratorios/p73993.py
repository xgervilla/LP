from functools import reduce


def evens_product(L):
    prod = 1
    for x in L:
        if(x % 2 == 0):
            prod*=x
    return prod


def reverse(L):
    def flatten(L):
        res = []
        if(not isinstance(L, list)):
            return [L]
        else:
            for i in L:
                res += flatten(i)
        return res

    rev1 = reduce(lambda acc, y: [y]+[acc], L)
    return flatten(rev1)


def zip_with(f, L1, L2):
    return [f(x, y) for x in L1 for y in L2 if(L1.index(x) == L2.index(y))]


def count_if(f, L):
    return len([x for x in L if(f(x))])
