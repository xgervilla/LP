def count_unique(L):
    return len(remove_duplicates(L))


def remove_duplicates(L):
    res = []
    for x in L:
        if(x not in res):
            res += [x]
    return res


def flatten_rec(L):
    res = []
    if(not isinstance(L, list)):
        return [L]
    else:
        for i in L:
            res += flatten_rec(i)
    return res


def flatten(L):
    res = []
    for i in L:
        for x in i:
            res += [x]
    return res
