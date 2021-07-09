from functools import reduce

class Tree:
    def __init__(self, x):
        self.rt = x        # valor de la raiz
        self.child = []    # sin hijos (vacio)


    def add_child(self, a):
        self.child.append(a)    # agrega 'a' como hijo
 

    def root(self):
        return self.rt          # devuelve la raiz
 

    def ith_child(self,i):
        return self.child[i]    # i-esimo hijo
 

    def num_children(self):
        return len(self.child)  # numero de hijos

   yield a


class Pre(Tree):
    def preorder(self):     # preorden: raiz-> hijos (de izquierda a derecha)
        pre = [self.rt]     # raiz
        for t in self.child:    # hijos de izquierda (menor indice) a derecha (mayor indice)
            pre = pre + t.preorder()
        return pre