data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add exp1 exp2) = (eval1 exp1) + (eval1 exp2)
eval1 (Sub exp1 exp2) = (eval1 exp1) - (eval1 exp2)
eval1 (Mul exp1 exp2) = (eval1 exp1) * (eval1 exp2)
eval1 (Div exp1 exp2) = div (eval1 exp1) (eval1 exp2)

eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add exp1 exp2) = eval2' (+) exp1 exp2
eval2 (Sub exp1 exp2) = eval2' (-) exp1 exp2
eval2 (Mul exp1 exp2) = eval2' (*) exp1 exp2
eval2 (Div exp1 exp2) = do 
    expr1 <- eval2 exp1
    expr2 <- eval2 exp2
    if(expr2 == 0) then Nothing else Just (div expr1 expr2)


eval2' op e1 e2 = do
    expr1 <- eval2 e1
    expr2 <- eval2 e2
    Just (op expr1 expr2)


eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add exp1 exp2) = do 
    expr1 <- eval3 exp1
    expr2 <- eval3 exp2
    Right(expr1+expr2)
eval3 (Sub exp1 exp2) = do 
    expr1 <- eval3 exp1
    expr2 <- eval3 exp2
    Right(expr1-expr2)
eval3 (Mul exp1 exp2) = do 
    expr1 <- eval3 exp1
    expr2 <- eval3 exp2
    Right(expr1*expr2)
eval3 (Div exp1 exp2) = do 
    expr1 <- eval3 exp1
    expr2 <- eval3 exp2
    if(expr2 == 0) then Left ("div0") else Right(div expr1 expr2)
