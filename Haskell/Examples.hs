{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-|
Module      : Examples
Description : example programs to run exploit generation on
Copyright   : (c) Nico Naus, 2022
Maintainer  : niconaus@vt.edu
Stability   : experimental
This module defines several example programs, including post conditions and a call to the solver
-}
module Examples(program,post, power,postPower, ndloop,postND, write,writeWrite,postW, nonDet,postNon, longDiv, faultyQsort, indirectJump) where

import Types
import qualified Prelude
import Prelude(Int,String,($),(.),show,error)
import qualified Data.Map as M
import While

program::Prog
post::Predicate

(program,post) = jumpTable
--(program,post) = ropExample

{- some syntactic sugar -}

(==>) :: a -> b -> (a, b)
x==>y = (x,y)

(...) :: S -> B -> B
a...b = Seq a b

($:=) :: ToExpr a => V -> a -> S
a$:=b = Assign a (toExpr b)

(?:=) :: ToExpr a => V -> a -> S
a?:=b = NDAssign a (toExpr b)

(#) :: ToExpr a => Int -> a -> E
n#e = toExpr e
infixl 0 ==>
infixr 1 ...
infix  2 $:=
infix  2 ?:=
infixr 2 ||
infixr 3 &&
infixr 4 ==
infixr 4 <=
infixr 4 >=
infixr 4 <
infixr 4 >
infix 4 #
infixl 6 +
infixl 6 -
infixl 7 *

not :: ToExpr a => a -> E
not = Not . toExpr

class ToExpr a where toExpr :: a -> E
instance ToExpr E where toExpr x = x
instance ToExpr String where toExpr s = Var s

class ToPred a where toPred :: a -> Predicate
instance ToPred Predicate where toPred e = e
instance ToPred E where toPred e = Expr e
instance ToPred p => ToPred [p]  where
  toPred [] = toPred true
  toPred [p] = toPred p
  toPred ps = Prelude.foldr1 PAnd (Prelude.map toPred ps)

(+) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a + b  = App (toExpr a) Plus (toExpr b)

(-) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a - b  = App (toExpr a) Minus (toExpr b)

(*) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a * b  = App (toExpr a) Mult (toExpr b)

(==) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a == b = App (toExpr a) Eq (toExpr b)

(/=) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a /= b = Not (App (toExpr a) Eq (toExpr b))

(<=) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a <= b = App (toExpr a) Leq (toExpr b)

(<) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a < b  = App (toExpr a) Less (toExpr b)

(>) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a > b  = App (toExpr a) Greater (toExpr b)

(>=) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a >= b = App (toExpr a) Geq (toExpr b)

(||) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a || b = App (toExpr a) Or (toExpr b)

(&&) :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a && b = App (toExpr a) And (toExpr b)

mod :: (ToExpr a1, ToExpr a2) => a1 -> a2 -> E
a `mod`  b = App (toExpr a) Mod (toExpr b)

exploit :: (Prelude.Ord k, ToPred a1) =>[(k, a2)] -> a1 -> ((k, M.Map k a2), Predicate)
exploit prog@((i,_):_) = ((i,M.fromList prog)==>).toPred
exploit _ = Prelude.undefined

exists :: ToPred p => V -> p -> Predicate
exists mv p = Exists mv (toPred p)

-- PAPER EXAMPLES

{- Figure 6: the "double write" example -}
doublewriteExample = (writeWrite, postW)

writeWrite :: Prog
writeWrite = (0, M.fromList [(0,Seq (Assign "temp1" (Deref ( (Var "a") )))
                                    (Seq (Types.Store ( (Var "b") ) "temp1")
                                         (Seq (Assign "temp2" (Deref ( (Var "c") )))
                                              (Seq (Types.Store ( (Var "d") ) "temp2") (Exit) )) ))])

write :: Prog
write = (0, M.fromList [(0,  (Seq (Types.Store ( (Var "b") ) "temp1")
                                         (Seq (Assign "temp2" (Deref ( (Var "c") )))
                                              (Seq (Types.Store ( (Var "d") ) "temp2") (Exit) )) ))])

postW :: Predicate
postW = Expr $ App (Deref ( (Var "e") )) Eq (Var "z")

{- Figure 7: long division -}
longDiv :: (Prog,Predicate)
longDiv = [
                  0 ==> CJump ("x" < "y") 3 1,
                  1 ==> "i" $:= Num 1...
                        "x" $:= "x" - "y"...
                        CJump ("x" >= "y") 2 3,
                  2 ==> "x" $:= "x" - "y"...
                        "i" $:= "i" + Num 1...
                        CJump ("x" > "y") 2 3,
                  3 ==> Exit
      ]
   `exploit` ("x" >= "y")

{- Figure 8: non-deterministic loop -}
ndloopExample = (ndloop, postND)

ndloop :: Prog
ndloop = (0,M.fromList [(0, Seq (NDAssign "d" (App (App (Num 1) Less (Var "d") ) And (App (Var "d") Less (Var "n") )))
                                (CJump (App (App (Var "n") Mod (Var "d")) Eq (Num 0)) 1 0) ),
                        (1,Exit)])

postND :: Predicate
postND = Expr $ App (Num 0) Eq (Num 0)

{- Figure 9: buffer overflow -}
ropExample :: (Prog, Predicate)
ropExample = [
   -1 ==> Store (8#"rsp") "return_address"...    -- (prelude: this is done by the call instruction)
          --"rdx" ?:= true ...                   -- we have no control over rdx
          Jump 0,

    0 ==> Assign "rsp" ("rsp" - Num 40)...       -- sub rsp, 40
          CJump ("rdi" > Num 10) 2 1,             -- cmp rdi, 10 / ja label
    1 ==> Store (8#("rsp"+"rdi"*Num 4)) "rdx"... -- mov [rsp+rdi*4], rdx
          Jump 2,
    2 ==> Assign "rsp" ("rsp" + Num 40)...       -- add rsp, 40
          Exit                                   -- "ret"
 ]
  `exploit` ((8#"rsp") /= "return_address")

{- Figure 10: buffer overflow -}
jumpTable :: (Prog, Predicate)
jumpTable = [
    -2  ==> --"y" $:= Num 0...
            CJump ("x" < Num 0 || "x" > Num 3) 9 (-1),
    -1  ==> IndirectJump (Var "x"),
     0  ==> Jump 2,
     1  ==> Jump 9,
     2 ==> "y" $:= Num 5...
           Jump 9,
     9 ==> Exit
 ]
 `exploit` ("y" > Num 0)

{- Figure XX: indirect jump -}
indirectJump :: (Prog, Predicate)
indirectJump = [0  ==> "y" $:= Num 0...
                       CJump ("x" < Num 1 || "x" > Num 4) 9 1,
                1  ==> IndirectJump (Var "x"),
                2  ==> Jump 4,
                3  ==> Jump 9,
                4  ==> "y" $:= Num 5...
                       Jump 9,
                9  ==> Exit
 ]
 `exploit` ("y" > Num 0)

-- QUICKSORT EXAMPLE

faultyPart :: (Prog,Predicate)
faultyPart = [
                -1  ==> "lwb" $:= Num 0... "upb" $:= Num 2...   -- to reason about 3-element arrays
                        CJump ("lwb" < "upb") 0 999,
                0   ==> "index" ?:= "lwb" <= "index" && "index" <= "upb" ...
                        --"index" $:= Num 0...                  -- this fixes E_range (or block 6 can be made conditional)
                        "pivot" $:= (4#("a" + "index")) ...
                        "i" $:= "lwb" ...
                        "j" $:= "upb" ...
                        Jump 1,

                1   ==> CJump ("i" <= "j") 2 999,   -- outer while

                2   ==> CJump ("i" < "j") 20 4, -- inner while 1
                20  ==> boundcheck 21,
                21  ==> CJump ((4#("a"+"i")) <= "pivot") 3 4,

                3   ==> "i" $:= "i" + Num 1 ...
                        CJump ("i" < "j") 20 4,

                4   ==> CJump ("i" <= "j") 40 6,  -- inner while 2 --
                40  ==> boundcheck 41,
                41  ==> CJump ("pivot" <= (4#("a"+"j"))) 5 6,

                5   ==> "j" $:= "j" - Num 1 ...
                        CJump ("i" <= "j") 40 6,

                6   ==> Jump 70,
                --6   ==> Jump ("i" < "j") 70 1, -- if statement

                70  ==> boundcheck 7,

                7   ==> "tmp_i" $:= (4#("a"+"i")) ...
                        "tmp_j" $:= (4#("a"+"j")) ...
                        Store (4#("a"+"i")) "tmp_j" ...
                        Store (4#("a"+"j")) "tmp_i" ...
                        "i" $:= "i" + Num 1 ...
                        "j" $:= "j" - Num 1 ...
                        CJump ("i" <= "j") 2 999,

                666 ==> "E_range" $:= Num 1 ...
                        Exit,

                999 ==> "E_range" $:= Num 0 ...
                        Exit
            ]
{- all the comments below are for the version where basic block 6 is an actual if statement instead of a unconditional jump -}
--   `exploit`(Expr true)
     `exploit`("E_range" == Num 1) -- with "fixes" applied: not found, because it doesnt happen
--   `exploit`("i" == "lwb") --       ,,                 ,, yes, causes stack overflow
--   `exploit`("j" >= "upb") --                             shouldn't occur, causes stack overflow
--   `exploit`("i" > "upb")  --                             shouldn't occur
--   `exploit`("j" < "lwb")  --                             occurs and indicative of errors
--   `exploit`(exists "x" $ "lwb" <= ("x") && ("x") <= "i" && (4#("a"+("x"))) > "pivot")
--   `exploit`(exists "x" $ "upb" >= ("x") && ("x") >= "j" && (4#("a"+("x"))) < "pivot")
   where boundcheck = CJump ("i" < "lwb" || "i" > "upb" || "j" < "lwb" || "j" > "upb") 666
         -- ign__check = (\x -> Jump true x x)

-- LIMITATION EXPLORATION

{- a depth-first search analysis will be defeated by this -}
depthFoil :: (Prog, Predicate)
depthFoil = [
    0 ==> "x" $:= Num 1000...
          Jump 1,
    1 ==> "x" $:= "x" - Num 1...
          CJump ("x" > Num 0) 1 2,          -- depth1st: ----, breadth1st: 2.2s
          --Jump (Not $ "x" > Num 0) 2 1,  -- depth1st: 2.7s, breadth1st: 2.1s
    2 ==> Exit
 ]
 `exploit` ("x" == Num 0)

{- a depth-first search analysis will be defeated by this, although it will theoretically find the exploit -}
breadthFoil :: (Prog, Predicate)
breadthFoil = [
    0 ==> "x" $:= Num 1000...
          Jump 1,
    1 ==> "x" $:= "x" - Num 1...
          CJump ("x" <= Num 0) 2 3,
    2 ==> Exit,
    3 ==> "y" ?:= true...
          CJump ("y"==Num 0) 1 3
 ]
 `exploit` ("x" == Num 0)

-- MISCELLANEOUS EXAMPLES AND TEST PROGRAMS

nonDet :: Prog
nonDet = (0,M.fromList [(0,Seq (NDAssign "x" (App (Var "x") Less (Num 5))) Exit)])

postNon :: Predicate
postNon = Expr $ App (Var "x") Eq (Var "z")

power :: Prog
power = (0,M.fromList [(0, (Seq (Assign "v" (Var "x")) (Seq (Assign "i" (Num 0)) (CJump (App (Var "i") Less (Var "x")) 1 2) ) )),
                       (1, (Seq (Assign "n" (App (App (Var "n") Mult (Num 3)) Mod (Num 7) ))
                                (Seq (Assign "v" (App (Var "v") Plus (Var "n")))
                                     (Seq (Assign "i" (App (Var "i") Plus (Num 1)))
                                          (CJump (App (Var "i") Less (Var "x")) 1 0 )) ) )),
                       (2,Exit)])

postPower :: Predicate
postPower = Expr $ App (Var "v") Eq (Num 5)

simple :: (Prog,Predicate)
simple = [
                  0 ==> "y" ?:= true ...
                        Jump 1,
                  1 ==> "x" $:= "y" ...
                        "y" $:= Num 1 ...
                        CJump ("x" == "y") 2 1,
                  2 ==> Exit
         ]
    `exploit`("x" == Num 1)

fac :: (Prog,Predicate)
fac = [
                  0 ==> "y" $:= Num 1 ...
                        "k" $:= Num 1 ...
                        Jump 1,
                  1 ==> "y" $:= "y" * "k" ...
                        "k" $:= "k" + Num 1 ...
                        CJump ("k" <= "x") 1 2,
                  2 ==> Exit
      ]
    `exploit`("y" >= Num 120)

qsortFail :: (Prog,Predicate)
qsortFail = [
                -1  ==> "exploited" $:= Num 0 ...
                        "i" ?:= Num 0 <= "i" ...
                        "p" $:= 4#("a" + "i") ...
                        "i" $:= Num 0 ...
                        Jump 0,

                0   ==> CJump ("i" >= "L") 666 1,

                1   ==> CJump ((4#("a" + "i")) > "p") 3 2,

                2   ==> "i" $:= "i" + Num 1 ...
                        Jump 0,

                3   ==> Exit,

                666 ==> "exploited" $:= Num 1 ...
                        Exit
            ]
    `exploit`("exploited" == Num 1)

swapwoTmp :: (Prog,Predicate)
swapwoTmp = [
                0 ==> "old_x" $:= 1#"x"...
                      "old_y" $:= 1#"y"...
                      "tmp" $:= ((1#"x") + (1#"y"))...Store (1#"x") "tmp"...
                      "tmp" $:= ((1#"x") - (1#"y"))...Store (1#"y") "tmp"...
                      "tmp" $:= ((1#"x") - (1#"y"))...Store (1#"x") "tmp"...
                      Exit
       ]
    `exploit`(not ((1#"y") == "old_x"))

memcpy :: (Prog,Predicate)
memcpy = [
{-
                1 ==> "N" $:= Num 2...
                      "tmp" $:= 1#"src"...
                      "src" $:= "src"+Num 1...
                      Store (1#"dst") "tmp"...
                      "dst" $:= "dst"+Num 1...
                      "tmp" $:= 1#"src"...
                      "src" $:= "src"+Num 1...
                      Store (1#"dst") "tmp"...
                      "dst" $:= "dst"+Num 1...
                      Exit,
-}

               -1 ==> "limit" $:= "src" + "N"...
                      Jump 0,
                0 ==> CJump ("src" >= "limit") 9 1,
                1 ==> "tmp" $:= 1#"src"...
                      "src" $:= "src"+Num 1...
                      Store (1#"dst") "tmp"...
                      "dst" $:= "dst"+Num 1...
                      Jump 0,
                9 ==> Exit
         ]
--   `exploit`(exists 0 $ [Num 0 < MetaVar 0, MetaVar 0 < "N", (1#("dst"-MetaVar 0)) /= (1#("src"-MetaVar 0))])
     `exploit`([Num 0 < "?", "?" < "N", (1#("dst"-"?")) /= (1#("src"-"?"))])
--   `exploit`[(1#("dst"-Num 2)) /= (1#("src"-Num 2))]

{-
  if lwb < upb then begin
          let index = any int ensures { lwb <= result <= upb } in
          let pivot = a[index] in
          let i = ref lwb in
          let j = ref upb in
          while !i <= !j do
            while !i < !j && defensive_get a !i <= pivot do
              i := !i + 1
            done;
            while !i <= !j && pivot <= defensive_get a !j do
              j := !j - 1
            done;
            if !i < !j then begin
              swap a !i !j;
              i := !i+1; j := !j-1
            end;
          done
-}

faultyQsort :: (Prog,Predicate)
faultyQsort = ([
                -3  ==> "lwb" $:= Num 0... "upb" $:= Num 2...   -- to reason about 3-element arrays
                        "SP" $:= Num 0...
                        CJump ("lwb" < "upb") 0 9999,

                -2  ==> CJump ("lwb" < "upb") 0 9999,

                -1  ==> CJump ("SP" > "maxSP") 777 (-2),

                {- cut and paste from above; to this more elegantly requires more Prelude -}
                0   ==> "index" ?:= "lwb" <= "index" && "index" <= "upb" ...
                        "pivot" $:= (4#("a" + "index")) ...
                        "i" $:= "lwb" ...
                        "j" $:= "upb" ...
                        Jump 1,

                1   ==> CJump ("i" <= "j") 2 999,   -- outer while

                2   ==> CJump ("i" < "j") 20 4, -- inner while 1
                20  ==> boundcheck 21,
                21  ==> CJump ((4#("a"+"i")) <= "pivot") 3 4,

                3   ==> "i" $:= "i" + Num 1 ...
                        CJump ("i" < "j") 20 4,

                4   ==> CJump ("i" <= "j") 40 6,  -- inner while 2 --
                40  ==> boundcheck 41,
                41  ==> CJump ("pivot" <= (4#("a"+"j"))) 5 6,

                5   ==> "j" $:= "j" - Num 1 ...
                        CJump ("i" <= "j") 40 6,

                6   ==> CJump ("i" < "j") 70 1, -- if statement

                70  ==> boundcheck 7,

                7   ==> "tmp_i" $:= (4#("a"+"i")) ...
                        "tmp_j" $:= (4#("a"+"j")) ...
                        Store (4#("a"+"i")) "tmp_j" ...
                        Store (4#("a"+"j")) "tmp_i" ...
                        "i" $:= "i" + Num 1 ...
                        "j" $:= "j" - Num 1 ...
                        CJump ("i" <= "j") 2 999,

                666 ==> "E_range" $:= Num 1 ...
                        Exit,

                {- end cut and paste from above -}

                777 ==> "E_stack" $:= Num 1 ...
                        Exit,

                -- module the recursive calls; push lwb j, and i upb, and go to the recursive handler
                999 ==> Store (4#("STACK_lwb"+"SP")) "lwb"...
                        Store (4#("STACK_upb"+"SP")) "j"...
                        "SP" $:= "SP" + Num 1 ...
                        Store (4#("STACK_lwb"+"SP")) "i"...
                        Store (4#("STACK_upb"+"SP")) "upb"...
                        "SP" $:= "SP" + Num 1 ...
                        Jump 9999,

                9999 ==> "SP" $:= "SP" - Num 1 ...
                         "lwb" $:= (4#("STACK_lwb"+"SP"))...
                         "upb" $:= (4#("STACK_upb"+"SP"))...
                        CJump ("SP" < Num 0) 99999 (-1),

                99999 ==>
                        "E_range" $:= Num 0 ...
                        "E_stack" $:= Num 0 ...
                        Exit
              ]
        )
     `exploit` (Expr true)
   where boundcheck = CJump ("i" < "lwb" || "i" > "upb" || "j" < "lwb" || "j" > "upb") 666
         -- ign__check = (\x -> Jump true x x)

dice :: (Prog,Predicate)
dice = [
     0 ==> "d" ?:= (Num 1 <= "d" && "d" <= Num 6) ...
           CJump ("d" == Num 6) 0 1,
     1 ==> Exit
 ]
  `exploit` true

dice2 :: (Prog,Predicate)
dice2 = [
     0 ==> "d" ?:= (Num 1 <= "d" && "d" <= Num 6) ... CJump ("d" == Num 6) 1 3,
     1 ==> "d" ?:= (Num 1 <= "d" && "d" <= Num 6) ... CJump ("d" == Num 6) 2 3,
     2 ==> "won" $:= Num 0...Exit,
     3 ==> "won" $:= Num 1...Exit
 ]
  `exploit` ("won" == Num 1)

primeNess :: (Prog,Predicate)
primeNess = [
     0 ==> "d" ?:= (Num 1 < "d" && "d" < "N") ...
           CJump ("N" `mod` "d" == "0") 1 0,
     1 ==> Exit
 ]
  `exploit` true


primeNess2 :: (Prog,Predicate)
primeNess2 = [
    -1 ==> "B" $:= true...Jump 0,
     0 ==> "d" $:= (1#"r")...
           "r" $:= "r"+Num 1...
           "B" $:= ("B" && (Num 1 < "d" && "d" < "N")) ...
           CJump ("N" `mod` "d" == "0") 1 0,
     1 ==> Exit
 ]
  `exploit` (Var "B")

-- to keep the example small, the stackpointer modifications done by call/ret are omitted (see the "STACK OPERATIONS" below which do that)
ropExampleInd :: (Prog, Predicate)
ropExampleInd = [
   -1 ==> "return_address" $:= Num 99 ...
          "rdx" ?:= true ...                     -- we have no control over rdx
          Store (8#"rsp") "return_address"...    -- (prelude: this is done by the call instruction)
          Jump 0,

    0 ==> Assign "rsp" ("rsp" - Num 40)...       -- sub rsp, 40
          CJump ("rdi" > Num 10) 2 1,             -- cmp rdi, 10 / ja label
    1 ==> Store (8#("rsp"+"rdi"*Num 4)) "rdx"... -- mov [rsp+rdi*4], rdx
          Jump 2,
    2 ==> Assign "rsp" ("rsp" + Num 40)...       -- add rsp, 40
          IndirectJump (toExpr (8#"rsp")),

  99  ==> "evil" $:= Num 0 ... Exit,
  1337==> "evil" $:= Num 1 ... Exit              -- this should be unreachable code
 ]
  `exploit` ("evil"==Num 1)

-- STACK MANIPULATION

push :: ToExpr e => e -> B -> B
push e b = "SP" $:= "SP" - Num 8 ... "_" $:= toExpr e ... Store (8#"SP") "_" ... b

pop :: V -> B -> B
pop v b = v $:= (8#"SP") ... "SP" $:= "SP" + Num 8 ... b

call :: L -> L -> B
call l ret = push (Num ret) $ Jump l

ret :: B
ret = pop "_" $ IndirectJump (Var "_")

{- this simulates what the C function gets does -}
getsExample :: (Prog, Predicate)
getsExample = [
    0 ==> call 1 (-1),
    1 ==> "n" ?:= Num 0 <= "n" ...
          "i" $:= Num 0 ...
          CJump ("i" < "n") 2 3,
    2 ==> "input" ?:= true ...
          Store (8#"i") "input" ...
          "i" $:= "i" + Num 1 ...
          CJump ("i" < "n") 2 3,
    3 ==> "input" $:= Num 0 ...
          Store (8#"i") "input"...
          ret,

   42 ==> "exploited" $:= Num 1 ... Exit,
   -1 ==> "exploited" $:= Num 0 ... Exit
 ]
  `exploit` ("exploited" == Num 1)

-- EXAMPLE OF "HIGHER LEVEL JUMP"

qpartW :: (Prog,Predicate)
qpartW = (jlang code, toPred postcondition)
  where
    code = Do [
              Stm$ "lwb" $:= Num 0,
              Stm$ "upb" $:= Num 2,
              Stm$ "index" ?:= ("lwb" <= "index" && "index" <= "upb"),
              Stm$ "pivot" $:= (4#"a"+"index"),
              Stm$ "i" $:= "lwb",
              Stm$ "j" $:= "upb",
              While ("i" <= "j") $ Do [
                While ("i" < "j" && (4#"a"+"i") <= "pivot") $ Do [
                  Stm$ "i" $:= "i" + Num 1
                ],
                While ("i" <= "j" && "pivot" <= (4#"a"+"j")) $ Do [
                  Stm$ "j" $:= "j" - Num 1
                ],
                If ("i" <= "j") `thenDo` [
                  swap ("a"+"i") ("a"+"j"),
                  Stm$ "i" $:= "i" + Num 1,
                  Stm$ "j" $:= "j" - Num 1
                ] `elseDo` []
              ]
            ]

    swap x y  = Do [ Stm$ "tmp_x" $:= (4#x),
                     Stm$ "tmp_y" $:= (4#y),
                     Stm$ Store (4#x) "tmp_y",
                     Stm$ Store (4#y) "tmp_x"
                ]

    postcondition = "i" == "lwb"

breakExample :: (Prog,Predicate)
breakExample = (jlang code, toPred postcondition)
  where
    code = Do [
              Stm$ "i" $:= Num 0,
              While true $ Do [
                 Stm$ "i" $:= "i" + Num 1,
                 If ("i"==Num 5) `thenDo` [Goto "break"] `elseDo` []
              ],
              Label "break" $
              Do []
            ]
    postcondition = "i" >= Num 5

gambler :: (Prog,Predicate)
gambler = (jlang code, toPred postcondition)
  where
    code = Do [
              {- conjure X into existence; otherwise the interpreter aborts -}
              Stm $ "X" ?:= true,
              {- make sure i gets set correctly in the first loop -}
              Stm $ "i" $:= Num (-1),
              Do [
                  {- update amount of unique numbers found && initialize memory cell -}
                  Stm$ (++) "i",
                  {- pick a random number between 0 and 1000 -}
                  Stm$ "r" ?:= Num 0 <= "r" && "r" <= Num 1000,
                  {- see if it is in the list -}
                  Stm$ "j" $:= Num 0,
                  While ("j" < "i" && not ((4#"X"+"j")=="r")) $ Do [Stm$(++)"j"],
                  {- j either points to the index of r, or to the next location it should be added -}
                  Stm$ Store (4#"X"+"j") "r"
              ] `Until` ("j" < "i")
              {- we found a duplicate! -}
            ]

    postcondition = true

    (++) var = var $:= var + Num 1

-- SOUNDNESS TEST

hll :: Prog -> W
hll (init, blocks) = code
  where code = Do [ Goto (show init), Label "Exit" $ If (not true) `thenDo` [Label (show n) (Do$toWhile b) | (n,b) <- M.toList blocks] `elseDo` []]
        toWhile (Seq s b) = Stm s : toWhile b
        toWhile (CJump e l1 l2) = [If e (Goto (show l1)) (Goto (show l2))]
        toWhile Exit = [Goto "Exit"]
        toWhile (IndirectJump _) = error "cannot express IndirectJumps in WhileGoto"

smudge :: (Prog,Predicate) -> (Prog,Predicate)
smudge (prog,post) = ((jlang.hll) prog, post)