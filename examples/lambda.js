const Zero = (f) => (x) => x
const Id = (x) => x
const Const = k => _ => k
// SUCC := λn.λf.λx.f (n f x)
const Succ = (n) => (f) => (x) => f (n (f) (x))

const Rev = x => g => h => h (g (x))
const Pred = (n) => (f) => (x) => n (Rev (f)) (Const(x)) (Id)

const One     = Succ(Zero)
const Two     = Succ(One)
const Three   = Succ(Two)
const Four    = Succ(Three)
const Five    = Succ(Four)
const Six     = Succ(Five)
const Seven   = Succ(Six)
const Eight   = Succ(Seven)
const Nine    = Succ(Eight)
const Ten     = Succ(Nine)

const Plus = (m) => (n) => m (Succ) (n)
const Mult = (m) => (n) => m (Plus (n)) (Zero)

const PrintNum = (n) => console.log(n ((x) => x + 1) (0))

// Bools

const True = (x) => (y) => x
const False = (x) => (y) => y

const And = (p) => (q) => p (q) (p)
const Or = (p) => (q) => p (p) (q)
const Not = (p) => p (False) (True)

const PrintBool = (p) => console.log(p ("True") ("False"))

// List



PrintNum (Seven)

PrintNum (Plus (Six) (Three))
PrintNum (Mult (Six) (Seven))

PrintBool (Not (Or (False) (True)))

PrintNum(Pred (Zero))
PrintNum(Pred (One))
PrintNum(Pred (Five))
PrintNum(Pred (Ten))