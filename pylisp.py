#!/usr/bin/env python
#a mini lisp-ish language interpreter
import sys
sys.setrecursionlimit(10000)
import string
__all__ = ["compile","evaluate"]
SymbolTable = {}
def iff(x,y,z):
    if evaluate(x): return evaluate(y)
    else: return evaluate(z)
def printf(*args):
    for x in map(evaluate,args):
        print x,
    print 
    return x
def setq(x,y):
    SymbolTable[x] = evaluate(y)
    return SymbolTable[x]
def incf(x,y):
    SymbolTable[x] = add(evaluate(x),evaluate(y))
    return SymbolTable[x]
def clone_sym_table(st):
    result = {}
    for k,v in st.items(): result[k] = v
    return result
def lambdaf(args,*blocks):
    global SymbolTable
    closed_over_sym = clone_sym_table(SymbolTable)
    def anon(*fargs):
        if len(args) != len(fargs): raise Exception("Expected %s args. Got %s" % (len(args),len(fargs)))
        fargs = map(evaluate,fargs) # Its important to evaluate function arguments *before* updating the symbol table
        global SymbolTable
        SymbolTable,local_sym = closed_over_sym,SymbolTable
        for k,v in local_sym.items():
            if k not in SymbolTable: SymbolTable[k] = v
        backup = {}
        result = None
        for arg,farg in zip(args,fargs):
            if arg in SymbolTable: backup[arg] = SymbolTable[arg]
            SymbolTable[arg] = farg
        for block in blocks:
            result = evaluate(block)
        for b in backup: SymbolTable[b] = backup[b]
        SymbolTable = local_sym
        return result
    return anon
def defun(func,args,*blocks): SymbolTable[func] = lambdaf(args,*blocks)
def add(*args): return reduce(lambda x,y:x+y,map(evaluate,args))
def mul(*args): return reduce(lambda x,y:x*y,map(evaluate,args))
def cond(*args):
    for condition in args:
        c,blocks = condition[0],condition[1:]
        result = None
        if evaluate(c):
            for block in blocks:
                result = evaluate(block)
            return result
    return None

#Warning: Ugly hack approaching...
def require(a):
    a = evaluate(a)
    module = a.replace('::','.')
    SymbolTable[a] = __import__(module)
    x = "import " + module
    exec(x)
def sub(a,b): return evaluate(a) - evaluate(b)
def div(a,b): return evaluate(a)/evaluate(b)
def eq(a,b): return evaluate(a)==evaluate(b)
def neq(a,b): return evaluate(a)!=evaluate(b)
# Pre-defined functions go here...
SymbolTable = {
        'null':lambda arg: not ((evaluate(arg) and True) or False)
        ,'t':True ,"add":add ,"+":add ,"-":sub,"sub":sub
        ,"*":mul,"mul":mul ,"/":div,"div":div ,"print":printf
        ,"incf":incf ,"str": lambda x: str(evaluate(x))
        ,"int": lambda x: int(evaluate(x))
        ,"float": lambda x:float(evaluate(x))
        ,">":lambda x,y: evaluate(x)>evaluate(y)
        ,">=":lambda x,y: evaluate(x)>=evaluate(y)
        ,"<":lambda x,y: evaluate(x)<evaluate(y)
        ,"<=":lambda x,y: evaluate(x)<=evaluate(y)
        ,"eq":eq,"=":eq ,"neq":neq,"!=":neq
        ,'if':iff ,'cond':cond ,'setq':setq
        ,'lambda':lambdaf ,'defun':defun
        ,'quote':lambda args: args
        ,'list':lambda *args:list(map(evaluate,args))
        ,'eval':lambda x: evaluate(evaluate(x))
        ,'map':lambda x,y: map(evaluate(x),evaluate(y))
        ,"reduce":lambda x,y: reduce(evaluate(x),evaluate(y))
        ,"filter":lambda x,y: filter(evaluate(x),evaluate(y))
        ,'car':lambda args: evaluate(args)[0]
        ,'cdr':lambda args: evaluate(args)[1:]
        ,'require':require
        }
def is_function(f): return hasattr(f,"func_code") or hasattr(f,"__call__")
def is_list(lst): return type(lst) is list or type(lst) is tuple
def is_quoted(i): return i.startswith('"') and i.endswith('"')
def unquote(i): return i[1:-1]
def is_num(i): return type(i) is float or type(i) is int
def decorate_evaluate(function):
    def func(*args):
        args = map(evaluate,args)
        return function(*args)
    return func
def func_from_module(f):
    try:
        func = eval(f.replace("::","."))
        if not func: raise Exception('')
        if is_function(func): func = decorate_evaluate(func)
        return func
    except Exception,e:
        sys.stderr.write("func_from_module:%s\n"%str(e))
        return None
def evaluate(lst):
    if not is_list(lst): 
        if is_num(lst): return lst
        if is_quoted(lst): return unquote(lst)
        if lst in SymbolTable: return SymbolTable[lst]
        func = func_from_module(lst)
        if func: return func
        raise Exception("Invalid symbol:" + lst)
    f = lst[0]
    if is_list(f): func = evaluate(f)
    else:
        if f in SymbolTable:
            func = SymbolTable[f]
        else:
            try:
                func = func_from_module(f)
            except:
                raise Exception("%s is not a valid symbol" % (f))
    if not is_function(func): raise Exception("%s is not callable" % (f))
    args = lst[1:]
    return func(*args)
def error(k): return "ERROR"
def oparen(k): return "("
def cparen(k): return ")"
def cmt(k): return ""
class Scanner:
    def __init__(self,input):
        self.token = ""
        self.input = input
        self.counter = 0
        self.ahead= ""
        self.type = ""
    def eat_white_space(self):
        while self.input[self.counter] in string.whitespace:
            self.counter += 1
        self.ahead = self.input[self.counter]
    def is_ident_char(self,ch):
        return ch in string.ascii_letters + ":_-+=./!?*{}<>"
    def NextChar(self):
        self.token += self.input[self.counter]
        self.counter += 1
        self.ahead = self.input[self.counter]
    def ScanNumber(self):
        self.NextChar()
        self.type = int
        while self.ahead in string.digits+'.':
            if self.ahead=='.':
                self.type = float
                self.NextChar()
            while self.ahead in string.digits: self.NextChar()
    def ScanString(self):
        self.type = str
        self.NextChar()
        while self.ahead != '"': self.NextChar()
        self.NextChar()
    def ScanComment(self):
        self.type = cmt
        self.NextChar()
        while self.ahead !='\n': self.NextChar()
        self.NextChar()
    def ScanIdent(self):
        self.type = str
        while self.is_ident_char(self.ahead) or self.ahead in string.digits: self.NextChar()
    def next(self):
        self.token = ""
        if self.ahead in string.whitespace: self.eat_white_space()
        if self.ahead in string.digits: self.ScanNumber()
        elif self.is_ident_char(self.ahead): self.ScanIdent()
        elif self.ahead=="(": 
            self.type = oparen
            self.NextChar()
        elif self.ahead==")": 
            self.type = cparen
            self.NextChar()
        elif self.ahead=='"': self.ScanString()
        elif self.ahead==';': self.ScanComment()
        elif self.ahead=="'":
            self.type = lambda x: "'"
            self.NextChar()
        else: raise Exception("Invalid char:" + self.ahead)
        return self.type(self.token)
    def NextToken(self):
        tok = self.next()
        while tok=="": tok = self.next()
        return tok
def parse_s_expression(scanner):
    result = []
    token = scanner.NextToken()
    while token != ")":
        if token == "(":
            val = parse_s_expression(scanner)
        elif token=="'":
            j = token = scanner.NextToken()
            if token=="(": j = parse_s_expression(scanner)
            val = ['quote',j]
        else:
            val = token
        result.append(val)
        token = scanner.NextToken()
    return result
def parse_s_expression_list(sexpr):
    result = []
    scanner = Scanner(sexpr)
    token = scanner.NextToken()
    while token=="(":
        try:
            result.append(parse_s_expression(scanner))
            token = scanner.NextToken()
        except: break
    return result
compile = parse_s_expression_list
def execute(code):
    result = None
    for c in code: result = evaluate(c)
    return result
### Intepreter code ends here ###
def repl():
    while True:
        sys.stdout.write(">")
        expr = sys.stdin.readline()
        if expr.strip().upper()=="QUIT": break
        try:
            code = compile(expr)
            result = execute(code)
            if result: sys.stdout.write(str(result)+"\n")
        except Exception,e:
            sys.stderr.write("Error:" + str(e)+"\n")
def main():
    if not sys.argv[1:]: repl()
    else:
        txt = open(sys.argv[1]).read()
        code = compile(txt)
        execute(code)
if __name__=="__main__": main()
