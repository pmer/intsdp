#include "os/c.h"
#include "util/int.h"
#include "util/io.h"
#include "util/measure.h"
#include "util/new.h"
#include "util/string-view.h"
#include "util/vector.h"

// typ.rs
struct Expr;
struct Add {
    Expr* lhs;
    Expr* rhs;
};
struct Assign {
    Size idx;
    Expr* rhs;
};
struct Then {
    Expr* lhs;
    Expr* rhs;
};
struct While {
    Expr* cond;
    Expr* body;
};
enum ExprType { LIT, VAR, ADD, ASSIGN, THEN, WHILE };
struct Expr {
    ExprType type;
    union {
        I64 lit;
        Size var;
        Add add;
        Assign assign;
        Then then;
        While while_;
    };
};

__attribute__((noinline))
I64 add(I64 x, I64 y) {
    return x + y;
}

// stack.rs
template<typename T>
class Stack {
 public:
     T* top;

     void push(T x) {
         *++top = x;
     }

     T pop() {
         return *top--;
     }

     void set(Size offset, T value) {
        top[-offset] = value;
     }

     void get(Size offset) {
         return top[-offset];
     }
};

template<typename T>
class StackOwner {
 public:
     T* items;

     StackOwner(Size size) {
         items = xmalloc(T, size);
     }

     Stack<T> stack() {
         Stack<T> s;
         s.top = items;
         return s;
     }

     T peek(Size i) {
         return items[i];
     }
};

// tape.rs
template<typename T>
class Tape {
 public:
    T* top;

    Tape(T* data) {
        top = data;
    }

    T next() {
        return *top++;
    }

    void jump(SSize offset) {
        top += offset;
    }
};

// alt/raw.rs
I64 rawRun(I64* x0_) {
    I64 x0 = *x0_;
    I64 x1 = 100;
    while (x0) {
        x1 = x1 + 4 + x1 + 3;
        x1 = x1 + 2 + 4;
        x0 = x0 - 1;
    }
    return x1;
}

I64 raw2Run(I64* x0_) {
    I64 x0 = *x0_;
    I64 x1 = 100;
    while (x0) {
        x1 = x1 + x1 + 13;
        x0 = x0 - 1;
    }
    return x1;
}

// alt/poor.rs
I64 poorRun(I64* x0_) {
    I64 x0 = *x0_;
    I64 x1 = 100;
    while (x0) {
        x1 = add(add(add(x1, 4), x1), 3);
        x1 = add(add(x1, 2), 4);
        x0 = add(x0, -1);
    }
    return x1;
}

// alt/ast.rs
I64 astEval(Expr* x, I64* slots) {
    switch (x->type) {
        case LIT: {
            return x->lit;
        }
        case VAR: {
            return slots[x->var];
        }
        case ADD: {
            return add(astEval(x->add.lhs, slots), astEval(x->add.rhs, slots));
        }
        case THEN: {
            astEval(x->then.lhs, slots);
            return astEval(x->then.rhs, slots);
        }
        case ASSIGN: {
            return slots[x->assign.idx] = astEval(x->assign.rhs, slots);
        }
        case WHILE: {
            while (astEval(x->while_.cond, slots)) {
                astEval(x->while_.body, slots);
            }
            return 0;
        }
    }
}
I64 astRun(Expr* x) {
    I64 slots[10] = {};
    return astEval(x, slots);
}

// alt/bytescode.rs
enum BcType { kBcAssign, kBcVar, kBcLit, kBcAdd, kBcJump, kBcJumpIf0, kBcDone };
struct Bytecode {
    BcType type;
    union {
        U32 assign;
        U32 var;
        I32 lit;
        U32 jump;
        U32 jumpIf0;
    };

    Bytecode(BcType type) : type(type) { }
    Bytecode(BcType type, I32 i) : type(type), lit(i) { }
    Bytecode(BcType type, U32 u) : type(type), assign(u) { }
};

Vector<Bytecode>* makeBytecode(I64 x0) {
    U32 prefixLen = 4;
    U32 innerLen = 8 + 6 + 4;

    Vector<Bytecode>* res = new Vector<Bytecode>;

    res->push(Bytecode(kBcLit, static_cast<I32>(x0)));
    res->push(Bytecode(kBcAssign, 0));
    res->push(Bytecode(kBcLit, 100));
    res->push(Bytecode(kBcAssign, 1));

    res->push(Bytecode(kBcVar, 0));

    res->push(
        Bytecode(kBcJumpIf0, static_cast<U32>(res->size + 1 + innerLen + 1))
    );

    res->push(Bytecode(kBcVar, 1));
    res->push(Bytecode(kBcLit, 4));
    res->push(Bytecode(kBcVar, 1));
    res->push(Bytecode(kBcLit, 3));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAssign, 1));

    res->push(Bytecode(kBcVar, 1));
    res->push(Bytecode(kBcLit, 2));
    res->push(Bytecode(kBcLit, 4));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAssign, 1));

    res->push(Bytecode(kBcVar, 0));
    res->push(Bytecode(kBcLit, -1));
    res->push(Bytecode(kBcAdd));
    res->push(Bytecode(kBcAssign, 0));

    res->push(Bytecode(kBcJump, prefixLen));

    res->push(Bytecode(kBcVar, 1));

    res->push(Bytecode(kBcDone));
    return res;
}

class BcStack {
 public:
     Size ptr;
     I64 vals[100];

     BcStack() : ptr(0) {
         memset(vals, 0, sizeof(vals));
     }

     I64 pop() {
         return vals[--ptr];
     }

     void push(I64 x) {
         vals[ptr++] = x;
     }
};

I64 bytecodeRun(Vector<Bytecode>* xs) {
    int slots[10] = {};
    BcStack stack;

    for (Size pc = 0; ; pc++) {
        Bytecode b = (*xs)[pc];
        switch (b.type) {
            case kBcAssign:
                slots[b.assign] = stack.pop();
                break;
            case kBcVar:
                stack.push(slots[b.var]);
                break;
            case kBcLit:
                stack.push(b.lit);
                break;
            case kBcAdd: {
                I64 x = stack.pop();
                I64 y = stack.pop();
                stack.push(add(x, y));
                break;
            }
            case kBcJump:
                pc = b.jump - 1;
                break;
            case kBcJumpIf0:
                if (stack.pop() == 0) {
                    pc = b.jumpIf0 - 1;
                }
                break;
            case kBcDone:
                return stack.pop();
        }
    }
}

// alt/closure.rs
typedef I64 (*ClosureFn)(void* scope, I64* slots);
struct Closure {
    ClosureFn f;
    void* scope;
};
struct LitScope { I64 i; };
struct VarScope { U64 u; };
struct AddScope { Closure l; Closure r; };
struct ThenScope { Closure fst; Closure snd; };
struct AssignScope { U64 u; Closure r; };
struct WhileScope { Closure cond; Closure body; };
I64 litClosure(I64 i, I64*) {
    return i;
};
I64 varClosure(U64 u, I64* slots) {
    return slots[u];
}
I64 addClosure(AddScope* s, I64* slots) {
    return s->l.f(s->l.scope, slots) + s->r.f(s->r.scope, slots);
}
I64 thenClosure(ThenScope* s, I64* slots) {
    s->fst.f(s->fst.scope, slots);
    return s->snd.f(s->snd.scope, slots);
}
I64 assignClosure(AssignScope* s, I64* slots) {
    return slots[s->u] = s->r.f(s->r.scope, slots);
}
I64 whileClosure(WhileScope* s, I64* slots) {
    while (s->cond.f(s->cond.scope, slots)) {
        s->body.f(s->body.scope, slots);
    }
    return 0;
}

Closure compileClosure(Expr* x) {
    Closure c;
    switch (x->type) {
        case LIT: {
            c.f = reinterpret_cast<ClosureFn>(litClosure);
            c.scope = reinterpret_cast<void*>(x->lit);
            break;
        }
        case VAR: {
            c.f = reinterpret_cast<ClosureFn>(varClosure);
            c.scope = reinterpret_cast<void*>(x->var);
            break;
        }
        case ADD: {
            c.f = reinterpret_cast<ClosureFn>(addClosure);
            AddScope* scope = xmalloc(AddScope, 1);
            scope->l = compileClosure(x->add.lhs);
            scope->r = compileClosure(x->add.rhs);
            c.scope = scope;
            break;
        }
        case ASSIGN: {
            c.f = reinterpret_cast<ClosureFn>(assignClosure);
            AssignScope* scope = xmalloc(AssignScope, 1);
            scope->u = x->assign.idx;
            scope->r = compileClosure(x->assign.rhs);
            c.scope = scope;
            break;
        }
        case THEN: {
            c.f = reinterpret_cast<ClosureFn>(thenClosure);
            ThenScope* scope = xmalloc(ThenScope, 1);
            scope->fst = compileClosure(x->then.lhs);
            scope->snd = compileClosure(x->then.rhs);
            c.scope = scope;
            break;
        }
        case WHILE: {
            c.f = reinterpret_cast<ClosureFn>(whileClosure);
            WhileScope* scope = xmalloc(WhileScope, 1);
            scope->cond = compileClosure(x->while_.cond);
            scope->body = compileClosure(x->while_.body);
            c.scope = scope;
            break;
        }
    }
    return c;
}
Closure* makeClosure(Expr* x) {
    return new Closure(compileClosure(x));
}

I64 closureRun(Closure* closure) {
    I64 slots[10] = {};
    return closure->f(closure->scope, slots);
}

// example.rs
Expr* exAssign(Size x, Expr* y) {
    Expr* e = xmalloc(Expr, 1);
    e->type = ASSIGN;
    e->assign.idx = x;
    e->assign.rhs = y;
    return e;
}
Expr* exThens(Vector<Expr*> x) {
    Expr* e = x[x.size - 1];
    for (SSize i = x.size - 2; i >= 0; i--) {
        Expr* f = xmalloc(Expr, 1);
        f->type = THEN;
        f->then.lhs = x[i];
        f->then.rhs = e;
        e = f;
    }
    return e;
}
Expr* exAdds(Vector<Expr*> x) {
    Expr* e = x[x.size - 1];
    for (SSize i = x.size - 2; i >= 0; i--) {
        Expr* f = xmalloc(Expr, 1);
        f->type = ADD;
        f->add.lhs = x[i];
        f->add.rhs = e;
        e = f;
    }
    return e;
}
Expr* exLit(I64 x) {
    Expr* e = xmalloc(Expr, 1);
    e->type = LIT;
    e->lit = x;
    return e;
}
Expr* exWhile(Expr* x, Expr* y) {
    Expr* e = xmalloc(Expr, 1);
    e->type = WHILE;
    e->while_.cond = x;
    e->while_.body = y;
    return e;
}
Expr* exVar(Size x) {
    Expr* e = xmalloc(Expr, 1);
    e->type = VAR;
    e->var = x;
    return e;
}

Expr* exAst(I64 x0) {
    Vector<Expr*> three;
    three.push(exVar(1));
    three.push(exLit(4));
    three.push(exVar(1));
    three.push(exLit(3));

    Vector<Expr*> four;
    four.push(exVar(1));
    four.push(exLit(2));
    four.push(exLit(4));

    Vector<Expr*> five;
    five.push(exVar(0));
    five.push(exLit(-1));

    Vector<Expr*> two;
    two.push(exAssign(1, exAdds(three)));
    two.push(exAssign(1, exAdds(four)));
    two.push(exAssign(0, exAdds(five)));

    Vector<Expr*> one;
    one.push(exAssign(0, exLit(x0)));
    one.push(exAssign(1, exLit(100)));
    one.push(exWhile(exVar(0), exThens(two)));
    one.push(exVar(1));

    return exThens(one);
}

// lib.rs
class Entry {
 public:
    const char* name;
    void* arg;
    I64 (*run)(void*);

    template<typename T>
    Entry(const char* name, T* x, I64 (*f)(T*)) {
        this->name = name;
        arg = x;
        run = reinterpret_cast<I64 (*)(void*)>(f);
    }

    // black_box
    __attribute__((noinline))
    I64 apply() {
        return run(arg);
    }
};

Vector<Entry> entries(I64 repeat) {
    Expr* ast = exAst(repeat);

    I64* r = xmalloc(I64, 1);
    *r = repeat;

    Vector<Entry> es;
    es.push(Entry("raw", r, rawRun));
    es.push(Entry("raw2", r, raw2Run));
    es.push(Entry("poor", r, poorRun));
    es.push(Entry("ast", ast, astRun));
    es.push(Entry("bytecode", makeBytecode(repeat), bytecodeRun));
    es.push(Entry("closure", makeClosure(ast), closureRun));
    //es.push(Entry("jumpStack", jumpStackCompile(ast), jumpStackRun));
    //es.push(Entry("jumpRegister", jumpRegisterCompile(ast), jumpRegisterRun));
    //es.push(Entry("jumpRegisterLoop", jumpRegisterLoopCompile(ast), jumpRegisterLoopRun));
    //es.push(Entry("jumpRegisterCompact", jumpRegisterCompactCompile(ast), jumpRegisterCompactRun));
    //es.push(Entry("jumpRegisterCompactLoop", jumpRegisterCompactLoopCompile(ast), jumpRegisterCompactLoopRun));
    return es;
}

// bench/main.rs
void benchmark() {
    Flusher f1(sout);
    Flusher f2(serr);

    Vector<Entry> es = entries(1000 * 1000);

    Vector<I64> results;
    for (Entry& e : es) {
        TimeMeasure m(e.name);
        results.push(e.apply());
    }
    for (I64 r : results) {
        sout << r << ' ';
    }
    sout << '\n';
}

// main.rs
int main() {
    Flusher f1(sout);
    Flusher f2(serr);

    for (I32 repeat = 2; repeat < 5; repeat++) {
        Vector<Entry> es = entries(repeat);

        Vector<I64> results;
        for (Entry& e : es) {
            results.push(e.apply());
        }
        for (I64 r : results) {
            sout << r << ' ';
        }
        sout << '\n';
    }

    benchmark();
    return 0;
}
