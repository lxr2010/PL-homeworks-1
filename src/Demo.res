module StackMachineWithVariables = {
  type instr = Cst (int) | Add | Mul | Var (int) | Pop | Swap 

  let toString = (inst: instr) : string => switch inst {
    | Cst (i) => "Cst" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Add => "Add"
    | Mul => "Mul"
    | Var (i) => "Var" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Pop => "Pop"
    | Swap => "Swap"
  }

  
  let listToString = (instrs: list<instr>) : string => {
    let listTailToString = (instrs: list<instr>) : string => 
      List.fold_left((str,inst)=>str++";"++toString(inst),"",instrs)
    switch instrs {  
      | list{} => "[]"
      | list{inst} => "[" ++ toString(inst) ++"]"
      | list{inst, ...rest} => "[" ++ toString(inst) ++ listTailToString(rest) ++ "]"
    }
  } 

  // Task 1. interpreter of stack machine with variables
  let rec eval = (instrs, stk) => {
    switch (instrs,stk) {
    | (list{Cst (i), ...rest}, stk) => eval (rest , list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval (rest, list{a+b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval (rest, list{a*b, ...stk})
    | (list{Var (i), ...rest}, stk) => switch stk->Belt.List.get(i) {
      | Some(v) => eval (rest, list{v, ...stk})
      | None => assert false
    }
    | (list{Pop, ...rest}, list{_, ...stk}) => eval (rest, stk)
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval (rest, list{b, a, ...stk})
    | (list{} , stk) => (list{}, stk)
    | _ => assert false
    }
  }

  let interpret = (instrs) => switch eval(instrs, list{}) {
    | (_, list{a, ...stk}) => a 
    | _ => assert false 
  }

} 

module Nameless = {
  type rec expr = 
    | Cst (int)
    | Add (expr, expr) // a + b
    | Mul (expr, expr) // a * b
    | Var (int) 
    | Let (expr, expr)

  let rec toString = (expr:expr) : string => switch expr {
    | Cst (i) => "Cst" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Add (expr1, expr2) => "Add" ++ "(" ++ toString(expr1) ++"," ++ toString(expr2) ++ ")"
    | Mul (expr1, expr2) => "Mul" ++ "(" ++ toString(expr1) ++"," ++ toString(expr2) ++ ")"
    | Var (i) => "Var" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Let (expr1, expr2) => "Let" ++ "(" ++ toString(expr1) ++ "," ++ toString(expr2) ++ ")"
  }

}

// Task 2. Compiler from Nameless expr to stack machine with variables.
module NamelessExprToStackMachineWithVariables = {
  open! StackMachineWithVariables 
  type depthTable = list<int>
  let rec compHelper = (expr: Nameless.expr, depthtable: depthTable, stkdepth:int) : list<instr> => {
    switch (expr,depthtable,stkdepth) {
      | (Nameless.Cst(i),_,_) => list{Cst(i)}
      | (Nameless.Add(expr1, expr2),dt,sd) => Belt.List.concatMany([
        compHelper(expr1,dt,sd),
        compHelper(expr2,dt,sd+1),
        list{Add}
      ])
      | (Nameless.Mul(expr1, expr2),dt,sd) => Belt.List.concatMany([
        compHelper(expr1,dt,sd),
        compHelper(expr2,dt,sd+1),
        list{Mul}
      ])
      | (Nameless.Var(i),dt,sd) => switch dt->Belt.List.get(i) {
        | Some(h) => list{Var(sd - h)}
        | None => assert false 
      }
      | (Nameless.Let(expr1, expr2),dt,sd) => Belt.List.concatMany([
        compHelper(expr1, dt, sd),
        compHelper(expr2, list{sd+1, ...dt}, sd+1),
        list{Swap,Pop}
      ])
    }
  }

  let comp = (expr: Nameless.expr) : list<StackMachineWithVariables.instr> => 
    compHelper(expr, list{}, 0)
}

module Named = {
  type rec expr = Cst(int) | Add(expr, expr) | Mul(expr,expr) | Var(string) | Let(string, expr, expr)
  type env = list<(string,int)>
  let rec toString = (expr:expr) : string => switch expr {
    | Cst (i) => "Cst" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Add (expr1, expr2) => "Add" ++ "(" ++ toString(expr1) ++"," ++ toString(expr2) ++ ")"
    | Mul (expr1, expr2) => "Mul" ++ "(" ++ toString(expr1) ++"," ++ toString(expr2) ++ ")"
    | Var (s) => "Var" ++ "(" ++ s ++ ")"
    | Let (s, expr1, expr2) => "Let" ++ "(" ++ s ++ "," ++ toString(expr1) ++ "," ++ toString(expr2) ++ ")"
  }

}

// Task 3. Design of stack machine with name and its compilers.
module StackMachineWithName = {
  type instr = 
    | Cst(int) 
    | Add 
    | Mul 
    | Store(string) // pop stack top value to name
    | Load(string) // push name's value to stack top
    | Clear(string) // remove record of name from env

  type env = list<(string, int)>

  let toString = (instr: instr) : string => switch instr {
    | Cst(i) => "Cst" ++ "(" ++ Js.Int.toString(i) ++ ")"
    | Add => "Add"
    | Mul => "Mul"  
    | Store(s) => "Store" ++ "(" ++ s ++ ")"
    | Load(s) => "Load" ++ "(" ++ s ++ ")"
    | Clear(s) => "Clear" ++ "(" ++ s ++ ")"
  }

  

  let listToString = (instrs : list<instr>): string => {
    let listTailToString = (instrs : list<instr>) : string => 
      List.fold_left((s, inst:instr)=>s++";"++toString(inst), "", instrs)
    switch instrs {
      | list{} => "[]"
      | list{instr} => "[" ++ toString(instr) ++ "]"
      | list{instr,...rest} => "[" ++ toString(instr) ++ listTailToString(rest) ++"]"
    }
  }


  let assoc = (s: string, env: env) : int =>
    switch env->Belt.List.getAssoc(s,(a, b)=> a==b) {
      | Some(i) => i 
      | None => assert false 
    }
  let removeName = (s: string, env: env) : env => 
    env->Belt.List.removeAssoc(s,(a,b)=> a==b)

  let rec eval = (instrs :list<instr>, stk : list<int> , env: env): list<int> => {
    switch (instrs, stk, env) {
      | (list{Cst(i),...rest}, stk, env) => eval(rest, list{i, ...stk}, env)
      | (list{Add, ...rest}, list{a, b, ...stk}, env) => eval(rest, list{a+b, ...stk}, env)
      | (list{Mul, ...rest}, list{a, b, ...stk}, env) => eval(rest, list{a*b, ...stk}, env)
      | (list{Store(s), ...rest}, list{a, ...stk} , env) => eval (rest, stk, list{(s,a), ...env})
      | (list{Load(s), ...rest}, stk, env) => eval(rest, list{assoc(s,env), ...stk}, env)
      | (list{Clear(s), ...rest}, stk, list{(x,_), ...env}) => {
        assert (s==x)
        eval(rest, stk, env)
        }
      | (list{}, stk, env) => stk
      | _ => {
        Js.log(listToString(instrs))
        Js.log(stk)
        Js.log(env)
        assert false
      }
    }
  }

  let interpret = (instrs) => 
    switch eval(instrs, list{}, list{}) {
      | list{a, ..._} => a 
      | stk => {
        Js.log("Empty stack")
        Js.log(stk)
        assert false 
      }
    }

}

module NamedExprToStackWithName = {
  type depthTable = list<(string,int)>
  open! StackMachineWithName

  // mangling: convert names with identical string but different scope into new, unique names.
  // This makes stack machine with name easier to implement.
  let mangled = (s: string, d:int) : string => {
    let len = Js.String.length(s)
    Js.Int.toString(len)++"_"++s++"_"++Js.Int.toString(d)
  }

  let depth = (s:string, d:depthTable): int => 
    switch d->Belt.List.getAssoc(s,(a,b)=>a==b) {
      | Some(i) => i 
      | None => 0
    }
  
  let updatedDepthtable = (d:depthTable, s:string, v: int) : depthTable =>
    Belt.List.setAssoc(d,s,v,(a, b)=>a==b)

  let rec compHelper = (expr: Named.expr, depthtable: depthTable):list<instr> => 
    switch (expr,depthtable) {
      | (Named.Cst(i),_) => list{Cst(i)}
      | (Named.Add(expr1, expr2),d) => Belt.List.concatMany([
        compHelper(expr1,d),
        compHelper(expr2,d),
        list{Add}
      ])
      | (Named.Mul(expr1, expr2),d) => Belt.List.concatMany([
        compHelper(expr1,d),
        compHelper(expr2,d),
        list{Mul}
      ])
      | (Named.Var(s),d) => list{Load(mangled(s,depth(s,d)))}
      | (Named.Let(s,expr1,expr2),d) => {
        let cur = depth(s,d)
        Belt.List.concatMany([
          compHelper(expr1,d),
          list{Store(mangled(s,cur+1))},
          compHelper(expr2,updatedDepthtable(d,s,cur+1)),
          list{Clear(mangled(s,cur+1))}
        ])
      }
    }

  let comp = (expr: Named.expr) => compHelper(expr,list{})
}

module StackMachineWithNameToWithVariables = {
  open! StackMachineWithVariables
  type depthTable = list<(string,int)>

  type transState = {
    outputRev : list<instr>, // record instructions of stack machine with variables.
    depthtable : depthTable, // record stack depth to compute named variables' distances from stack top
    stkdepth : int  // current stack depth
  }

  // reduce this fuction through input
  let transformer = (state: transState, instr : StackMachineWithName.instr) : transState => {
    let rev = state.outputRev
    let d = state.depthtable
    let sd = state.stkdepth

    // helper function to compute named variables' distance from stack top
    let getDiff = (d: depthTable, sd: int, s: string):int => 
      switch Belt.List.getAssoc(d,s,(a,b)=>a==b) {
        | Some(i) =>  sd - i 
        | None => {
          Js.log("Invalid variable reference to \"" ++ s ++ "\"")
          assert false 
        }
      }

    let (revPart, dNew, sdDelta) = switch instr {
      | StackMachineWithName.Cst(i) => (list{Cst(i)}, d, + 1 )
      | StackMachineWithName.Add => (list{Add}, d, - 1)
      | StackMachineWithName.Mul => (list{Mul}, d, - 1)
      | StackMachineWithName.Store(s) => (list{}, 
        Belt.List.setAssoc(d,s,sd,(a,b)=>a==b),
        0)
      | StackMachineWithName.Load(s) => 
        (list{Var(getDiff(d,sd,s))}, d, + 1)
      | StackMachineWithName.Clear(s) => (list{Pop, Swap},
        Belt.List.removeAssoc(d,s,(a,b)=>a==b),
        -1)
    }
    {outputRev: Belt.List.concat(revPart,rev),
     depthtable: dNew,
     stkdepth: sd + sdDelta}
  }

  let comp = (instrs: list<StackMachineWithName.instr>) : list<instr> => {
    let initState : transState = {outputRev: list{}, depthtable:list{}, stkdepth:0}
    let finalState : transState = Belt.List.reduce(instrs,initState,transformer)
    Belt.List.reverse(finalState.outputRev)
  }
  
}

module Test = {
  let namedExprList = {
    open! Named
    list{
  Let("x",Cst(17),Add(Var("x"),Var("x"))),
  Add(Cst(1),Let("x",Cst(2),Add(Var("x"),Cst(7)))),
  Let("x",Let("x",Cst(1),Var("x")),Let("x",Var("x"),Var("x"))),
  Let("x",Cst(1),Cst(2))
    }
  }

  let namelessExprList = {
    open! Nameless 
    list{
  Let(Cst(17),Add(Var(0),Var(0))),
  Add(Cst(1),Let(Cst(2),Add(Var(0),Cst(7)))),
  Let(Let(Cst(1),Var(0)),Let(Var(0),Var(0))),
  Let(Cst(1),Cst(2))
    }
  }

  let task2Helper = (expr :Nameless.expr) : list<StackMachineWithVariables.instr> => {
    Js.log(Nameless.toString(expr))
    let instrList = NamelessExprToStackMachineWithVariables.comp(expr)
    Js.log("Compiled to " ++ StackMachineWithVariables.listToString(instrList))
    Js.log("\n")
    instrList
  }

  let task1Helper = (instrList : list<StackMachineWithVariables.instr>) : int => {
    Js.log(StackMachineWithVariables.listToString(instrList))
    let val = StackMachineWithVariables.interpret(instrList)
    Js.log("Interpreted to " ++ Js.Int.toString(val))
    Js.log("\n")
    val
  }

  let task3Helper = (expr :Named.expr) : int => {
    Js.log(Named.toString(expr))
    let namedInstrList = NamedExprToStackWithName.comp(expr)
    Js.log("Compiled to stack machine with names: " ++ StackMachineWithName.listToString(namedInstrList))
    let val = StackMachineWithName.interpret(namedInstrList)
    Js.log("Interpreted to " ++ Js.Int.toString(val))
    let variableInstrList = StackMachineWithNameToWithVariables.comp(namedInstrList)
    Js.log("Compiled to stack machine with variables: " ++ StackMachineWithVariables.listToString(variableInstrList))
    let variableVal = StackMachineWithVariables.interpret(variableInstrList)
    Js.log("Interpreted to " ++ Js.Int.toString(variableVal))
    Js.log("\n")
    val
  }

  let run = () => {
    Js.log("Task 2 test:")
    let task2res = List.map(task2Helper, namelessExprList)

    Js.log("\n")

    Js.log("Task 1 test:")
    let task1res = List.map(task1Helper, task2res)

    Js.log("\n")

    Js.log("Task 3 test:")
    let task3res = List.map(task3Helper, namedExprList)

    Js.log("\n")
  }
}

Test.run()