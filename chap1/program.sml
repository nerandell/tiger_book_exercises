fun max(a,b) = if a>b then a else b;

fun maxargs_stm stm = 
let fun maxargs_exp exp = 
    case exp of (EseqExp(stm,exp)) => max(maxargs_stm(stm), maxargs_exp(exp)) 
    | _ => 0 
in 
    case stm of (CompoundStm(stm1, stm2)) => max(maxargs_stm(stm1), maxargs_stm(stm2))
    | (AssignStm(id, exp)) => max(maxargs_exp(exp), 0)
    | (PrintStm([])) => 0
    | (PrintStm(exp_list)) => max(maxargs_exp(hd(exp_list)), max(maxargs_stm(PrintStm(tl(exp_list))), length(exp_list)))
end;

fun maxargs stm = maxargs_stm(stm);
