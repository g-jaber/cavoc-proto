let credit = ref 0
let add n = credit := !credit + n
let sub n = credit := !credit - n
let covers price = !credit >= price
let solvent () = !credit >= 0
