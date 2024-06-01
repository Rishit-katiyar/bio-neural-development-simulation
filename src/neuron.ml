(* neuron.ml *)

type neuron = {
  id : int;
  mutable position : (float * float);
  mutable axon_length : float;
  mutable dendrites : (float * float) list;
  mutable activity_level : float;
}

let distance (x1, y1) (x2, y2) =
  sqrt ((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.)

let grow_neuron neuron growth_rate =
  neuron.axon_length <- neuron.axon_length +. growth_rate;
  let angle = Random.float (2. *. 3.141592653589793) in
  let new_position = 
    (fst neuron.position +. growth_rate *. cos angle, 
     snd neuron.position +. growth_rate *. sin angle) in
  neuron.dendrites <- new_position :: neuron.dendrites
