(* simulation.ml *)

open Neuron
open Synapse
open Graphics

let simulate_activity neurons synapses =
  List.iter (fun neuron ->
    neuron.activity_level <- Random.float 1.0 (* Random activity level for simplicity *)
  ) neurons;
  List.iter (fun synapse ->
    if synapse.pre_neuron.activity_level > 0.5 then
      synapse.strength <- synapse.strength +. 0.01 (* Hebbian plasticity rule *)
  ) synapses

let visualize neurons synapses =
  open_graph " 800x600";
  set_window_title "Neural Development Simulation";
  let rec draw_neurons neurons =
    match neurons with
    | [] -> ()
    | neuron::rest ->
      set_color black;
      fill_circle (int_of_float (fst neuron.position)) (int_of_float (snd neuron.position)) 3;
      List.iter (fun (x, y) ->
        set_color blue;
        moveto (int_of_float (fst neuron.position)) (int_of_float (snd neuron.position));
        lineto (int_of_float x) (int_of_float y)
      ) neuron.dendrites;
      draw_neurons rest
  in
  let rec draw_synapses synapses =
    match synapses with
    | [] -> ()
    | synapse::rest ->
      set_color red;
      moveto (int_of_float (fst synapse.pre_neuron.position)) (int_of_float (snd synapse.pre_neuron.position));
      lineto (int_of_float (fst synapse.post_neuron.position)) (int_of_float (snd synapse.post_neuron.position));
      draw_synapses rest
  in
  clear_graph ();
  draw_neurons neurons;
  draw_synapses synapses;
  synchronize ()
