(* main.ml *)

open Neuron
open Synapse
open Simulation

let num_neurons = 50
let growth_rate = 5.0
let synapse_threshold = 50.0
let initial_synapse_strength = 0.5
let activity_threshold = 0.2

let init_neurons num =
  let rec aux n acc =
    if n = 0 then acc
    else
      let neuron = {
        id = n;
        position = (Random.float 800., Random.float 600.);
        axon_length = 0.0;
        dendrites = [];
        activity_level = 0.0;
      } in
      aux (n-1) (neuron :: acc)
  in
  aux num []

let rec simulate neurons synapses =
  List.iter (fun neuron -> grow_neuron neuron growth_rate) neurons;
  let new_synapses = List.fold_left (fun acc neuron1 ->
    List.fold_left (fun acc2 neuron2 ->
      match form_synapse neuron1 neuron2 synapse_threshold initial_synapse_strength with
      | Some synapse -> synapse :: acc2
      | None -> acc2
    ) acc neurons
  ) [] neurons in
  let all_synapses = synapses @ new_synapses in
  let pruned_synapses = prune_synapses all_synapses activity_threshold in
  simulate_activity neurons pruned_synapses;
  visualize neurons pruned_synapses;
  Unix.sleepf 0.1; (* Delay for visualization *)
  simulate neurons pruned_synapses

let () =
  let neurons = init_neurons num_neurons in
  let synapses = [] in
  simulate neurons synapses
