(* synapse.ml *)

open Neuron

type synapse = {
  pre_neuron : neuron;
  post_neuron : neuron;
  mutable strength : float;
}

let form_synapse neuron1 neuron2 threshold initial_strength =
  if distance neuron1.position neuron2.position < threshold then
    Some { pre_neuron = neuron1; post_neuron = neuron2; strength = initial_strength }
  else
    None

let prune_synapses synapses activity_threshold =
  List.filter (fun synapse -> synapse.pre_neuron.activity_level > activity_threshold) synapses
