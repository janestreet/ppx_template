open! Stdppx
open! Import

val module_binding : loc:Location.t -> module_binding -> structure_item
val module_declaration : loc:Location.t -> module_declaration -> signature_item
