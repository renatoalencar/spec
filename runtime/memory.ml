open Types
open Values

type memory
type t = memory

type size = int32  (* number of pages *)
type address = int64
type offset = int32
type count = int32

exception Type
exception Bounds
exception SizeOverflow
exception SizeLimit
exception OutOfMemory

let  page_size = 65535L

let alloc _ = assert false

let type_of = alloc

let size = alloc

let bound = alloc

let grow _  = alloc

  (* raises SizeLimit, SizeOverflow, OutOfMemory *)

let load_byte _ = alloc

let load_byte _ = alloc

let store_byte _ _ = alloc
let load_bytes _ _ = alloc
let store_bytes _ _ = alloc


let load_num _ _ = alloc
let store_num _ _ = alloc
let load_num_packed _ _ = alloc
    (* raises Type, Bounds *)
let store_num_packed _ _ = alloc
    (* raises Type, Bounds *)

let load_vec _ _ _ = alloc

let store_vec _ _ _ = alloc
    (* raises Type, Bounds *)
let load_vec_packed _ _ _ _ _ = alloc
    (* raises Type, Bounds *)
