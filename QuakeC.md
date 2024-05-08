# QuakeC

## Functions

Functions are defined this way in the QuakeC VM:
```
Functions table contains 5 entries:
    Num  Builtin  Entrypoint    Name          File  First local  Nb. locals  Nb. args  Args size
      1      yes         -14   spawn  operators.qc            0           0        +0         []
      2      yes         -26    ftos                          0           0        +1        [1]
      3      yes         -27    vtos                          0           0        +1        [1]
      4      yes         -99   print                          0           0        +1        [1]
      5       no          +1    main                         80          11        +0         []
```
Negative entrypoint denotes builtin functions. The entrypoint is either:
- For builtins: an index in the global definitions.
- For regular function: The index of the first statement of the function in the
  statement list of the `dat` file.

Function local variables are actually stored in the global table. The `first
local` field contains the index in the global index of the first local variable.
The local variables will then span from `first local` to
`first local + nb locals`. `nb locals` will take into consideration of the size
of the variable. If the function only use one vector for example, `nb locals`
will be 3.

The arguments of a functions are actually part of the locals. If a function has
arguments, the first argument will be stored at the `first local` address. As
arguments can be ints, floats, string pointers or vectors, the size are provided
for each arguments.

Parameters and return values are stored in special places in the global table.
- Index 1 in the global table is used for return values of either builtin or
  regular functions.
- Index 4, 7, 10, 13, 16, 19, 21 and 24 are for arguments.

Opcode to deal with function calls are `CALLx` and `RETURN`. `x` belongs to
[0;8] and corresponds to the number of arguments. For example, `CALL0` call a
function with no argument and `CALL4` with 4 arguments.

`CALLx` instructions are NOT reponsible for saving parameters in the
aforementioned special addresses. This is done by the compiler using regular
`STORE` instructions. However `CALLx` instructions are responsible for:
- Copying the function parameters stored in the aforementioned special addresses
  to function local variables.
- Saving the PC.
- Changing the PC to the proper address corresponding to the function for
  regular function or calling builtins. How the builtins are called is an
  implementation detail of the VM.

The `RETURN` instruction is responsible for:
- Storing the return value to the aforementioned special address for return
  value.
- Restoring the PC.

## Entities

Entities are stored in the bsp file under the form of a long string containing a
"JSON like" string of characters.

Entities are composed of fields. Once parsed (with `entity.zig`), you get an
array of entities which themselves are string hash maps. Fields are the keys and
the values can either be an integer, a float or a string.

For the QuakeC VM, entities' fields are defined in the `.qc` files this way:
```qc
.float speed;
.vector direction;
```
The initial dot mark them as field. Each field as a type and a name in the `.qc`
file.

All entities have the same fields. Theoretically, each entities have hundreds of
fields. In practice, each entity will only use a few fields.

Most of the fields are defined in `defs.qc` but a field can be defined anywhere.
Once all the `.qc` files are compiled though, the fields definitions are encoded
in the `.dat` file.

Fields are encoded with an index (decided by the compiler) and which is how the
VM identify fields.
```
Fields table contains 217 entries:
    Num      Type                                    Name Index
      1     Float                              modelindex     0
      2    Vector                                  absmin     1
      3     Float                                absmin_x     1
      4     Float                                absmin_y     2
      5     Float                                absmin_z     3
      6    Vector                                  absmax     4
      7     Float                                absmax_x     4
      8     Float                                absmax_y     5
      9     Float                                absmax_z     6
     10     Float                                   ltime     7
```
Fields can share index because vector fields will be decomposed as individual
components (see `absmin`).

The values of the fields are not stored within the global array of the VM but
held separatly in the entities list. The way entities are stored and managed is
implementation defined. It's up to the VM to reconcile the field index and the
actual field of the entity.

Opcodes like `ADDRESS`, `LOAD_*`, `STOREP_*` and `*_ENT` will be used to read
and write those fields from the QuakeC VM. For example, this code:
```qc
entity e = spawn();
e.mem = 10;
```
will translate to:
```
; Function 28 is spawn
    CALL0 28      0       0
; An arbitrary handler identifying the entity is returned.
; Move the returned handler value to 85 
STORE_ENT 1       85      0
; Move the handler value to 81 (useless?)
STORE_ENT 85      81      0
; Load the entity's (81 = e) mem field (59 = .mem) address to 85
  ADDRESS 81      59      85
; Store the immediate (_F) float value in global index 36 (=10)
; to the address (P = Pointer) in 85 (e.mem)
 STOREP_F 36      85      0
```
The content of 85 is opaque to the compiler. This is a VM implementation detail.
The VM has to come up with a scheme which allows it to identify a particular
field of a particular entity with just a u32.
