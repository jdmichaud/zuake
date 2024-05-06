// Interpret QuakeC bytecode.
//
// reference:
//  https://github.com/graphitemaster/gmqcc
//  http://ouns.nexuizninjaz.com/dev_quakec.html
//
// cmd: clear && zig build-exe -freference-trace qvm.zig && ./qvm ../data/pak/progs.dat

const std = @import("std");
const misc = @import("misc.zig");
const datModule = @import("dat.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

fn bitCast(comptime T: type, v: anytype) T {
  return @bitCast(v);
}

fn intCast(comptime T: type, v: anytype) T {
  switch (@TypeOf(v)) {
    f16, f32, f64, f80, f128 => return @intFromFloat(v),
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128 => return @intCast(v),
    else => unreachable,
  }
}

// Special entry in the global index
// There are spaced by 3 bytes so that they can take vectors.
// The calling convention is as follows:
// - Statement can only take 3 parameters thus CALL cannot pass 8 parameters, so
//   the compiler will emit STORE instructions to put value inside the Parameters
//   starting at 0x04
// - The CALL instruction will copy those parameters to the function locals
//   starting with the first local defined in the Function structure.
//   Except for builtins which handles their parameters by themselves.
// - When the function ends, it emits a RETURN instruction passing the address
//   in the memory of the returned value. The RETURN instruction copies that
//   value in ReturnValue (0x01).
// - The compiler will then emit a STORE instructions to retrieve the returned
//   value from ReturnValue (0x01) and put it in a function local.
const CallRegisters = enum(u8) {
  ReturnValue = 0x01,
  Parameter1 = 0x04,
  Parameter2 = 0x07,
  Parameter3 = 0x0a,
  Parameter4 = 0x0d,
  Parameter5 = 0x10,
  Parameter6 = 0x13,
  Parameter7 = 0x16,
  Parameter8 = 0x19,
};

const ParameterList = .{
  CallRegisters.Parameter1,
  CallRegisters.Parameter2,
  CallRegisters.Parameter3,
  CallRegisters.Parameter4,
  CallRegisters.Parameter5,
  CallRegisters.Parameter6,
  CallRegisters.Parameter7,
  CallRegisters.Parameter8,
};

pub const RuntimeError = struct {
  const Self = @This();

  pc: i32 = 0,
  message: [255:0]u8 = [_:0]u8{ 0 } ** 255,
};

const Builtins = struct {
  pub fn call(vm: *VM, index: u32, argc: u32) !void {
    switch (index) {
      1 => @panic("makevectors is not yet implemented"),
      2 => @panic("setorigin is not yet implemented"),
      3 => @panic("setmodel is not yet implemented"),
      4 => @panic("setsize is not yet implemented"),
      6 => @panic("break is not yet implemented"),
      7 => @panic("random is not yet implemented"),
      8 => @panic("sound is not yet implemented"),
      9 => @panic("normalize is not yet implemented"),
      10 => @panic("error is not yet implemented"),
      11 => @panic("objerror is not yet implemented"),
      12 => @panic("vlen is not yet implemented"),
      13 => @panic("vectoyaw is not yet implemented"),
      14 => @panic("spawn is not yet implemented"),
      15 => @panic("remove is not yet implemented"),
      16 => @panic("traceline is not yet implemented"),
      17 => @panic("checkclient is not yet implemented"),
      18 => @panic("find is not yet implemented"),
      19 => { // precache_sound
        // Only try to open a file and prints with a warning if it fails
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        const data = misc.load(path) catch |err| {
          try stderr.print("warning: {}, trying to open open {s}\n", .{ err, path });
          return;
        };
        vm.registerFile(path, data);
      },
      20 => @panic("precache_model is not yet implemented"),
      21 => @panic("stuffcmd is not yet implemented"),
      22 => @panic("findradius is not yet implemented"),
      23 => @panic("bprint is not yet implemented"),
      24 => @panic("sprint is not yet implemented"),
      25 => { // dprint
        try Builtins.call(vm, 99, argc); // => print
      },
      26 => { // ftos
        const f = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
        const strPointer = try vm.pushString("{d}", .{ f });
        vm.mem32[@intFromEnum(CallRegisters.ReturnValue)] = @intCast(strPointer);
      },
      27 => { // vtos
        const v_x = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)    ]);
        const v_y = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1) + 1]);
        const v_z = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1) + 2]);
        const strPointer = try vm.pushString("'{d} {d} {d}'", .{ v_x, v_y, v_z });
        vm.mem32[@intFromEnum(CallRegisters.ReturnValue)] = @intCast(strPointer);
      },
      28 => { // coredump
        try stderr.print("warning: coredump: not connected to any entities yet\n", .{});
      },
      29 => @panic("traceon is not yet implemented"),
      30 => @panic("traceoff is not yet implemented"),
      31 => @panic("eprint is not yet implemented"),
      32 => @panic("walkmove is not yet implemented"),
      34 => @panic("droptofloor is not yet implemented"),
      35 => @panic("lightstyle is not yet implemented"),
      36 => @panic("rint is not yet implemented"),
      37 => @panic("floor is not yet implemented"),
      38 => @panic("ceil is not yet implemented"),
      40 => @panic("checkbottom is not yet implemented"),
      41 => @panic("pointcontents is not yet implemented"),
      43 => @panic("fabs is not yet implemented"),
      44 => @panic("aim is not yet implemented"),
      45 => @panic("cvar is not yet implemented"),
      46 => @panic("localcmd is not yet implemented"),
      47 => @panic("nextent is not yet implemented"),
      48 => @panic("particle is not yet implemented"),
      49 => @panic("ChangeYaw is not yet implemented"),
      51 => @panic("vectoangles is not yet implemented"),
      52 => @panic("WriteByte is not yet implemented"),
      53 => @panic("WriteChar is not yet implemented"),
      54 => @panic("WriteShort is not yet implemented"),
      55 => @panic("WriteLong is not yet implemented"),
      56 => @panic("WriteCoord is not yet implemented"),
      57 => @panic("WriteAngle is not yet implemented"),
      58 => @panic("WriteString is not yet implemented"),
      59 => @panic("WriteEntity is not yet implemented"),
      67 => @panic("movetogoal is not yet implemented"),
      68 => { // precache_file
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        try stderr.print("warning: precache_file {s} ignored\n", .{ path });
        // Apparently, this function does nothing.
        // Returns the string pass to it.
        vm.mem32[@intFromEnum(CallRegisters.ReturnValue)] = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
      },
      69 => @panic("makestatic is not yet implemented"),
      70 => @panic("changelevel is not yet implemented"),
      72 => @panic("cvar_set is not yet implemented"),
      73 => @panic("centerprint is not yet implemented"),
      74 => @panic("ambientsound is not yet implemented"),
      75 => @panic("precache_model2 is not yet implemented"),
      76 => @panic("precache_sound2 is not yet implemented"),
      77 =>  { // precache_file2
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        try stderr.print("warning: precache_file2 {s} ignored\n", .{ path });
        // Apparently, this function does nothing.
        // Returns the string pass to it.
        vm.mem32[@intFromEnum(CallRegisters.ReturnValue)] = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
      },
      78 => @panic("setspawnparms is not yet implemented"),
      85 => @panic("stov is not yet implemented"),
      99 => { // print
        var str = [_]u8{ 0 } ** 1024;
        var head: usize = 0;
        inline for (ParameterList) |parameter| {
          const strOffset = vm.mem32[@intFromEnum(parameter)];
          if ((@intFromEnum(parameter) - @intFromEnum(CallRegisters.Parameter1)) / 3 < argc) {
            const written = try std.fmt.bufPrint(str[head..], "{s}", .{
              vm.getString(strOffset),
            });
            head += written.len;
            if (head > 1024) {
              return error.StringToLong;
            }
          }
        }
        try stdout.print("{s}", .{ str[0..head] });
      },
      else => @panic("unknow builtin id"),
    }
  }
};

const VM = struct {
  const Self = @This();

  dat: ?datModule.Dat,

  // Dat Memory layout        VM memory layout
  // 0-------------------
  //  strings
  //  -------------------     0------------------
  //  globals (ro)             globals (rw)
  //  -------------------      ------------------ <- stringsOffset
  //  statements               dynamic strings    <- stringsPointer somewhere in between
  //  -------------------      ------------------ <- stackLimit
  //  fields                   stack              <- sp somewhere
  //  -------------------      ------------------
  //  functions
  //  -------------------
  mem: []u8,
  // Same as mem but in 32bits for convenience
  mem32: []u32,
  // Stack Pointer
  // Index in mem32 (so incremented by 1)
  sp: usize,
  // Dynamic string data
  stringsOffset: usize,
  // Pointer to the next available dynamic string memory
  stringsPointer: usize,
  // Upper limit of the stack. If we reach it, it means the stack is overflown.
  stacklimit: usize,
  // Program Counter
  // This is not an offset in memory but an index in the statement list
  pc: usize,

  pub fn init(allocator: std.mem.Allocator) !Self {
    // We allocate a fixed amount of memory (TODO: make it configurable)
    // We assume that the globals fit within the memory with a little room for the stack
    const mem32 = try allocator.alloc(u32, 256 * 1024 * 1); // 1 Mb for now
    // Keep a []32 slice around for convenience
    const mem: []u8 = std.mem.sliceAsBytes(mem32);
    // Stacks starts at the end and goes down
    const sp = mem32.len - 1;

    return Self{
      .dat = null,
      .mem = mem,
      .mem32 = mem32,
      .sp = sp,
      .stringsOffset = 0,
      .stringsPointer = 0,
      .stacklimit = 0,
      .pc = 0,
    };
  }

  pub fn load(self: *Self, dat: datModule.Dat) void {
    // Check that the global are less than the allocated memory and reserving
    // 1K for dynamic string data and 1K for the stack.
    std.debug.assert(dat.globals.len * @sizeOf(u32) < self.mem.len - 1024 * 2);
    // Load globals
    @memcpy(self.mem32[0..dat.globals.len], dat.globals);
    // Set boundary pointers
    self.stringsOffset = dat.globals.len;
    self.stringsPointer = dat.globals.len;
    self.stacklimit = dat.globals.len * @sizeOf(u32) + 1024;
    self.dat = dat;
  }

  pub fn jumpToFunction(self: *Self, functionName: []const u8) !void {
    const function = blk: for (self.dat.?.definitions) |def| {
      if (def.getType() == datModule.QType.Function
        and std.mem.eql(u8, self.dat.?.getString(def.nameOffset), functionName)) {
        break :blk self.dat.?.getFunction(def);
      }
    } else {
      return error.NoMainFunction;
    };

    if (function == null) {
      return error.MainIsNotAFunction;
    }

    if (function.?.entryPoint < 0) {
      return error.MainIsABuiltin;
    }

    self.pc = @intCast(function.?.entryPoint);
  }

  pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.mem);
    self.* = undefined;
  }

  // Push a string to the dynamic string pile
  pub fn pushString(self: *Self, comptime fmt: []const u8, args: anytype) !usize {
    const pointer = self.stringsPointer;
    self.stringsPointer += (try std.fmt.bufPrintZ(self.mem[self.stringsPointer * @sizeOf(u32)..], fmt, args)).len;
    // We need to return an address that is beyond the read only string
    // address of the DAT file so that we can distinguish were the string
    // is stored.
    return pointer + self.dat.?.header.stringsOffset + self.dat.?.header.stringsSize;
  }

  // Get a string either from the static string area of the dynamic one.
  pub fn getString(self: Self, offset: u32) [:0]const u8 {
    const datStringBoundary = self.dat.?.header.stringsOffset + self.dat.?.header.stringsSize;
    // In order to manage dynamic strings we have two different memory area for strings.
    // The static strings that comes from the dat file are below the string offset + string size.
    // The dynamic strings are above the string offset + string size.
    if (offset < datStringBoundary) {
      return self.dat.?.getString(offset);
    }
    // We still need to subtract the string boundary.
    return std.mem.span(@as([*:0]const u8, @ptrCast(self.mem32[offset - datStringBoundary..])));
  }

  pub fn registerFile(self: *Self, path: []const u8, data: []const u8) void {
    // Do nothing for now
    _ = self;
    _ = path;
    _ = data;
  }

  fn call(self: *Self, statement: datModule.Statement, argc: u8, err: *RuntimeError) !void {
    const fnIndex = statement.arg1;

    if (self.dat.?.getFunctionByIndex(fnIndex)) |fun| {
      if (fun.entryPoint > 0) {
        // Copy the parameters onto the function locals
        var offset: u32 = 0;
        for (0..argc) |i| {
          const paramAddress = @intFromEnum(CallRegisters.Parameter1);
          @memcpy(
            self.mem32[fun.firstLocal + offset..fun.firstLocal + offset + fun.argSizes[i]],
            self.mem32[paramAddress + i * 3..paramAddress + i * 3 + fun.argSizes[i]],
          );
          offset += fun.argSizes[i];
        }
        // Save the the string pointer and the PC
        self.mem32[self.sp] = @intCast(self.pc);
        self.sp -= 1;
        self.mem32[self.sp] = @intCast(self.stringsPointer);
        self.sp -= 1;
        self.pc = @intCast(fun.entryPoint);
      } else {
        // No saving the PC because the builtin will not change it
        try Builtins.call(self, @abs(fun.entryPoint), argc);
        self.pc += 1;
      }
    } else {
      _ = try std.fmt.bufPrint(&err.message, "No function with index {}", .{ fnIndex });
      return error.RuntimeError;
    }
  }

  pub fn execute(self: *Self, err: *RuntimeError) !bool {
    const statement = self.dat.?.statements[self.pc];
    return self.executeStatement(statement, err);
  }

  inline fn executeStatement(self: *Self, statement: datModule.Statement, err: *RuntimeError) !bool {
    switch (statement.opcode) {
      datModule.OpCode.DONE => {
        std.log.debug("DONE {} pc 0x{x} sp 0x{x}", .{ statement.arg1, self.pc, self.sp });
        return true;
      },
      datModule.OpCode.STATE => @panic("STATE unimplemented"),
      datModule.OpCode.GOTO => @panic("GOTO unimplemented"),
      datModule.OpCode.ADDRESS => @panic("ADDRESS unimplemented"),
      datModule.OpCode.RETURN => {
        std.log.debug("RETURN {} pc 0x{x} sp 0x{x}", .{ statement.arg1, self.pc, self.sp });
        const src = statement.arg1;
        if (src != 0) {
          const returnAddress = @intFromEnum(CallRegisters.ReturnValue);
          self.mem32[returnAddress    ] = self.mem32[statement.arg1    ];
          self.mem32[returnAddress + 1] = self.mem32[statement.arg1 + 1];
          self.mem32[returnAddress + 2] = self.mem32[statement.arg1 + 2];
        }
        if (self.sp == self.mem32.len - 1) {
          // The stack is at the bottom of the memory so we are returning from main
          return true;
        }
        // Restore the the string pointer and the PC
        self.sp += 1;
        self.stringsPointer = self.mem32[self.sp];
        self.sp += 1;
        self.pc = self.mem32[self.sp];
        self.pc += 1;
        return false;
      },
      // Arithmetic Opcode Mnemonic
      datModule.OpCode.MUL_F => @panic("MUL_F unimplemented"),
      datModule.OpCode.MUL_V => @panic("MUL_V unimplemented"),
      datModule.OpCode.MUL_FV => @panic("MUL_FV unimplemented"),
      datModule.OpCode.MUL_VF => @panic("MUL_VF unimplemented"),
      datModule.OpCode.DIV_F => @panic("DIV_F unimplemented"),
      datModule.OpCode.ADD_F => {
        std.log.debug("ADD_F, dst {} = lhs {} - rhs {} - pc {}",
          .{ statement.arg3, statement.arg1, statement.arg2, self.pc });

        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.mem32[dst] = bitCast(u32, bitCast(f32, self.mem32[lhs]) + bitCast(f32, self.mem32[rhs]));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.ADD_V => @panic("ADD_V unimplemented"),
      datModule.OpCode.SUB_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.mem32[dst] = bitCast(u32, bitCast(f32, self.mem32[lhs]) - bitCast(f32, self.mem32[rhs]));
        self.pc += 1;

        std.log.debug("SUB_F, dst {} ({d}) = lhs {} ({d}) - rhs {} ({d}) - pc {}", .{
          statement.arg3, bitCast(f32, self.mem32[dst]),
          lhs, bitCast(f32, self.mem32[lhs]),
          rhs, bitCast(f32, self.mem32[rhs]), self.pc,
        });
        return false;
      },
      datModule.OpCode.SUB_V => {
        std.log.debug("SUB_V, dst {} = lhs {} - rhs {} - pc {}",
          .{ statement.arg3, statement.arg1, statement.arg2, self.pc });

        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.mem32[dst    ] = bitCast(u32, bitCast(f32, self.mem32[lhs    ]) - bitCast(f32, self.mem32[rhs    ]));
        self.mem32[dst + 1] = bitCast(u32, bitCast(f32, self.mem32[lhs + 1]) - bitCast(f32, self.mem32[rhs + 1]));
        self.mem32[dst + 2] = bitCast(u32, bitCast(f32, self.mem32[lhs + 2]) - bitCast(f32, self.mem32[rhs + 2]));
        self.pc += 1;
        return false;
      },
      // Comparison Opcode Mnemonic
      datModule.OpCode.EQ_F => {
        if (self.mem32[statement.arg1] == self.mem32[statement.arg2]) {
          self.mem32[statement.arg3] = bitCast(u32, @as(f32, @floatFromInt(1)));
        } else {
          self.mem32[statement.arg3] = 0;
        }
        self.pc += 1;

        std.log.debug("EQ_F, dst {} - lhs {} (0x{x}) - rhs {} (0x{x}) - pc {}", .{
          statement.arg3, statement.arg1, self.mem32[statement.arg1],
          statement.arg2, self.mem32[statement.arg2], self.pc,
        });
        return false;
      },
      datModule.OpCode.EQ_V => @panic("EQ_V unimplemented"),
      datModule.OpCode.EQ_S => @panic("EQ_S unimplemented"),
      datModule.OpCode.EQ_E => @panic("EQ_E unimplemented"),
      datModule.OpCode.EQ_FNC => @panic("EQ_FNC unimplemented"),
      datModule.OpCode.NE_F => @panic("NE_F unimplemented"),
      datModule.OpCode.NE_V => @panic("NE_V unimplemented"),
      datModule.OpCode.NE_S => @panic("NE_S unimplemented"),
      datModule.OpCode.NE_E => @panic("NE_E unimplemented"),
      datModule.OpCode.NE_FNC => @panic("NE_FNC unimplemented"),
      datModule.OpCode.LE => @panic("LE unimplemented"),
      datModule.OpCode.GE => @panic("GE unimplemented"),
      datModule.OpCode.LT => @panic("LT unimplemented"),
      datModule.OpCode.GT => @panic("GT unimplemented"),
      // Loading / Storing Opcode Mnemonic
      datModule.OpCode.LOAD_F => @panic("LOAD_F unimplemented"),
      datModule.OpCode.LOAD_V => @panic("LOAD_V unimplemented"),
      datModule.OpCode.LOAD_S => @panic("LOAD_S unimplemented"),
      datModule.OpCode.LOAD_ENT => @panic("LOAD_ENT unimplemented"),
      datModule.OpCode.LOAD_FLD => @panic("LOAD_FLD unimplemented"),
      datModule.OpCode.LOAD_FNC => @panic("LOAD_FNC unimplemented"),
      datModule.OpCode.STORE_F => {
        self.mem32[statement.arg2] = self.mem32[statement.arg1];
        self.pc += 1;

        std.log.debug("STORE_F, dst {} ({d}) = src {} - pc 0x{x}",
          .{ statement.arg2, bitCast(f32, self.mem32[statement.arg2]), statement.arg1, self.pc });
        return false;
      },
      datModule.OpCode.STORE_V => {
        std.log.debug("STORE_V, dst {} = src {} - pc 0x{x}",
          .{ statement.arg2, statement.arg1, self.pc });

        const src = statement.arg1;
        const dst = statement.arg2;
        self.mem32[dst    ] = self.mem32[src    ];
        self.mem32[dst + 1] = self.mem32[src + 1];
        self.mem32[dst + 2] = self.mem32[src + 2];

        self.pc += 1;
        return false;
      },
      datModule.OpCode.STORE_S => {
        self.mem32[statement.arg2] = self.mem32[statement.arg1];
        self.pc += 1;

        var buf = [_:0]u8 { 0 } ** 255;
        _ = std.mem.replace(u8, self.getString(self.mem32[statement.arg2]), "\n", "\\n", &buf);
        std.log.debug("STORE_S, dst {} (\"{s}\") = src {} - pc {}",
          .{ statement.arg2, std.mem.span(@as([*:0]u8, @ptrCast(&buf))), statement.arg1, self.pc });
        return false;
      },
      datModule.OpCode.STORE_ENT => @panic("STORE_ENT unimplemented"),
      datModule.OpCode.STORE_FLD => @panic("STORE_FLD unimplemented"),
      datModule.OpCode.STORE_FNC => @panic("STORE_FNC unimplemented"),
      datModule.OpCode.STOREP_F => @panic("STOREP_F unimplemented"),
      datModule.OpCode.STOREP_V => @panic("STOREP_V unimplemented"),
      datModule.OpCode.STOREP_S => @panic("STOREP_S unimplemented"),
      datModule.OpCode.STOREP_ENT => @panic("STOREP_ENT unimplemented"),
      datModule.OpCode.STOREP_FLD => @panic("STOREP_FLD unimplemented"),
      datModule.OpCode.STOREP_FNC => @panic("STOREP_FNC unimplemented"),
      // If, Not Opcode Mnemonic
      datModule.OpCode.NOT_F => @panic("NOT_F unimplemented"),
      datModule.OpCode.NOT_V => @panic("NOT_V unimplemented"),
      datModule.OpCode.NOT_S => @panic("NOT_S unimplemented"),
      datModule.OpCode.NOT_ENT => @panic("NOT_ENT unimplemented"),
      datModule.OpCode.NOT_FNC => @panic("NOT_FNC unimplemented"),
      datModule.OpCode.IF => @panic("IF unimplemented"),
      datModule.OpCode.IFNOT => {
        std.log.debug("IFNOT, cnd v{} pc offset v{} - pc {}",
          .{ self.mem32[statement.arg1], statement.arg2, self.pc });
        if (self.mem32[statement.arg1] == 0) {
          self.pc += statement.arg2;
        } else {
          self.pc += 1;
        }
        return false;
      },
      // Function Calls Opcode Mnemonic
      datModule.OpCode.CALL0,
      datModule.OpCode.CALL1,
      datModule.OpCode.CALL2,
      datModule.OpCode.CALL3,
      datModule.OpCode.CALL4,
      datModule.OpCode.CALL5,
      datModule.OpCode.CALL6,
      datModule.OpCode.CALL7,
      datModule.OpCode.CALL8 => {
        const callArgc = @intFromEnum(statement.opcode) - @intFromEnum(datModule.OpCode.CALL0);
        std.log.debug("CALL{}, {} (0x{x}) - pc 0x{x} sp 0x{x}", .{
          callArgc, statement.arg1, self.mem32[statement.arg1], self.pc, self.sp,
        });
        try self.call(statement, @intCast(callArgc), err);
        return false;
      },
      // Boolean Operations Opcode Mnemonic
      datModule.OpCode.AND => @panic("AND unimplemented"),
      datModule.OpCode.OR => @panic("OR unimplemented"),
      datModule.OpCode.BITAND => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        // u32 bitcast to f32
        // f32 intcast to i32
        // do the operation
        // i32 floatcast f32
        // f32 bitcast to
        const result = intCast(i32, bitCast(f32, self.mem32[lhs])) & intCast(i32, bitCast(f32, self.mem32[rhs]));
        self.mem32[dst] = bitCast(u32, @as(f32, @floatFromInt(result)));
        self.pc += 1;

        std.log.debug("BITAND, dst {} ({d}) = lhs {} ({d}) - rhs {} ({d}) - pc {}",
          .{ dst, bitCast(f32, self.mem32[dst]), lhs, bitCast(f32, self.mem32[lhs]), rhs, bitCast(f32, self.mem32[rhs]), self.pc });
        return false;
      },
      datModule.OpCode.BITOR => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;

        const result = intCast(i32, bitCast(f32, self.mem32[lhs])) | intCast(i32, bitCast(f32, self.mem32[rhs]));
        self.mem32[dst] = bitCast(u32, @as(f32, @floatFromInt(result)));
        self.pc += 1;

        std.log.debug("BITOR, dst {} ({d}) = lhs {} ({d}) - rhs {} ({d}) - pc {}",
          .{ dst, bitCast(f32, self.mem32[dst]), lhs, bitCast(f32, self.mem32[lhs]), rhs, bitCast(f32, self.mem32[rhs]), self.pc });
        return false;
      },
    }
  }
};

pub fn main() !u8 {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 2 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a bsp29 tool\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <datfile> - Show the dat file content\n", .{ args[0] });
    return 1;
  }
  const mapfilepath = args[1];
  const buffer = misc.load(mapfilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, args[1] });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);

  var dat = datModule.Dat.init(allocator, buffer) catch |err| {
    return err;
  };
  defer dat.deinit(allocator);
  if (dat.header.version != 6) {
    try stderr.print("error: version {} not supported\n", .{ dat.header.version });
    std.posix.exit(1);
  }

  var vm = try VM.init(allocator);
  defer vm.deinit(allocator);
  vm.load(dat);
  try vm.jumpToFunction("main");

  var err = RuntimeError{};
  while (true) {
    const done = vm.execute(&err) catch |e| {
      switch (e) {
        error.RuntimeError => {
          try stderr.print("error: {s}\n", .{ err.message });
          std.posix.exit(1);
        },
        else => return e,
      }
    };
    if (done) break;
  }
  // bitCast the u32 into its f32 then convert it to u8.
  return @as(u8, @intFromFloat(@as(f32, @bitCast(vm.mem32[@intFromEnum(CallRegisters.ReturnValue)]))));
}
