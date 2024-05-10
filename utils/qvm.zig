// Interpret QuakeC bytecode.
//
// reference:
//  https://github.com/graphitemaster/gmqcc
//  http://ouns.nexuizninjaz.com/dev_quakec.html
//
// cmd: clear && zig build-exe -freference-trace qvm.zig && ./qvm ../data/pak/progs.dat

const std = @import("std");
const misc = @import("misc.zig");
const clap = @import("clap.zig");
const datModule = @import("dat.zig");
const entityModule = @import("entity.zig");
const rfba = @import("reverse-fixed-buffer-allocator.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

fn bitCast(comptime T: type, v: anytype) T {
  return @bitCast(v);
}

fn intCast(comptime T: type, v: anytype) T {
  switch (@TypeOf(v)) {
    f16, f32, f64, f80, f128 => return @intFromFloat(v),
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize => return @intCast(v),
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

const Entity = packed struct {
  data: [*]u32,
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
      14 => { // spawn
        const allocator = vm.heapAllocator.allocator();
        const entity = try allocator.create(Entity);
        entity.data = (try allocator.alloc(u32, vm.maxFieldIndex + 1)).ptr;
        const entityIndex = (@intFromPtr(entity) - @intFromPtr(vm.mem.ptr)) / @sizeOf(u32);
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), intCast(u32, entityIndex));
      },
      15 => @panic("remove is not yet implemented"),
      16 => @panic("traceline is not yet implemented"),
      17 => @panic("checkclient is not yet implemented"),
      18 => @panic("find is not yet implemented"),
      19 => { // precache_sound
        // Only try to open a file and prints with a warning if it fails
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        const data = misc.load(path) catch |err| {
          try stderr.print("warning: {}, trying to open {s}\n", .{ err, path });
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
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), @intCast(strPointer));
      },
      27 => { // vtos
        const v_x = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)    ]);
        const v_y = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1) + 1]);
        const v_z = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1) + 2]);
        const strPointer = try vm.pushString("'{d} {d} {d}'", .{ v_x, v_y, v_z });
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), @intCast(strPointer));
      },
      28 => @panic("coredump is not yet implemented"),
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
      62 => { // sqrt
        const result = std.math.sqrt(bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)]));
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), bitCast(u32, result));
      },
      67 => @panic("movetogoal is not yet implemented"),
      68 => { // precache_file
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        try stderr.print("warning: precache_file {s} ignored\n", .{ path });
        // Apparently, this function does nothing.
        // Returns the string pass to it.
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
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
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
      },
      78 => @panic("setspawnparms is not yet implemented"),
      85 => @panic("stov is not yet implemented"),
      97 => { // pow
        const base = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
        const exp = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter2)]);
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), bitCast(u32, std.math.pow(f32, base, exp)));
      },
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

pub const VMOptions = struct {
  trace: bool = false,
  memsize: usize = 1024 * 1024 * 1, // 1Mb by default
};

const VM = struct {
  const Self = @This();

  allocator: std.mem.Allocator,
  dat: ?datModule.Dat,

  // Dat Memory layout        VM memory layout
  // 0-------------------
  //  strings
  //  -------------------     0------------------
  //  globals (ro)             globals (rw)
  //  -------------------      ------------------ <- stackOffset
  //  statements               stack              (pc, fp saved here)
  //  -------------------                         <- fp somewhere
  //  fields                                      <- sp somewhere
  //  -------------------      ------------------
  //  functions                heap (TBD)
  //  -------------------      ------------------
  mem: []u8,
  // Same as mem but in 32bits for convenience
  mem32: []u32,
  // Stack Pointer
  // Index in mem (so incremented by @sizeOf(u32)). Must always remain align on @sizeOf(u32).
  sp: usize,
  // Beginning of the stack (align @sizeOf(u32))
  stackOffset: usize,
  // Frame pointer. Points to the beginning of the frame (after the PC and FP is saved)
  fp: usize,
  // Program Counter
  // This is not an offset in memory but an index in the statement list
  pc: usize,
  // The maximum number of fields an entity can have.
  // This value is derived from the max index found in the dat file.
  // It will be used in the formula to get a unique identifier for each field of
  // each entity
  // TODO: We assume that indices are contiguous and ever increasing.
  maxFieldIndex: usize,
  // Heap allocator
  heapAllocator: rfba.ReverseFixedBufferAllocator,
  // Options provided at creation
  options: VMOptions,

  pub fn init(allocator: std.mem.Allocator, options: VMOptions) !Self {
    // We make sure that memsize is divisible by @sizeOf(u32)
    const memsize: usize =
      @intFromFloat(@ceil(@as(f32, @floatFromInt(options.memsize)) / @sizeOf(u32)) * @sizeOf(u32));
    // Allocate memory that is aligned along the u32 alignment constraints as we
    // are going to mainly access this memory by reading and writing u32s.
    const mem32: []u32 = try allocator.alloc(u32, memsize / @sizeOf(u32));
    // Keep a []8 slice around for convenience
    const mem: []u8 = std.mem.sliceAsBytes(mem32);

    return Self{
      .allocator = allocator,
      .dat = null,
      .mem = mem,
      .mem32 = mem32,
      .sp = 0,
      .fp = 0,
      .stackOffset = 0,
      .pc = 0,
      .maxFieldIndex = 0,
      .heapAllocator = rfba.ReverseFixedBufferAllocator.init(mem),
      .options = options,
    };
  }

  pub fn deinit(self: *Self) void {
    self.allocator.free(self.mem32);
    self.* = undefined;
  }

  // Load a dat file and set the various pointers.
  pub fn loadDat(self: *Self, dat: datModule.Dat) !void {
    // Check that the global are less than the allocated memory and reserving
    // 1K for dynamic string data and 1K for the stack.
    if (dat.globals.len * @sizeOf(u32) > self.mem.len - 1024 * 2) {
      return error.NotEnoughMemory;
    }
    // Load globals
    @memcpy(self.mem32[0..dat.globals.len], dat.globals);
    // Set boundary pointers
    self.stackOffset = std.mem.alignForward(usize, dat.globals.len, @sizeOf(u32));
    self.sp = self.stackOffset * @sizeOf(u32);
    self.fp = self.sp;
    self.dat = dat;
    var maxFieldIndex: usize = 0;
    for (dat.fields) |field| {
      if (field.offset > maxFieldIndex) maxFieldIndex = field.offset;
    }
    maxFieldIndex += 1; // To simplify, ensure that it's never 0
    self.maxFieldIndex = maxFieldIndex;
  }

  pub fn loadEntities(self: *Self, entities: []entityModule.Entity) !void {
    _ = self;
    _ = entities;
  }

  pub inline fn read32(self: Self, addr: usize) u32 {
    return self.mem32[addr];
  }

  pub inline fn write32(self: Self, addr: usize, value: u32) void {
    self.mem32[addr] = value;
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

  // Push a string to the dynamic string pile
  pub fn pushString(self: *Self, comptime fmt: []const u8, args: anytype) !usize {
    const pointer = self.sp;
    const written = (try std.fmt.bufPrintZ(self.mem[self.sp..], fmt, args)).len;
    self.sp = std.mem.alignForward(usize, self.sp + written, @sizeOf(u32));
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
    return std.mem.span(@as([*:0]const u8, @ptrCast(self.mem[offset - datStringBoundary..])));
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
        // Save the the FP and the PC
        self.write32(self.sp / @sizeOf(u32), @intCast(self.pc));
        self.sp += @sizeOf(u32);
        self.write32(self.sp / @sizeOf(u32), @intCast(self.fp));
        self.fp = self.sp;
        self.sp += @sizeOf(u32);
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

  pub fn getFieldByIndex(self: Self, fieldIndex: usize) ?datModule.Field {
    // Let's not forget that in the dat structure, all indexed 0 element are
    // meaningless and array are 1-based
    for (self.dat.?.fields[1..]) |field| {
      if (field.offset == fieldIndex) {
        return field;
      }
    }
    return null;
  }

  // From a entityIndex (just a number really) and fieldIndex construct an
  // opaque number that will be used to identify this particular field of this
  // particular struct.
  pub fn getFieldPtr(self: Self, err: *RuntimeError, entityIndex: u32, fieldIndex: u32) !u32 {
    if (fieldIndex >= self.maxFieldIndex) {
      _ = try std.fmt.bufPrint(&err.message, "field index {} is out of bound (nb of fields: {})", .{
        fieldIndex, self.maxFieldIndex,
      });
      return error.RuntimeError;
    }
    const entity = @as(*Entity, @ptrCast(@alignCast(&self.mem32[entityIndex])));
    return intCast(u32, (@intFromPtr(&entity.data[fieldIndex]) - @intFromPtr(self.mem32.ptr)) / @sizeOf(u32));
  }

  pub fn execute(self: *Self, err: *RuntimeError) !bool {
    const statement = self.dat.?.statements[self.pc];
    return self.executeStatement(statement, err);
  }

  inline fn executeStatement(self: *Self, statement: datModule.Statement, err: *RuntimeError) !bool {
    if (self.options.trace) {
      try stdout.print("{s: <9} {: >5}[{d: >8.6}] {: >5}[{d: >8.6}] {: >5}[{d: >8.6}] pc {} sp 0x{x} fp 0x{x}\n", .{
        @tagName(statement.opcode),
        statement.arg1, bitCast(f32, self.mem32[statement.arg1]),
        statement.arg2, bitCast(f32, self.mem32[statement.arg2]),
        statement.arg3, bitCast(f32, self.mem32[statement.arg3]),
        self.pc,
        self.sp,
        self.fp,
      });
    }

    switch (statement.opcode) {
      datModule.OpCode.DONE => {
        return true;
      },
      datModule.OpCode.STATE => @panic("STATE unimplemented"),
      datModule.OpCode.GOTO => {
        self.pc = intCast(usize, intCast(i32, self.pc) + bitCast(i16, statement.arg1));
        return false;
      },
      datModule.OpCode.ADDRESS => {
        const entityIndex = self.mem32[statement.arg1];
        const fieldIndex = self.mem32[statement.arg2];
        const dst = statement.arg3;

        self.write32(dst, try self.getFieldPtr(err, entityIndex, fieldIndex));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.RETURN => {
        const src = statement.arg1;
        if (src != 0) {
          const returnAddress = @intFromEnum(CallRegisters.ReturnValue);
          self.write32(returnAddress    , self.mem32[statement.arg1    ]);
          self.write32(returnAddress + 1, self.mem32[statement.arg1 + 1]);
          self.write32(returnAddress + 2, self.mem32[statement.arg1 + 2]);
        }
        if (self.fp == self.stackOffset * @sizeOf(u32)) {
          // The stack is at the bottom of the memory so we are returning from main
          return true;
        }
        // Restore the the FP and the PC
        self.sp = self.fp;
        self.fp = self.mem32[self.sp / @sizeOf(u32)];
        self.sp -= @sizeOf(u32);
        self.pc = self.mem32[self.sp / @sizeOf(u32)];
        self.pc += 1;
        return false;
      },
      // Arithmetic Opcode Mnemonic
      datModule.OpCode.MUL_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.mem32[lhs]) * bitCast(f32, self.mem32[rhs])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.mem32[lhs    ]) * bitCast(f32, self.mem32[rhs    ]))
                        + bitCast(u32, bitCast(f32, self.mem32[lhs + 1]) * bitCast(f32, self.mem32[rhs + 1]))
                        + bitCast(u32, bitCast(f32, self.mem32[lhs + 2]) * bitCast(f32, self.mem32[rhs + 2])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_FV => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const f = bitCast(f32, self.mem32[lhs]);
        self.write32(dst    , bitCast(u32, f * bitCast(f32, self.mem32[rhs    ])));
        self.write32(dst + 1, bitCast(u32, f * bitCast(f32, self.mem32[rhs + 1])));
        self.write32(dst + 2, bitCast(u32, f * bitCast(f32, self.mem32[rhs + 2])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_VF => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const f = bitCast(f32, self.mem32[rhs]);
        self.write32(dst    , bitCast(u32, f * bitCast(f32, self.mem32[lhs    ])));
        self.write32(dst + 1, bitCast(u32, f * bitCast(f32, self.mem32[lhs + 1])));
        self.write32(dst + 2, bitCast(u32, f * bitCast(f32, self.mem32[lhs + 2])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.DIV_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.mem32[lhs]) / bitCast(f32, self.mem32[rhs])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.ADD_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.mem32[lhs]) + bitCast(f32, self.mem32[rhs])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.ADD_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst    , bitCast(u32, bitCast(f32, self.mem32[lhs    ]) + bitCast(f32, self.mem32[rhs    ])));
        self.write32(dst + 1, bitCast(u32, bitCast(f32, self.mem32[lhs + 1]) + bitCast(f32, self.mem32[rhs + 1])));
        self.write32(dst + 2, bitCast(u32, bitCast(f32, self.mem32[lhs + 2]) + bitCast(f32, self.mem32[rhs + 2])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.SUB_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.mem32[lhs]) - bitCast(f32, self.mem32[rhs])));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.SUB_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst    , bitCast(u32, bitCast(f32, self.mem32[lhs    ]) - bitCast(f32, self.mem32[rhs    ])));
        self.write32(dst + 1, bitCast(u32, bitCast(f32, self.mem32[lhs + 1]) - bitCast(f32, self.mem32[rhs + 1])));
        self.write32(dst + 2, bitCast(u32, bitCast(f32, self.mem32[lhs + 2]) - bitCast(f32, self.mem32[rhs + 2])));
        self.pc += 1;
        return false;
      },
      // Comparison Opcode Mnemonic
      datModule.OpCode.EQ_F => {
        if (self.mem32[statement.arg1] == self.mem32[statement.arg2]) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
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
      datModule.OpCode.LE => {
        if (self.mem32[statement.arg1] <= self.mem32[statement.arg2]) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.GE => {
        if (self.mem32[statement.arg1] >= self.mem32[statement.arg2]) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.LT => {
        if (self.mem32[statement.arg1] < self.mem32[statement.arg2]) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.GT => {
        if (self.mem32[statement.arg1] > self.mem32[statement.arg2]) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      // Loading / Storing Opcode Mnemonic
      datModule.OpCode.LOAD_F,
      datModule.OpCode.LOAD_S,
      datModule.OpCode.LOAD_ENT,
      datModule.OpCode.LOAD_FLD,
      datModule.OpCode.LOAD_FNC => {
        const entityRef = statement.arg1;
        const fieldRef = statement.arg2;
        const dst = statement.arg3;

        const entityIndex = self.mem32[entityRef];
        const fieldIndex = self.mem32[fieldRef];

        const fieldPtr = try self.getFieldPtr(err, entityIndex, fieldIndex);

        self.write32(dst, self.mem32[fieldPtr]);

        self.pc += 1;
        return false;
      },
      datModule.OpCode.LOAD_V => {
        const entityRef = statement.arg1;
        const fieldRef = statement.arg2;
        const dst = statement.arg3;

        const entityIndex = self.mem32[entityRef];
        const fieldIndex = self.mem32[fieldRef];

        const fieldPtr = try self.getFieldPtr(err, entityIndex, fieldIndex);

        self.write32(dst    , self.mem32[fieldPtr    ]);
        self.write32(dst + 1, self.mem32[fieldPtr + 1]);
        self.write32(dst + 2, self.mem32[fieldPtr + 2]);

        self.pc += 1;
        return false;
      },
      datModule.OpCode.STORE_F,
      datModule.OpCode.STORE_S,
      datModule.OpCode.STORE_ENT,
      datModule.OpCode.STORE_FLD,
      datModule.OpCode.STORE_FNC => {
        self.write32(statement.arg2, self.mem32[statement.arg1]);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.STORE_V => {
        const src = statement.arg1;
        const dst = statement.arg2;
        self.write32(dst    , self.mem32[src    ]);
        self.write32(dst + 1, self.mem32[src + 1]);
        self.write32(dst + 2, self.mem32[src + 2]);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.STOREP_S,
      datModule.OpCode.STOREP_F,
      datModule.OpCode.STOREP_V,
      datModule.OpCode.STOREP_ENT,
      datModule.OpCode.STOREP_FLD,
      datModule.OpCode.STOREP_FNC => {
        const src = statement.arg1;
        const fieldPtr = statement.arg2;

        self.write32(self.mem32[fieldPtr], self.mem32[src]);

        self.pc += 1;
        return false;
      },
      // If, Not Opcode Mnemonic
      datModule.OpCode.NOT_F => {
        const src = statement.arg1;
        const dst = statement.arg3;
        self.write32(dst, if (self.mem32[src] == 0) bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NOT_V => {
        const vec = statement.arg1;
        const dst = statement.arg3;
        self.write32(dst, if (self.mem32[vec    ] == 0 and
                              self.mem32[vec + 1] == 0 and
                              self.mem32[vec + 2] == 0) bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NOT_S => @panic("NOT_S unimplemented"),
      datModule.OpCode.NOT_ENT => @panic("NOT_ENT unimplemented"),
      datModule.OpCode.NOT_FNC => @panic("NOT_FNC unimplemented"),
      datModule.OpCode.IF => @panic("IF unimplemented"),
      datModule.OpCode.IFNOT => {
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
        try self.call(statement, @intCast(callArgc), err);
        return false;
      },
      // Boolean Operations Opcode Mnemonic
      datModule.OpCode.AND => {
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const dst = statement.arg3;
        self.write32(dst, if (self.mem32[lhs] != 0 and self.mem32[rhs] != 0)
          bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
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
        self.write32(dst, bitCast(u32, @as(f32, @floatFromInt(result))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.BITOR => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;

        const result = intCast(i32, bitCast(f32, self.mem32[lhs])) | intCast(i32, bitCast(f32, self.mem32[rhs]));
        self.write32(dst, bitCast(u32, @as(f32, @floatFromInt(result))));
        self.pc += 1;
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

  const parsedArgs = clap.parser(clap.ArgDescriptor{
    .name = "qvm",
    .description = "A QuakeC virtual machine",
    .withHelp = true,
    .version = "0.1.0",
    .expectArgs = &[_][]const u8{ "datfile" },
    .options = &[_]clap.OptionDescription{ .{
      .short = "t",
      .long = "trace",
      .help = "Enable tracing of instructions",
    }, .{
      .short = "j",
      .long = "jump-to",
      .arg = .{ .name = "function", .type = []const u8 },
      .help = "Jump to function on startup",
    } },
  }).parse(args);

  const mapfilepath = parsedArgs.arguments.items[0];
  const buffer = misc.load(mapfilepath) catch |err| {
    try stderr.print("error: {}, trying to open {s}\n", .{ err, mapfilepath });
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

  var vm = try VM.init(allocator, .{ .trace = parsedArgs.getSwitch("trace") });
  defer vm.deinit();
  try vm.loadDat(dat);
  if (parsedArgs.getSwitch("jump-to")) {
    try vm.jumpToFunction(parsedArgs.getOption([]const u8, "jump-to") orelse "main");
  }

  var err = RuntimeError{};
  while (true) {
    const done = vm.execute(&err) catch |e| {
      return switch (e) {
        error.RuntimeError => {
          try stderr.print("error: {s}\n", .{ err.message });
          std.posix.exit(1);
        },
        else => return e,
      };
    };
    if (done) break;
  }
  // bitCast the u32 into its f32 then convert it to u8.
  return @as(u8, @intFromFloat(@as(f32, @bitCast(vm.mem32[@intFromEnum(CallRegisters.ReturnValue)]))));
}
