// Interpret QuakeC bytecode.
//
// reference:
//  https://github.com/graphitemaster/gmqcc
//
// cmd: clear && zig build-exe -freference-trace qvm.zig && ./qvm ../data/pak/progs.dat

const std = @import("std");
const misc = @import("misc.zig");
const datModule = @import("dat.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub const RuntimeError = struct {
  const Self = @This();

  pc: i32 = 0,
  message: [255:0]u8 = [_:0]u8{ 0 } ** 255,
};

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

const VM = struct {
  const Self = @This();

  dat: datModule.Dat,

  globals: []u32,
  pc: usize,

  pub fn init(allocator: std.mem.Allocator, dat: datModule.Dat) !Self {
    const globals = try allocator.dupe(u32, dat.globals);
    const mainFn = blk: for (dat.definitions) |def| {
      if (def.getType() == datModule.QType.Function
        and std.mem.eql(u8, dat.getString(def.nameOffset), "main")) {
        break :blk dat.getFunction(def);
      }
    } else {
      return error.noMainFunction;
    };

    if (mainFn == null) {
      return error.mainIsNotAFunction;
    }

    if (mainFn.?.entryPoint < 0) {
      return error.mainIsABuiltin;
    }

    return Self{
      .dat = dat,
      .globals = globals,
      .pc = @intCast(mainFn.?.entryPoint),
    };
  }

  pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.globals);
    self.* = undefined;
  }

  pub fn execute(self: *Self, err: *RuntimeError) !bool {
    const statement = self.dat.statements[self.pc];
    switch (statement.opcode) {
      datModule.OpCode.DONE => @panic("DONE unimplemented"),
      datModule.OpCode.STATE => @panic("STATE unimplemented"),
      datModule.OpCode.GOTO => @panic("GOTO unimplemented"),
      datModule.OpCode.ADDRESS => @panic("ADDRESS unimplemented"),
      datModule.OpCode.RETURN => @panic("RETURN unimplemented"),
      // Arithmetic Opcode Mnemonic
      datModule.OpCode.MUL_F => @panic("MUL_F unimplemented"),
      datModule.OpCode.MUL_V => @panic("MUL_V unimplemented"),
      datModule.OpCode.MUL_FV => @panic("MUL_FV unimplemented"),
      datModule.OpCode.MUL_VF => @panic("MUL_VF unimplemented"),
      datModule.OpCode.DIV_F => @panic("DIV_F unimplemented"),
      datModule.OpCode.ADD_F => @panic("ADD_F unimplemented"),
      datModule.OpCode.ADD_V => @panic("ADD_V unimplemented"),
      datModule.OpCode.SUB_F => @panic("SUB_F unimplemented"),
      datModule.OpCode.SUB_V => @panic("SUB_V unimplemented"),
      // Comparison Opcode Mnemonic
      datModule.OpCode.EQ_F => @panic("EQ_F unimplemented"),
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
      datModule.OpCode.STORE_F => @panic("STORE_F unimplemented"),
      datModule.OpCode.STORE_V => @panic("STORE_V unimplemented"),
      datModule.OpCode.STORE_S => {
        std.log.debug("STORE_S, src {} dst {} - pc {}",
          .{ statement.arg1, statement.arg2, self.pc });

        const src = statement.arg1;
        const dst = statement.arg2;

        self.globals[dst] = self.globals[src];
        self.pc += 1;
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
      datModule.OpCode.IFNOT => @panic("IFNOT unimplemented"),
      // Function Calls Opcode Mnemonic
      datModule.OpCode.CALL0 => {
        std.log.debug("CALL0, {} - pc {}", .{ statement.arg1, self.pc });
        const fnIndex = statement.arg1;

        if (self.dat.getFunctionByIndex(fnIndex)) |fun| {
          self.pc = @intCast(fun.entryPoint);
        } else {
          _ = try std.fmt.bufPrint(&err.message, "No function with index {}", .{ fnIndex });
          return error.RuntimeError;
        }
        return false;
      },
      datModule.OpCode.CALL1 => {
        std.log.debug("CALL1, {} arg {} - pc {}", .{ statement.arg1, self.pc });
        const fnIndex = statement.arg1;

        if (self.dat.getFunctionByIndex(fnIndex)) |fun| {
          self.pc = @intCast(fun.entryPoint);
        } else {
          _ = try std.fmt.bufPrint(&err.message, "No function with index {}", .{ fnIndex });
          return error.RuntimeError;
        }
        return false;
      },
      datModule.OpCode.CALL2 => @panic("CALL2 unimplemented"),
      datModule.OpCode.CALL3 => @panic("CALL3 unimplemented"),
      datModule.OpCode.CALL4 => @panic("CALL4 unimplemented"),
      datModule.OpCode.CALL5 => @panic("CALL5 unimplemented"),
      datModule.OpCode.CALL6 => @panic("CALL6 unimplemented"),
      datModule.OpCode.CALL7 => @panic("CALL7 unimplemented"),
      datModule.OpCode.CALL8 => @panic("CALL8 unimplemented"),
      // Boolean Operations Opcode Mnemonic
      datModule.OpCode.AND => @panic("AND unimplemented"),
      datModule.OpCode.OR => @panic("OR unimplemented"),
      datModule.OpCode.BITAND => @panic("BITAND unimplemented"),
      datModule.OpCode.BITOR => @panic("BITOR unimplemented"),
    }
  }
};

pub fn main() !void {
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
    return;
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

  var vm = try VM.init(allocator, dat);
  defer vm.deinit(allocator);
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
}