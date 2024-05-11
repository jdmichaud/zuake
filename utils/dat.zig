// Utilities related to progs.dat quakec bytecode.
//
// reference:
//  https://www.leonrische.me/pages/quakec.html
//  https://www.leonrische.me/pages/quakec_bytecode_format.html
//
// cmd: clear && zig build-exe -freference-trace dat.zig && ./dat ../data/pak/progs.dat

const std = @import("std");
const misc = @import("misc.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub inline fn bitCast(comptime T: type, qfloat: u32) T {
  return @bitCast(qfloat);
}

const Header = extern struct {
  version: u32,         // Should be 6
  crc: u16,
  _: u16,               // unused
  statementsOffset: u32,
  statementsNum: u32,
  definitionsOffset: u32,
  definitionsNum: u32,
  fieldsOffset: u32,
  fieldsNum: u32,
  functionsOffset: u32,
  functionsNum: u32,
  stringsOffset: u32,
  stringsSize: u32,
  globalsOffset: u32,
  globalsNum: u32,
};

pub const OpCode = enum(u16) {
  //Misc Opcode Mnemonic
  DONE = 0x00,
  STATE = 0x3C,
  GOTO = 0x3D,
  ADDRESS = 0x1E,
  RETURN = 0x2B,
  // Arithmetic Opcode Mnemonic
  MUL_F = 0x01,
  MUL_V = 0x02,
  MUL_FV = 0x03,
  MUL_VF = 0x04,
  DIV_F = 0x05,
  ADD_F = 0x06,
  ADD_V = 0x07,
  SUB_F = 0x08,
  SUB_V = 0x09,
  // Comparison Opcode Mnemonic
  EQ_F = 0x0A,
  EQ_V = 0x0B,
  EQ_S = 0x0C,
  EQ_E = 0x0D,
  EQ_FNC = 0x0E,
  NE_F = 0x0F,
  NE_V = 0x10,
  NE_S = 0x11,
  NE_E = 0x12,
  NE_FNC = 0x13,
  LE = 0x14,
  GE = 0x15,
  LT = 0x16,
  GT = 0x17,
  // Loading / Storing Opcode Mnemonic
  LOAD_F = 0x18,
  LOAD_V = 0x19,
  LOAD_S = 0x1A,
  LOAD_ENT = 0x1B,
  LOAD_FLD = 0x1C,
  LOAD_FNC = 0x1D,
  STORE_F = 0x1F,
  STORE_V = 0x20,
  STORE_S = 0x21,
  STORE_ENT = 0x22,
  STORE_FLD = 0x23,
  STORE_FNC = 0x24,
  STOREP_F = 0x25,
  STOREP_V = 0x26,
  STOREP_S = 0x27,
  STOREP_ENT = 0x28,
  STOREP_FLD = 0x29,
  STOREP_FNC = 0x2A,
  // If, Not Opcode Mnemonic
  NOT_F = 0x2C,
  NOT_V = 0x2D,
  NOT_S = 0x2E,
  NOT_ENT = 0x2F,
  NOT_FNC = 0x30,
  IF = 0x31,
  IFNOT = 0x32,
  // Function Calls Opcode Mnemonic
  CALL0 = 0x33,
  CALL1 = 0x34,
  CALL2 = 0x35,
  CALL3 = 0x36,
  CALL4 = 0x37,
  CALL5 = 0x38,
  CALL6 = 0x39,
  CALL7 = 0x3A,
  CALL8 = 0x3B,
  // Boolean Operations Opcode Mnemonic
  AND = 0x3E,
  OR = 0x3F,
  BITAND = 0x40,
  BITOR = 0x41,
};

pub const Statement = extern struct {
  opcode: OpCode,
  arg1: u16,
  arg2: u16,
  arg3: u16,
};

pub const QType = enum(u16) {
  Void = 0,
  String = 1,
  Float = 2,
  Vector = 3,
  Entity = 4,
  Field = 5,
  Function = 6,
};

const TypedValue = union(QType) {
  Void: u32,
  String: []const u8,
  Float: f32,
  Vector: @Vector(3, f32),
  Entity: u32,
  Field: u32,
  Function: u32,

  pub fn format(
    self: TypedValue,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
  ) !void {
    _ = fmt;
    _ = options;

    switch (self) {
      TypedValue.Void => |v| try writer.print("Void {}", .{ v }),
      TypedValue.String => |s| try writer.print("{s}", .{ s }),
      TypedValue.Float => |f| try writer.print("{d}", .{ f }),
      TypedValue.Vector => |v| try writer.print("Vector {any}", .{ v }),
      TypedValue.Entity => |v| try writer.print("Entity {}", .{ v }),
      TypedValue.Field => |v| try writer.print("Field {}", .{ v }),
      TypedValue.Function => |v| try writer.print("Function {}", .{ v }),
    }
  }
};

const Definition = packed struct {
  const Self = @This();
  ty: u16,
  globalIndex: u16,
  nameOffset: u32, // offset in the string table

  pub fn getType(self: Self) QType {
    return @enumFromInt(self.ty & 0x7FFF);
  }

  pub fn isGlobal(self: Self) bool {
    return (self.ty & 0x8000) != 0;
  }
};

pub const Field = packed struct {
  ty: QType,
  index: u16,
  nameOffset: u32, // offset in the string table
};

const Function = extern struct {
  entryPoint: i32,  // if positive, index the first instruction if negative, index of a builtin
  firstLocal: u32,
  numberOfLocals: u32,
  profile: u32,     // always zero
  nameOffset: u32,   // offset in the string table
  fileOffset: u32,   // offset in the string table
  nbArgs: i32,      // number of arguments
  argSizes: [8]u8,     // size of arguments
};

const StringSection = []const []const u8;
const GlobalSection = []const u32;

fn loadLumpArray(comptime T: type, dat: []const u8, offset: u32, num: u32) ![]align(1) const T {
  const ts: [*]align(1) const T = @alignCast(@ptrCast(&dat[offset]));
  return ts[0..num];
}

pub const Dat = struct {
  const Self = @This();

  header: *align(1) const Header,
  statements: []align(1) const Statement,
  definitions: []align(1) const Definition,
  fields: []align(1) const Field,
  functions: []align(1) const Function,
  // Do we need this stringTable? Getting rid of it would mean no more allocation
  strings: []const u8,
  stringTable: StringSection,
  globals: GlobalSection,

  pub fn init(allocator: std.mem.Allocator, dat: []const u8) !Self {
    const header: *align(1) const Header = @ptrCast(dat);
    var acc = std.ArrayList([]const u8).init(allocator);
    defer acc.deinit();
    var splitIt = std.mem.splitAny(u8,
      dat[header.stringsOffset + 1..header.stringsOffset + header.stringsSize], &[_]u8{ 0 });
    while (splitIt.next()) |s| try acc.append(s);
    const stringTable: StringSection = try acc.toOwnedSlice();
    errdefer allocator.free(stringTable);

    return .{
      .header = header,
      .statements = try loadLumpArray(Statement, dat, header.statementsOffset, header.statementsNum),
      .definitions = try loadLumpArray(Definition, dat, header.definitionsOffset, header.definitionsNum),
      .fields = try loadLumpArray(Field, dat, header.fieldsOffset, header.fieldsNum),
      .functions = try loadLumpArray(Function, dat, header.functionsOffset, header.functionsNum),
      .strings = dat[header.stringsOffset..header.stringsOffset + header.stringsSize],
      .stringTable = stringTable,
      .globals = @as(*const []const u32,
        @ptrCast(&dat[header.globalsOffset..header.globalsOffset + header.globalsNum])).*,
    };
  }

  pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.stringTable);
  }

  pub fn getDefinitionByName(self: Self, identifier: []const u8) ?Definition {
    for (self.definitions) |definition| {
      if (std.mem.eql(u8, identifier, self.getString(definition.nameOffset))) {
        return definition;
      }
    }
    return null;
  }

  pub fn getVar(self: Self, definition: Definition) TypedValue {
    switch (definition.getType()) {
      QType.String => return TypedValue { .String = self.getString(self.globals[definition.globalIndex]) },
      QType.Float => {
        const globalValue = self.globals[definition.globalIndex];
        return TypedValue { .Float = bitCast(f32, globalValue) };
      },
      QType.Void => return TypedValue { .Void = self.globals[definition.globalIndex] },
      QType.Vector => {
        return TypedValue {
          .Vector = .{
            @bitCast(self.globals[definition.globalIndex]),
            @bitCast(self.globals[definition.globalIndex + 1]),
            @bitCast(self.globals[definition.globalIndex + 2]),
          },
        };
      },
      QType.Entity => return TypedValue { .Entity = self.globals[definition.globalIndex] },
      QType.Field => return TypedValue { .Field = self.globals[definition.globalIndex] },
      QType.Function => return TypedValue { .Function = self.globals[definition.globalIndex] },
    }
  }

  pub fn getFunction(self: Self, definition: Definition) ?Function {
    return switch (self.getVar(definition)) {
      QType.Function => self.functions[self.globals[definition.globalIndex]],
      else => null,
    };
  }

  pub fn getFunctionByIndex(self: Self, index: u32) ?Function {
    if (index < self.functions.len) {
      return self.functions[index];
    }
    return null;
  }

  pub fn getFunctionByName(self: Self, name: []const u8) ?Function {
    for (self.functions) |function| {
      if (std.mem.eql(u8, name, self.getString(function.nameOffset))) {
        return function;
      }
    }
    return null;
  }

  pub fn getString(self: Self, offset: u32) [:0]const u8 {
    return std.mem.span(@as([*:0]const u8, @ptrCast(self.strings[offset..])));
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

  var dat = try Dat.init(allocator, buffer);
  defer dat.deinit(allocator);
  if (dat.header.version != 6) {
    try stderr.print("error: version {} not supported\n", .{ dat.header.version });
    std.posix.exit(1);
  }

  try stdout.print("QuakeC Bytecode:\n", .{});
  try stdout.print("  version      {}\n", .{ dat.header.version });
  try stdout.print("\nSection Headers:\n", .{});
  try stdout.print("  [Nr]\t{s: >15}{s: >10}{s: >10}{s: >10}\n", .{ "Name", "Type", "Address", "Num" });
  try stdout.print("  [ 0]\t{s: >15}{s: >10}{x: >10}{: >10}\n",  .{ "statements", "", dat.header.statementsOffset, dat.header.statementsNum });
  try stdout.print("  [ 1]\t{s: >15}{s: >10}{x: >10}{: >10}\n",  .{ "definitions", "", dat.header.definitionsOffset, dat.header.definitionsNum });
  try stdout.print("  [ 2]\t{s: >15}{s: >10}{x: >10}{: >10}\n",  .{ "fields", "", dat.header.fieldsOffset, dat.header.fieldsNum });
  try stdout.print("  [ 3]\t{s: >15}{s: >10}{x: >10}{: >10}\n",  .{ "functions", "", dat.header.functionsOffset, dat.header.functionsNum });
  try stdout.print("  [ 4]\t{s: >15}{s: >10}{x: >10}{: >10} (in byte)\n",  .{ "strings", "", dat.header.stringsOffset, dat.header.stringsSize });
  try stdout.print("  [ 5]\t{s: >15}{s: >10}{x: >10}{: >10}\n",  .{ "globals", "", dat.header.globalsOffset, dat.header.globalsNum });

  try stdout.print("\nDefinitions table contains {} entries:\n", .{ dat.definitions.len - 1 });
  try stdout.print("  {s: >5}{s: >13}{s: >10}{s: >10}{s: >40} {s: <20}\n", .{
    "Num", "Global index", "Global", "Type", "Name", "Value",
  });
  for (dat.definitions[1..], 1..) |def, index| {
    try stdout.print("  {: >5}{: >13}{s: >10}{s: >10}{s: >40} {: <20}\n", .{
      index,
      def.globalIndex,
      if (def.isGlobal()) "yes" else "no",
      @tagName(def.getType()),
      dat.getString(def.nameOffset),
      dat.getVar(def),
    });
  }

  // ty: QType,
  // offset: u16,
  // nameOffset: u32, // offset in the string table

  try stdout.print("\nFields table contains {} entries:\n", .{ dat.fields.len - 1 });
  try stdout.print("  {s: >5}{s: >10}{s: >40}{s: >6}\n", .{
    "Num", "Type", "Name", "Index"
  });
  for (dat.fields[1..], 1..) |field, index| {
    try stdout.print("  {: >5}{s: >10}{s: >40}{: >6}\n", .{
      index,
      @tagName(field.ty),
      dat.getString(field.nameOffset),
      field.index,
    });
  }

  try stdout.print("\nFunctions table contains {} entries:\n", .{ dat.functions.len - 1 });
  try stdout.print("  {s: >5}{s: >8}{s: >11}{s: >30} {s: >15}{s: >13}{s: >13}{s: >13}\n", .{
    "Num", "Builtin", "Entrypoint", "Name", "File", "First local", "Nb. locals", "Nb. args",
  });
  for (dat.functions[1..], 1..) |fun, index| {
    try stdout.print("  {: >5}{s: >8}{: >11}{s: >30} {s: >15}{: >13}{: >13}{: >13}\n", .{
      index,
      if (fun.entryPoint < 0) "yes" else "no",
      fun.entryPoint,
      dat.getString(fun.nameOffset),
      dat.getString(fun.fileOffset),
      fun.firstLocal,
      fun.numberOfLocals,
      fun.nbArgs,
    });
  }

  try stdout.print("\nStrings table contains {} entries:\n", .{ dat.stringTable.len - 1 });
  try stdout.print("  {s: >5}{s: >10} {s: <50}\n", .{ "Num", "Num", "Value" });
  for (dat.stringTable[1..], 1..) |s, index| {
    try stdout.print("  {: >5}{: >10} {s: <50}\n", .{ index, s.len, s });
  }

  try stdout.print("\nStatements table contains {} entries:\n", .{ dat.statements.len - 1 });
  try stdout.print("  {s: >5}{s: >15} {s: <8}{s: <8}{s: <8}\n", .{ "Num", "Opcode", "Arg1", "Arg2", "Arg3" });
  for (dat.statements[1..], 1..) |s, index| {
    try stdout.print("  {: >5}{s: >15} {: <8}{: <8}{: <8}\n", .{ index, @tagName(s.opcode), s.arg1, s.arg2, s.arg3 });
  }

  try stdout.print("\nGlobals table contains {} entries:\n", .{ dat.globals.len - 1 });
  try stdout.print("  {s: >5}{s: >10}\n", .{ "Num", "Value" });
  for (1..dat.globals.len) |index| {
    try stdout.print("  {: >5}{x: >10}\n", .{ index, dat.globals[index] });
  }
}
