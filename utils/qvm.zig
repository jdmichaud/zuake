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
const pakModule = @import("pak.zig");
const bspModule = @import("bsp.zig");
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
  // TODO: Rework this and nbEntities
  indexInOffsetList: usize,
  data: [*]u32,
};

const Builtins = struct {
  var random = std.rand.DefaultPrng.init(0);

  const Angles = enum(u8) {
    // up / down
    PITCH = 0,
    // left / right
    YAW = 1,
    // fall over
    ROLL = 2,
  };

  pub fn call(vm: *VM, index: u32, argc: u32) !void {
    switch (index) {
      1 => {
        // from a vector containing 3 angles (yaw, pitch, roll) compute 3
        // vectors: v_forward, v_up and v_right. These three vectors are globals
        // in the VM.
        // Yaw is the rotation about the z axis (between -180 and 180)
        // Pitch is the rotations about the y axis (between -90 and 90 deg)
        // Roll is the rotation about the x axis (between -180 and 180 deg)
        // So if we define the rotation matrix of those angles as Rx, Ry and Rz.
        // To get the vectors we need to multiply them by the unit vectors:
        // v_forward = RzRyRx[1, 0, 0]
        // v_up = RzRyRx[0, 1, 0]
        // v_right = RzRyRx[0, 0, 1]
        // ref:
        //   https://quakewiki.org/wiki/makevectors
        //   https://forums.insideqc.com/viewtopic.php?p=53892&sid=201ecf3bb7860db0d5eaa3dfd2078d56#p53892
        //   https://en.wikipedia.org/wiki/Euler_angles#Tait.E2.80.93Bryan_angles
        const paramOffset = @intFromEnum(CallRegisters.Parameter1);
        const convertToRad: f64 = (std.math.pi * 2.0 / 360.0);
        const angleYaw = @as(f64, @floatCast(bitCast(f32, vm.mem32[paramOffset + @intFromEnum(Angles.YAW)]))) * convertToRad;
        const anglePitch = @as(f64, @floatCast(bitCast(f32, vm.mem32[paramOffset + @intFromEnum(Angles.PITCH)]))) * convertToRad;
        const angleRoll = @as(f64, @floatCast(bitCast(f32, vm.mem32[paramOffset + @intFromEnum(Angles.ROLL)]))) * convertToRad;
        const sy: f64 = std.math.sin(angleYaw);
        const cy: f64 = std.math.cos(angleYaw);
        const sp: f64 = std.math.sin(anglePitch);
        const cp: f64 = std.math.cos(anglePitch);
        const sr: f64 = std.math.sin(angleRoll);
        const cr: f64 = std.math.cos(angleRoll);
        const v_forward = vm.dat.?.getDefinitionByName("v_forward") orelse unreachable;
        var forward = vm.mem32[v_forward.globalIndex..];
        forward[0] = bitCast(u32, @as(f32, @floatCast( cp * cy)));
        forward[1] = bitCast(u32, @as(f32, @floatCast( cp * sy)));
        forward[2] = bitCast(u32, @as(f32, @floatCast(-sp)));
        const v_right = vm.dat.?.getDefinitionByName("v_right") orelse unreachable;
        var right = vm.mem32[v_right.globalIndex..];
        right[0] = bitCast(u32, @as(f32, @floatCast(-1 * sr * sp * cy + -1 * cr * -sy)));
        right[1] = bitCast(u32, @as(f32, @floatCast(-1 * sr * sp * sy + -1 * cr *  cy)));
        right[2] = bitCast(u32, @as(f32, @floatCast(-1 * sr * cp)));
        const v_up = vm.dat.?.getDefinitionByName("v_up") orelse unreachable;
        var up = vm.mem32[v_up.globalIndex..];
        up[0] = bitCast(u32, @as(f32, @floatCast(cr * sp * cy + -sr * -sy)));
        up[1] = bitCast(u32, @as(f32, @floatCast(cr * sp * sy + -sr *  cy)));
        up[2] = bitCast(u32, @as(f32, @floatCast(cr * cp)));
      },
      2 => @panic("setorigin is not yet implemented"),
      3 => { // setmodel
        const entity = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter2)];
        const path = vm.getString(strOffset);
        std.log.warn("setmodel is not yet implemented {} {s}", .{ entity, path });
      },
      4 => { // setsize
        const entityIndexVM = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        // Retrieve the index of the entity in the entity offset list from the entity itself
        const entityIndex = intCast(u32, vm.translateVMToEnt(entityIndexVM));
        // Retrieve the mins and maxs vector
        const mins_x = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter2)    ]);
        const mins_y = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter2) + 1]);
        const mins_z = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter2) + 2]);
        const maxs_x = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter3)    ]);
        const maxs_y = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter3) + 1]);
        const maxs_z = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter3) + 2]);
        if (mins_x > maxs_x or mins_y > maxs_y or mins_z > maxs_z) return error.MinsAboveMaxs;
        var err = RuntimeError{}; // TODO: ignored for now

        try vm.setEntityField(entityIndex, "mins", .{ .Vector = .{ mins_x, mins_y, mins_z } }, &err);
        try vm.setEntityField(entityIndex, "maxs", .{ .Vector = .{ maxs_x, maxs_y, maxs_z } }, &err);
      },
      6 => @panic("break is not yet implemented"),
      7 => {
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), bitCast(u32, random.random().float(f32)));
      },
      8 => {
        const entityIndexVM = vm.read32(@intFromEnum(CallRegisters.Parameter1)); // entity
        // Retrieve the index of the entity in the entity offset list from the entity itself
        const entityIndex = intCast(u32, vm.translateVMToEnt(entityIndexVM));
        const channel: f32 = @bitCast(vm.read32(@intFromEnum(CallRegisters.Parameter2))); // float
        const soundPath = vm.getString(vm.read32(@intFromEnum(CallRegisters.Parameter3))); // string
        const volume: f32 = @bitCast(vm.read32(@intFromEnum(CallRegisters.Parameter4))); // float
        const attenuation: f32 = @bitCast(vm.read32(@intFromEnum(CallRegisters.Parameter5))); // float
        std.log.debug("sound is not yet implemented e {} channel {d} soundPath {s} volume {d} attenuation {d}", .{
          entityIndex, channel, soundPath, volume, attenuation,
        });
      },
      9 => @panic("normalize is not yet implemented"),
      10 => @panic("error is not yet implemented"),
      11 => @panic("objerror is not yet implemented"),
      12 => @panic("vlen is not yet implemented"),
      13 => @panic("vectoyaw is not yet implemented"),
      14 => { // spawn
        const addr = intCast(u32, vm.translateVMToEnt(try vm.spawnEntity()));
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), addr);
      },
      15 => { // remove
        const entityIndexVM = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        // Retrieve the index of the entity in the entity offset list from the entity itself
        const entityIndex = vm.translateVMToEnt(entityIndexVM);
        const entity: *Entity = @ptrCast(@alignCast(&vm.mem32[entityIndex]));
        // Entity are removed by setting their pointer to world (0 in the VM memory space).
        vm.entityOffsets[entity.indexInOffsetList] = vm.world;
      },
      16 => @panic("traceline is not yet implemented"),
      17 => @panic("checkclient is not yet implemented"),
      18 => @panic("find is not yet implemented"),
      19 => { // precache_sound
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        _ = vm.filesystem.readFile(path);
      },
      20 => { // precache_model
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        _ = vm.filesystem.readFile(path);
      },
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
      35 => { // lightstyle
        std.log.warn("lightstyle not yet implemented", .{});
      },
      36 => @panic("rint is not yet implemented"),
      37 => @panic("floor is not yet implemented"),
      38 => @panic("ceil is not yet implemented"),
      40 => @panic("checkbottom is not yet implemented"),
      41 => @panic("pointcontents is not yet implemented"),
      43 => { // fabs
        const value = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), bitCast(u32, @abs(value)));
      },
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
        // Do nothing and return the path
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        vm.write32(@intFromEnum(CallRegisters.ReturnValue), strOffset);
      },
      69 => { // makestatic
        std.log.warn("makestatic is not yet implemented", .{});
      },
      70 => @panic("changelevel is not yet implemented"),
      72 => { // cvar_set
        const identifier = vm.getString(vm.mem32[@intFromEnum(CallRegisters.Parameter1)]);
        const value = vm.getString(vm.mem32[@intFromEnum(CallRegisters.Parameter2)]);
        try vm.cvars.put(vm.heapAllocator.allocator(), identifier, value);
      },
      73 => @panic("centerprint is not yet implemented"),
      74 => { // ambientsound
        const p1index = @intFromEnum(CallRegisters.Parameter1);
        const position: @Vector(3, f32) = .{
          bitCast(f32, vm.mem32[p1index    ]),
          bitCast(f32, vm.mem32[p1index + 1]),
          bitCast(f32, vm.mem32[p1index + 2]),
        };
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter2)];
        const wavFile = vm.getString(strOffset);
        const volume = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter3)]);
        const attenuation = bitCast(f32, vm.mem32[@intFromEnum(CallRegisters.Parameter4)]);
        std.log.warn("ambientsound is not yet implemented {d} {s} {d} {d}", .{
          position, wavFile, volume, attenuation,
        });
      },
      75 => { // precache_model2
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        _ = vm.filesystem.readFile(path);
      },
      76 => { // precache_sound2
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        _ = vm.filesystem.readFile(path);
      },
      77 =>  { // precache_file2
        const strOffset = vm.mem32[@intFromEnum(CallRegisters.Parameter1)];
        const path = vm.getString(strOffset);
        _ = vm.filesystem.readFile(path);
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
  verbose: bool = false,
};

const VM = struct {
  const Self = @This();
  const MAX_ENTITIES = 1024;

  allocator: std.mem.Allocator,
  dat: ?datModule.Dat,
  bsp: ?bspModule.Bsp,

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
  //                                              <-  entity X's fields
  //                                              <- entity X
  //  functions                heap               <-  world's fields
  //  -------------------      ------------------ <- world
  //
  //  The memory model of the VM is designed to:
  //  1. Use as little space as possible
  //  2. Limit segmentation (we get everything in only one memory buffer ideally)
  //  3. Have the world entity address at 0 (from the point of view of the VM)
  //  This is achieved by:
  //  - Having all static data from the dat are mmaped from the file and used as
  //    is except the global area which read/write.
  //  - Loading the globals from the dat file directly at address 0. So all
  //    global indices corresponds to the index in memory without any
  //    translation.
  //  - Stack data (saved PC and SP registers, string allocated by builtings)
  //    are stored in the regions after the global. The stackOffset indicates
  //    where that region starts. The SP and FP register points at stackOffset
  //    initially and on each function call, the current SP is stored on the
  //    stack, the FP points to that saved SP and the SP is then moved to the
  //    next available slot. The strings allocated by the builtins are available
  //    only in the current function call so they are store on the stack. When
  //    we return from a function and the PC, SP and FP are restored, those
  //    strings are still present but they can be overwritten at any time. This
  //    is expected by the QuakeC code. Finally, entity strings are also loaded
  //    at the beginning of the stack before the first function call so they are
  //    never overwritten and can be considered static.
  //  - Finally, the heap is where dynamic allocation occurs. The entities and
  //    their data are stored there and a allocator is used to keep track of
  //    allocation. The allocator allocate starting at the bottom of the heap.
  //    All data from the heap (which is currently used only for entities) goes
  //    through an address translation so that, from the VM perspective, the
  //    heap data is in another segment with the allocation going from 0
  //    increasing. The world entity is the first allocated and all translation
  //    starts with the address of the world entity (which is not necessarily
  //    the bottom of the address space due to alignment). As a consequnce, from
  //    the VM perspective, the world's address it 0 which is expected in the
  //    QuaceC code as the condition `if (world)` is always false and world is
  //    returned by builtins as meaning "no entity".
  //  As strings can be stored at multiple places (either static string in the
  //  dat file or string in the stack) the `getString` of the VM implement a
  //  scheme that allows to automatically fetch the string where it is stored.
  //  From the VM point of view, string from static data keep have their
  //  original addresses. But string stored in the VM memory have an offset
  //  added. The offset is the stringAddress + stringOffset in the original dat.
  //  `getString` will detect that the address provided for a string is higher
  //  than the possible static string address and will subtract the offset and
  //  fetch the string in the VM memory instead of the dat file. From the point
  //  of view of the VM all the strings are in a linear address space where in
  //  reality, some of them are the dat file and some are in the VM memory.
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
  // The world entity
  world: usize,
  // The list of entity offset in memory
  // 0 is world
  entityOffsets: [MAX_ENTITIES]usize = [_]usize{ 0 } ** MAX_ENTITIES,
  // TODO: Rework this indexInOffsetList
  nbEntities: usize = 0,
  // What are console variables?
  cvars: std.StringHashMapUnmanaged([]const u8),
  // An object used to retrieve content from the filesystem (most probably a pak file)
  filesystem: Filesystem,
  // Time at which the VM is instantiated in milli seconds
  startTime: i64,
  // Options provided at creation
  options: VMOptions,

  // Counters
  instructionCount: usize = 0,
  functionCallCount: usize = 0,
  builtinCallCount: usize = 0,

  pub fn init(allocator: std.mem.Allocator, filesystem: Filesystem, options: VMOptions) !Self {
    // We make sure that memsize is divisible by @sizeOf(u32)
    const memsize: usize =
      @intFromFloat(@ceil(@as(f32, @floatFromInt(options.memsize)) / @sizeOf(u32)) * @sizeOf(u32));
    if (options.verbose) {
      try stdout.print("{}{s} of memory allocated\n", .{
        if (options.memsize < 1024) options.memsize else options.memsize / 1024,
        if (options.memsize < 1024) " bytes" else "Ko",
      });
    }
    // Allocate memory that is aligned along the u32 alignment constraints as we
    // are going to mainly access this memory by reading and writing u32s.
    const mem32: []u32 = try allocator.alloc(u32, memsize / @sizeOf(u32));
    @memset(mem32, 0);
    // Keep a []8 slice around for convenience
    const mem: []u8 = std.mem.sliceAsBytes(mem32);

    return Self{
      .allocator = allocator,
      .dat = null,
      .bsp = null,
      .mem = mem,
      .mem32 = mem32,
      .sp = 0,
      .fp = 0,
      .stackOffset = 0,
      .pc = 0,
      .maxFieldIndex = 0,
      .heapAllocator = rfba.ReverseFixedBufferAllocator.init(mem),
      .world = intCast(u32, mem32.len), // If no BSP loaded, world is the end of the memory
      .cvars = std.StringHashMapUnmanaged([]const u8){},
      .filesystem = filesystem,
      .startTime = std.time.milliTimestamp(),
      .options = options,
    };
  }

  pub fn deinit(self: *Self) void {
    if (self.options.verbose) {
      stdout.print("{} instrution executed {} function calls {} builtin calls\n", .{
        self.instructionCount, self.functionCallCount, self.builtinCallCount,
      }) catch unreachable;
    }
    self.allocator.free(self.mem32);
    self.* = undefined;
  }

  pub fn coreDump(self: Self, corefile: []const u8) !void {
    const file = try std.fs.cwd().createFile(corefile, .{ .read = true });
    defer file.close();
    try file.writeAll(self.mem);
    try stdout.print("core dumped: {s}\n", .{ corefile });
  }

  // Load a dat file and set the various pointers.
  pub fn loadDat(self: *Self, dat: datModule.Dat) !void {
    // Check that the global are less than the allocated memory and reserving
    // 1K for dynamic string data and 1K for the stack.
    if (dat.globals.len * @sizeOf(u32) > self.mem.len) {
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
      if (field.index > maxFieldIndex) maxFieldIndex = field.index;
    }
    maxFieldIndex += 1; // To simplify, ensure that it's never 0
    self.maxFieldIndex = maxFieldIndex;
    if (self.options.verbose) {
      try stdout.print(
        "Dat loaded {} globals {} strings {} definitions {} fields {} functions {} statements loaded\n", .{
          dat.globals.len,
          dat.strings.len,
          dat.definitions.len,
          dat.fields.len,
          dat.functions.len,
          dat.statements.len,
        }
      );
      const used = self.stackOffset * @sizeOf(u32);
      try stdout.print("{}{s} of static memory used\n", .{
        if (used < 1024) used else used / 1024,
        if (used < 1024) " bytes" else "Ko",
      });
    }
  }

  // Load a bsp file and initialize its entities.
  pub fn loadBsp(self: *Self, bsp: bspModule.Bsp, err: *RuntimeError) !void {
    self.bsp = bsp;
    var entityList = try entityModule.EntityList.init(self.allocator, bsp.entities);
    defer entityList.deinit();
    if (entityList.get("worldspawn")) |worldEntity| {
      try self.createWorld(worldEntity, err);
    } else {
      return error.NoWorld;
    }
    try self.loadEntities(entityList.entities, err);
    if (self.options.verbose) {
      try stdout.print("Bsp loaded {} entities into {}Ko of static memory and {}Ko of heap with {} fields per entity\n", .{
        entityList.entities.len,
        (self.sp - self.stackOffset) / 1024,
        (self.mem.len - self.heapAllocator.end_index) / 1024,
        self.maxFieldIndex,
      });
    }
    try self.callConstructors(err);
  }

  // The first entity automatically created.
  // world is special in a sense that its address is 0.
  // Any function returning en entity will return world to signify no entity returned.
  // Using world as a condition will then fail:
  // ```qc
  //   const e = findEntity();
  //   if (e == world) {
  //     print("no entity found");
  //   }
  // ```
  fn createWorld(self: *Self, world: entityModule.Entity, err: *RuntimeError) !void {
    // We can't rely on spawn which depends on the address to self.world
    const allocator = self.heapAllocator.allocator();
    const entity = try allocator.create(Entity);
    entity.data = (try allocator.alloc(u32, self.maxFieldIndex + 1)).ptr;
    entity.indexInOffsetList = 0;
    const entityIndex = (@intFromPtr(entity) - @intFromPtr(self.mem.ptr)) / @sizeOf(u32);
    self.world = entityIndex;
    self.entityOffsets[self.nbEntities] = self.world;
    self.nbEntities += 1;
    try self.loadEntityFields(intCast(u32, self.world), world, err);
  }

  fn spawnEntity(self: *Self) !usize {
    const allocator = self.heapAllocator.allocator();
    const entity = try allocator.create(Entity);
    entity.data = (try allocator.alloc(u32, self.maxFieldIndex + 1)).ptr;
    // The VM expects memory to be initialized to 0. We already initialized the
    // memory of the VM to zero but in debug mode, zig allocate with a known
    // pattern for uninitialized access check.
    var i: usize = 0;
    while (i < self.maxFieldIndex) { entity.data[i] = 0; i +=1; }
    const entityIndex = (@intFromPtr(entity) - @intFromPtr(self.mem.ptr)) / @sizeOf(u32);
    // Add the entity to the offset list
    self.entityOffsets[self.nbEntities] = entityIndex;
    entity.indexInOffsetList = self.nbEntities;
    self.nbEntities += 1;
    return entityIndex;
  }

  // Return the entityIndexVM of self.
  pub fn getSelfEntity(self: Self) !u32 {
    // Retrieve the self definition
    const selfDefinition = self.dat.?.getDefinitionByName("self") orelse {
      return error.ClassnameNotAString;
    };
    return self.read32(selfDefinition.globalIndex);
  }

  pub fn setEntityField(self: *Self, entityIndex: u32, fieldName: []const u8,
    value: entityModule.EntityValue, err: *RuntimeError) !void {

    const fieldIndex = try (self.getFieldIndexFromName(fieldName) orelse blk: {
      _ = try std.fmt.bufPrintZ(&err.message, "Undeclared field name: {s}", .{ fieldName });
      break :blk error.UnknownFieldName;
    });

    const fieldPtr = try self.getFieldPtr(intCast(u32, self.translateEntToVM(entityIndex)),
      fieldIndex, err);

    switch (value) {
      .String => |s| {
        const uvalue = try self.pushString("{s}", .{ s });
        self.write32ent(fieldPtr, intCast(u32, uvalue));
      },
      .Float => |f| {
        const uvalue = bitCast(u32, f);
        self.write32ent(fieldPtr, uvalue);
      },
      .Vector => |v| {
        const x = bitCast(u32, v[0]);
        self.write32ent(fieldPtr, x);
        const y = bitCast(u32, v[1]);
        self.write32ent(fieldPtr - 1, y);
        const z =  bitCast(u32, v[2]);
        self.write32ent(fieldPtr - 2, z);
      },
    }
  }

  // Load all the entity's fields into memory.
  // String are pushed on the stack and their reference set in the entity field.
  // Float are set directly in the entity field.
  fn loadEntityFields(self: *Self, entityIndex: u32, entity: entityModule.Entity,
    err: *RuntimeError) !void {
    // Go through the entity from the BSP, load the data in memory and set the fields.
    var it = entity.iterator();
    while (it.next()) |entry| {
      self.setEntityField(entityIndex, entry.key_ptr.*, entry.value_ptr.value, err) catch {};
    }
  }

  pub fn loadEntities(self: *Self, entities: []entityModule.Entity, err: *RuntimeError) !void {
    if (entities.len > MAX_ENTITIES) return error.StaticArrayTooSmall;
    var i: usize = 0;
    while (i < entities.len) {
      const entity = entities[i];
      i += 1;
      // Do not load world twice
      const classname = entity.get("classname").?.toString() catch "NOCLASSNAME";
      if (std.mem.eql(u8, classname, "worldspawn")) continue;
      // Load fields
      const entityIndex = try self.spawnEntity();
      try self.loadEntityFields(intCast(u32, entityIndex), entity, err);
    }
  }

  pub fn callConstructors(self: *Self, err: *RuntimeError) !void {
    for (self.entityOffsets[0..self.nbEntities]) |entityIndex| {
      // Retrieve the classname
      const entity = @as(*Entity, @ptrCast(@alignCast(&self.mem32[entityIndex])));
      const classnameIndex = self.getFieldIndexFromName("classname") orelse continue;
      const classname = self.getString(entity.data[classnameIndex]);
      // Retrieve the self definition
      const selfDefinition = self.dat.?.getDefinitionByName("self") orelse {
        _ = try std.fmt.bufPrintZ(&err.message,
          "no self definition. Without a self definition, constructors cannot be called.", .{});
        return error.ClassnameNotAString;
      };
      // If there is a function defined with the class name then it is a constructor
      if (self.dat.?.getFunctionByName(classname)) |constructorFn| {
        // Constructor shoudn't be a builtin
        std.debug.assert(constructorFn.entryPoint > 0);
        // set entity to self
        if (self.options.verbose) {
          try stdout.print("calling entity contructor {s}\n", .{ classname });
        }
        self.write32(selfDefinition.globalIndex, intCast(u32, self.translateEntToVM(entityIndex)));
        // goto funtion
        self.pc = intCast(usize, constructorFn.entryPoint);
        try self.execute(err);
      } else std.log.warn("constructor {s} could not be found", .{ classname });
    }
  }

  // Accessors to regular memory (globals, strings, etc...)
  inline fn read32(self: Self, addr: usize) u32 {
    return self.mem32[addr];
  }

  inline fn write32(self: Self, addr: usize, value: u32) void {
    self.mem32[addr] = value;
  }

  // Translate from entity address to VM address
  // This allows the VM to see entities in a different address space than they
  // really are.
  inline fn translateEntToVM(self: Self, addr: usize) usize {
    return self.world - addr;
  }

  inline fn translateVMToEnt(self: Self, addr: usize) usize {
    return self.world - addr;
  }

  // Accessors to entity memory
  inline fn read32ent(self: Self, addr: usize) u32 {
    return self.read32(self.translateVMToEnt(addr));
  }

  inline fn write32ent(self: Self, addr: usize, value: u32) void {
    self.write32(self.translateVMToEnt(addr), value);
  }

  inline fn getTime(self: Self) f32 {
    const fstartTime = @as(f32, @floatFromInt(self.startTime));
    return (@as(f32, @floatFromInt(std.time.milliTimestamp())) - fstartTime) / 1000;
  }

  // Set the global time variable to the current time
  fn setTimeVariable(self: Self) !void {
    const timeDefinition = self.dat.?.getDefinitionByName("time") orelse {
      return error.ClassnameNotAString;
    };
    self.write32(timeDefinition.globalIndex, bitCast(u32, self.getTime()));
  }

  fn getFieldIndexFromName(self: Self, fieldName: []const u8) ?u32 {
    for (self.dat.?.fields) |field| {
      if (std.mem.eql(u8, self.dat.?.getString(field.nameOffset), fieldName)) {
        return field.index;
      }
    }
    return null;
  }

  // From a entityIndex (just a number really) and fieldIndex construct an
  // opaque number that will be used to identify this particular field of this
  // particular struct.
  // entityIndex is a the index as used in the VM, not the index in the memory array.
  fn getFieldPtr(self: Self, entityIndexVM: u32, fieldIndex: u32, err: *RuntimeError) !u32 {
    if (fieldIndex >= self.maxFieldIndex) {
      _ = try std.fmt.bufPrintZ(&err.message, "field index {} is out of bound (nb of fields: {})", .{
        fieldIndex, self.maxFieldIndex,
      });
      return error.RuntimeError;
    }
    const entityIndexEnt = self.translateVMToEnt(entityIndexVM);
    const entity = @as(*Entity, @ptrCast(@alignCast(&self.mem32[entityIndexEnt])));
    const addrFieldEnt = (@intFromPtr(&entity.data[fieldIndex]) - @intFromPtr(self.mem32.ptr)) / @sizeOf(u32);
    const addrVM = intCast(u32, self.translateEntToVM(addrFieldEnt));
    return addrVM;
  }

  pub fn jumpToFunction(self: *Self, functionName: []const u8) !void {
    const function = blk: for (self.dat.?.definitions) |def| {
      if (def.getType() == datModule.QType.Function
        and std.mem.eql(u8, self.dat.?.getString(def.nameOffset), functionName)) {
        break :blk self.dat.?.getFunction(def);
      }
    } else {
      return error.FunctionNotFound;
    };

    if (function == null) {
      return error.IdentifierIsNotAFunction;
    }

    if (function.?.entryPoint < 0) {
      return error.FunctionIsABuiltin;
    }

    self.pc = @intCast(function.?.entryPoint);
  }

  pub fn runFunction(self: *Self, functionName: []const u8, err: *RuntimeError) !void {
    try self.jumpToFunction(functionName);
    try self.execute(err);
  }

  // Push a string to the dynamic string pile
  pub fn pushString(self: *Self, comptime fmt: []const u8, args: anytype) !usize {
    const pointer = self.sp;
    const written = (try std.fmt.bufPrintZ(self.mem[self.sp..], fmt, args)).len + 1;
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
    _ = self;
    _ = data;
    std.log.warn("registerFile {s} not implemented", .{ path });
  }

  fn call(self: *Self, statement: datModule.Statement, argc: u8, err: *RuntimeError) !void {
    const fnIndex = self.mem32[statement.arg1];

    if (self.dat.?.getFunctionByIndex(fnIndex)) |fun| {
      if (fun.entryPoint > 0) {
        self.functionCallCount += 1;
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
        self.builtinCallCount += 1;
        // No saving the PC because the builtin will not change it
        try Builtins.call(self, @abs(fun.entryPoint), argc);
        self.pc += 1;
      }
    } else {
      _ = try std.fmt.bufPrintZ(&err.message, "No function with index {}", .{ fnIndex });
      return error.RuntimeError;
    }
  }

  pub fn execute(self: *Self, err: *RuntimeError) !void {
    const startInstructionCount = self.instructionCount;
    // Set the time variable for the next VM execution.
    // If the variable does not exists, do nothing.
    self.setTimeVariable() catch {};
    while (true) {
      const statement = self.dat.?.statements[self.pc];
      const done = try self.executeStatement(statement, err);
      if (done) break;
      if (self.instructionCount - startInstructionCount > 100000) {
        return error.InstructionLimitReached;
      }
    }
  }

  inline fn executeStatement(self: *Self, statement: datModule.Statement, err: *RuntimeError) !bool {
    self.instructionCount +=1;
    if (self.options.trace) {
      const location = self.dat.?.getLocationFromStatement(self.pc);
      try stdout.print("{s: <11} {: >5}[{: >10.6}] {: >5}[{: >10.6}] {: >5}[{: >10.6}] pc {: >5} sp 0x{x} fp 0x{x} {s}({s})\n", .{
        @tagName(statement.opcode),
        statement.arg1, self.mem32[statement.arg1],
        statement.arg2, self.mem32[statement.arg2],
        statement.arg3, self.mem32[statement.arg3],
        self.pc,
        self.sp,
        self.fp,
        location.filename,
        location.function,
      });
    }

    switch (statement.opcode) {
      datModule.OpCode.DONE => {
        if (self.fp != self.stackOffset * @sizeOf(u32)) {
          // If the stack is not at the bottom of the memory, restore the
          // previous the fp and sp registers.
          self.sp = self.fp;
          self.fp = self.read32(self.sp / @sizeOf(u32));
          self.sp -= @sizeOf(u32);
        }
        return true;
      },
      datModule.OpCode.STATE => {
        // https://quakewiki.org/wiki/QuakeC_Definition_of_Functions#Definition_of_a_frame_function
        const framenum = bitCast(f32, self.read32(statement.arg1));
        const nextthinkFnIndex = self.read32(statement.arg2);

        const selfEntity = intCast(u32, self.translateEntToVM(self.getSelfEntity() catch |e| {
          _ = try std.fmt.bufPrintZ(&err.message,
            "no self definition. Without a self definition, constructors cannot be called.", .{});
          return e;
        }));

        try self.setEntityField(selfEntity, "frame", .{ .Float = framenum }, err);
        try self.setEntityField(selfEntity, "nextthink", .{ .Float = self.getTime() + 0.1 }, err);
        try self.setEntityField(selfEntity, "think", .{ .Float = bitCast(f32, nextthinkFnIndex) }, err);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.GOTO => {
        self.pc = intCast(usize, intCast(i32, self.pc) + bitCast(i16, statement.arg1));
        return false;
      },
      datModule.OpCode.ADDRESS => {
        const entityIndex = self.read32(statement.arg1);
        const fieldIndex = self.read32(statement.arg2);
        const dst = statement.arg3;

        self.write32(dst, try self.getFieldPtr(entityIndex, fieldIndex, err));
        self.pc += 1;

        return false;
      },
      datModule.OpCode.RETURN => {
        const src = statement.arg1;
        if (src != 0) {
          const returnAddress = @intFromEnum(CallRegisters.ReturnValue);
          self.write32(returnAddress    , self.read32(statement.arg1    ));
          self.write32(returnAddress + 1, self.read32(statement.arg1 + 1));
          self.write32(returnAddress + 2, self.read32(statement.arg1 + 2));
        }
        if (self.fp == self.stackOffset * @sizeOf(u32)) {
          // The stack is at the bottom of the memory so we are returning from main
          return true;
        }
        // Restore the the FP and the PC
        self.sp = self.fp;
        self.fp = self.read32(self.sp / @sizeOf(u32));
        self.sp -= @sizeOf(u32);
        self.pc = self.read32(self.sp / @sizeOf(u32));
        self.pc += 1;
        return false;
      },
      // Arithmetic Opcode Mnemonic
      datModule.OpCode.MUL_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.read32(lhs)) * bitCast(f32, self.read32(rhs))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.read32(lhs    )) * bitCast(f32, self.read32(rhs    )))
                        + bitCast(u32, bitCast(f32, self.read32(lhs + 1)) * bitCast(f32, self.read32(rhs + 1)))
                        + bitCast(u32, bitCast(f32, self.read32(lhs + 2)) * bitCast(f32, self.read32(rhs + 2))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_FV => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const f = bitCast(f32, self.read32(lhs));
        self.write32(dst    , bitCast(u32, f * bitCast(f32, self.read32(rhs    ))));
        self.write32(dst + 1, bitCast(u32, f * bitCast(f32, self.read32(rhs + 1))));
        self.write32(dst + 2, bitCast(u32, f * bitCast(f32, self.read32(rhs + 2))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.MUL_VF => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const f = bitCast(f32, self.read32(rhs));
        self.write32(dst    , bitCast(u32, f * bitCast(f32, self.read32(lhs    ))));
        self.write32(dst + 1, bitCast(u32, f * bitCast(f32, self.read32(lhs + 1))));
        self.write32(dst + 2, bitCast(u32, f * bitCast(f32, self.read32(lhs + 2))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.DIV_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.read32(lhs)) / bitCast(f32, self.read32(rhs))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.ADD_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.read32(lhs)) + bitCast(f32, self.read32(rhs))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.ADD_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst    , bitCast(u32, bitCast(f32, self.read32(lhs    )) + bitCast(f32, self.read32(rhs    ))));
        self.write32(dst + 1, bitCast(u32, bitCast(f32, self.read32(lhs + 1)) + bitCast(f32, self.read32(rhs + 1))));
        self.write32(dst + 2, bitCast(u32, bitCast(f32, self.read32(lhs + 2)) + bitCast(f32, self.read32(rhs + 2))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.SUB_F => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst, bitCast(u32, bitCast(f32, self.read32(lhs)) - bitCast(f32, self.read32(rhs))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.SUB_V => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        self.write32(dst    , bitCast(u32, bitCast(f32, self.read32(lhs    )) - bitCast(f32, self.read32(rhs    ))));
        self.write32(dst + 1, bitCast(u32, bitCast(f32, self.read32(lhs + 1)) - bitCast(f32, self.read32(rhs + 1))));
        self.write32(dst + 2, bitCast(u32, bitCast(f32, self.read32(lhs + 2)) - bitCast(f32, self.read32(rhs + 2))));
        self.pc += 1;
        return false;
      },
      // Comparison Opcode Mnemonic
      datModule.OpCode.EQ_F => {
        if (self.read32(statement.arg1) == self.read32(statement.arg2)) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.EQ_V => {
        if (self.read32(statement.arg1    ) == self.read32(statement.arg2    ) and
            self.read32(statement.arg1 + 1) == self.read32(statement.arg2 + 1) and
            self.read32(statement.arg1 + 2) == self.read32(statement.arg2 + 2)) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.EQ_S => {
        var i: usize = 0;
        while (self.mem[statement.arg1 * @sizeOf(u32) + i] != 0 and
               self.mem[statement.arg2 * @sizeOf(u32) + i] != 0 and
               self.mem[statement.arg1 * @sizeOf(u32) + i] == self.mem[statement.arg2 * @sizeOf(u32) + i]) {
          i += 1;
        }

        if (self.mem[statement.arg1 * @sizeOf(u32) + i] == 0 and
            self.mem[statement.arg2 * @sizeOf(u32) + i] == 0) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }

        self.pc += 1;
        return false;
      },
      datModule.OpCode.EQ_E => @panic("EQ_E unimplemented"),
      datModule.OpCode.EQ_FNC => @panic("EQ_FNC unimplemented"),
      datModule.OpCode.NE_F => {  // untested
        if (self.read32(statement.arg1) == self.read32(statement.arg2)) {
          self.write32(statement.arg3, 0);
        } else {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NE_V => {  // untested
        if (self.read32(statement.arg1    ) == self.read32(statement.arg2    ) and
            self.read32(statement.arg1 + 1) == self.read32(statement.arg2 + 1) and
            self.read32(statement.arg1 + 2) == self.read32(statement.arg2 + 2)) {
          self.write32(statement.arg3, 0);
        } else {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NE_S => {  // untested
        var i: usize = 0;
        while (self.mem[statement.arg1 * @sizeOf(u32) + i] != 0 and
               self.mem[statement.arg2 * @sizeOf(u32) + i] != 0 and
               self.mem[statement.arg1 * @sizeOf(u32) + i] == self.mem[statement.arg2 * @sizeOf(u32) + i]) {
          i += 1;
        }

        if (self.mem[statement.arg1 * @sizeOf(u32) + i] == 0 and
            self.mem[statement.arg2 * @sizeOf(u32) + i] == 0) {
          self.write32(statement.arg3, 0);
        } else {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        }

        self.pc += 1;
        return false;
      },
      datModule.OpCode.NE_E => @panic("NE_E unimplemented"),
      datModule.OpCode.NE_FNC => @panic("NE_FNC unimplemented"),
      datModule.OpCode.LE => {
        if (self.read32(statement.arg1) <= self.read32(statement.arg2)) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.GE => {
        if (self.read32(statement.arg1) >= self.read32(statement.arg2)) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.LT => {
        if (self.read32(statement.arg1) < self.read32(statement.arg2)) {
          self.write32(statement.arg3, bitCast(u32, @as(f32, @floatFromInt(1))));
        } else {
          self.write32(statement.arg3, 0);
        }
        self.pc += 1;
        return false;
      },
      datModule.OpCode.GT => {
        if (self.read32(statement.arg1) > self.read32(statement.arg2)) {
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

        const entityIndex = self.read32(entityRef);
        const fieldIndex = self.read32(fieldRef);

        const fieldPtr = try self.getFieldPtr(entityIndex, fieldIndex, err);

        self.write32(dst, self.read32ent(fieldPtr));

        self.pc += 1;
        return false;
      },
      datModule.OpCode.LOAD_V => {
        const entityRef = statement.arg1;
        const fieldRef = statement.arg2;
        const dst = statement.arg3;

        const entityIndex = self.read32(entityRef);
        const fieldIndex = self.read32(fieldRef);

        const fieldPtr = try self.getFieldPtr(entityIndex, fieldIndex, err);

        self.write32(dst    , self.read32ent(fieldPtr    ));
        // TODO: Having to decrease memory here is a leaky abstraction
        self.write32(dst + 1, self.read32ent(fieldPtr - 1));
        self.write32(dst + 2, self.read32ent(fieldPtr - 2));

        self.pc += 1;
        return false;
      },
      datModule.OpCode.STORE_F,
      datModule.OpCode.STORE_S,
      datModule.OpCode.STORE_ENT,
      datModule.OpCode.STORE_FLD,
      datModule.OpCode.STORE_FNC => {
        self.write32(statement.arg2, self.read32(statement.arg1));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.STORE_V => {
        const src = statement.arg1;
        const dst = statement.arg2;

        self.write32(dst    , self.read32(src    ));
        self.write32(dst + 1, self.read32(src + 1));
        self.write32(dst + 2, self.read32(src + 2));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.STOREP_S,
      datModule.OpCode.STOREP_F,
      datModule.OpCode.STOREP_ENT,
      datModule.OpCode.STOREP_FLD,
      datModule.OpCode.STOREP_FNC => {
        const src = statement.arg1;
        const fieldRef = statement.arg2;

        self.write32ent(self.read32(fieldRef), self.read32(src));

        self.pc += 1;
        return false;
      },
      datModule.OpCode.STOREP_V => {
        const src = statement.arg1;
        const fieldRef = statement.arg2;

        self.write32ent(self.read32(fieldRef)    , self.read32(src    ));
        // TODO: Having to decrease memory here is a leaky abstraction
        self.write32ent(self.read32(fieldRef) - 1, self.read32(src + 1));
        self.write32ent(self.read32(fieldRef) - 2, self.read32(src + 2));

        self.pc += 1;
        return false;
      },
      // If, Not Opcode Mnemonic
      datModule.OpCode.NOT_F,
      datModule.OpCode.NOT_ENT => {
        const src = statement.arg1;
        const dst = statement.arg3;
        self.write32(dst, if (self.read32(src) == 0) bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NOT_V => {
        const vec = statement.arg1;
        const dst = statement.arg3;
        self.write32(dst, if (self.read32(vec    ) == 0 and
                              self.read32(vec + 1) == 0 and
                              self.read32(vec + 2) == 0) bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.NOT_S => {
        const src = statement.arg1;
        const dst = statement.arg3;

        const strptr = self.read32(src);

        if (strptr == 0 or self.getString(strptr)[0] == 0) {
          self.write32(dst, bitCast(u32, @as(f32, 1)));
        } else {
          self.write32(dst, 0);
        }

        self.pc += 1;
        return false;
      },
      datModule.OpCode.NOT_FNC => @panic("NOT_FNC unimplemented"),
      datModule.OpCode.IF => { // untested
        if (self.read32(statement.arg1) != 0) {
          self.pc += statement.arg2;
        } else {
          self.pc += 1;
        }
        return false;
      },
      datModule.OpCode.IFNOT => {
        if (self.read32(statement.arg1) == 0) {
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
        self.write32(dst, if (self.read32(lhs) != 0 and self.read32(rhs) != 0)
          bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.OR => { // untested
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        const dst = statement.arg3;
        self.write32(dst, if (self.read32(lhs) != 0 or self.read32(rhs) != 0)
          bitCast(u32, @as(f32, 1)) else 0);
        self.pc += 1;
        return false;
      },
      datModule.OpCode.BITAND => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;
        // u32 bitcast to f32
        // f32 intcast to i32
        // do the operation
        // i32 floatcast f32
        // f32 bitcast to
        const result = intCast(i32, bitCast(f32, self.read32(lhs))) & intCast(i32, bitCast(f32, self.read32(rhs)));
        self.write32(dst, bitCast(u32, @as(f32, @floatFromInt(result))));
        self.pc += 1;
        return false;
      },
      datModule.OpCode.BITOR => {
        const dst = statement.arg3;
        const lhs = statement.arg1;
        const rhs = statement.arg2;

        const result = intCast(i32, bitCast(f32, self.read32(lhs))) | intCast(i32, bitCast(f32, self.read32(rhs)));
        self.write32(dst, bitCast(u32, @as(f32, @floatFromInt(result))));
        self.pc += 1;
        return false;
      },
    }
  }
};

fn getMemorySize(args: clap.Args) !usize {
  return if (args.getOption([]const u8, "memory-size")) |memsizeArg| blkinner: {
    const lastChar = memsizeArg[memsizeArg.len - 1];
    break :blkinner switch (lastChar) {
      'k', 'K' => try std.fmt.parseInt(usize, memsizeArg[0..memsizeArg.len - 1], 10) * 1024,
      'm', 'M' => try std.fmt.parseInt(usize, memsizeArg[0..memsizeArg.len - 1], 10) * 1024 * 1024,
      else => try std.fmt.parseInt(usize, memsizeArg, 10),
    };
  } else 1024 * 1024 * 1; // 1Mb by default;
}

const Filesystem = struct {
  const Self = @This();
  entries: ?[]*align(1) const pakModule.EntryHeader,

  pub fn readFile(self: Self, path: []const u8) []const u8 {
    if (self.entries) |_| {
      std.log.warn("readFileFromPak {s}", .{ path });
      const buffer = [_]u8{ 0 } ** 1;
      return &buffer;
    } else {
      std.log.warn("readFileStub {s}", .{ path });
      const buffer = [_]u8{ 0 } ** 1;
      return &buffer;
    }
  }
};

const FileType = enum {
  DatFile,
  PakFile,
  Unknown,
};

fn getFileType(buffer: []const u8) FileType {
  return if (std.mem.eql(u8, buffer[0..4], "PACK"))
    FileType.PakFile
  else if (std.mem.eql(u8, buffer[0..4], &.{ 6, 0, 0, 0 }))
    FileType.DatFile
  else FileType.Unknown;
}

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
      .short = "e",
      .long = "verbose",
      .help = "Display additional information about the VM",
    }, .{
      .short = "m",
      .long = "memory-size",
      .arg = .{ .name = "memory", .type = []const u8 },
      .help = "Amount of memory to allocate for the VM (-m 12, -m 64K, -m 1M)",
    }, .{
      .short = "j",
      .long = "jump-to",
      .arg = .{ .name = "function", .type = []const u8 },
      .help = "Jump to function on startup",
    }, .{
      .short = "b",
      .long = "bsp-file",
      .arg = .{ .name = "bspfile", .type = []const u8 },
      .help = "Load a BSP file",
    } },
  }).parse(args);

  const memsize = try getMemorySize(parsedArgs);
  const filepath = parsedArgs.arguments.items[0];

  // Create the VM
  var vm = try VM.init(allocator, .{
    .entries = null,
  }, .{
    .trace = parsedArgs.getSwitch("trace"),
    .memsize = memsize,
    .verbose = parsedArgs.getSwitch("verbose"),
  });
  defer vm.deinit();
  // Load the file passed as arguments
  const buffer = misc.load(filepath) catch |err| {
    try stderr.print("error: {}, trying to open {s}\n", .{ err, filepath });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);
  // If we are passed a PakFile, load it, replace the readFile stub and return
  // the buffer to the dat file (progs.dat).
  // If we are passed a DatFile, just return the buffer.
  const bufferSplit = switch (getFileType(buffer)) {
    FileType.DatFile => blk: {
      break :blk .{ buffer, null };
    },
    FileType.PakFile => blk: {
      const entries = try pakModule.loadPak(allocator, buffer);
      // If we are provided a pak file, replace the stub for readFile
      vm.filesystem = Filesystem{ .entries = entries };
      for (entries) |file| {
        if (std.mem.eql(u8, file.pathname[0.."progs.dat".len], "progs.dat")) {
          break :blk .{ buffer[file.offset..file.offset + file.size], entries };
        }
      }
      try stderr.print("error: pak file does not contain a progs.dat entry", .{});
      std.posix.exit(1);
    },
    FileType.Unknown => {
      try stderr.print("error: unrecognized file format: expecting either a dat file or a pak file", .{});
      std.posix.exit(1);
    },
  };
  defer {
    if (bufferSplit[1]) |entries| allocator.free(entries);
  }
  // Load the dat buffer
  var dat = datModule.Dat.init(allocator, bufferSplit[0]) catch |err| {
    return err;
  };
  defer dat.deinit(allocator);
  if (dat.header.version != 6) {
    try stderr.print("error: version {} not supported\n", .{ dat.header.version });
    std.posix.exit(1);
  }
  try vm.loadDat(dat);

  var err = RuntimeError{};
  // Load a BSP file if provided.
  // This piece of code it quite contrived and should probably be revised.
  var bspTupleP = if (parsedArgs.getOption([]const u8, "bsp-file")) |bspfilepath| blkinner: {
    const bspBuffer = misc.load(bspfilepath) catch |e| {
      try stderr.print("error: {}, trying to open {s}\n", .{ e, bspfilepath });
      std.posix.exit(1);
    };
    var bsp = try bspModule.Bsp.init(allocator, bspBuffer);
    errdefer bsp.deinit(allocator);
    vm.loadBsp(bsp, &err) catch |e| {
      vm.coreDump("core.dump") catch {};
      return switch (e) {
        // error.UnknownFieldName,
        error.RuntimeError => {
          try stderr.print("error: {s}\n", .{ err.message });
          std.posix.exit(1);
        },
        else => return e,
      };
    };
    break :blkinner .{ bspBuffer, bsp };
  } else null;
  defer {
    if (bspTupleP) |*bspTuple| {
      bspTuple[1].deinit(allocator);
      std.posix.munmap(bspTuple[0]);
    }
  }
  // Check if we should jump to a particular function on boot.
  if (parsedArgs.getSwitch("jump-to")) {
    const functionName = parsedArgs.getOption([]const u8, "jump-to") orelse "main";
    vm.runFunction(functionName, &err) catch |e| {
      vm.coreDump("core.dump") catch {};
      return switch (e) {
        error.RuntimeError => {
          try stderr.print("error: {s}\n", .{ err.message });
          std.posix.exit(1);
        },
        else => return e,
      };
    };
  }
  // bitCast the u32 into its f32 then convert it to u8.
  return @as(u8, @intFromFloat(@as(f32, @bitCast(vm.mem32[@intFromEnum(CallRegisters.ReturnValue)]))));
}
