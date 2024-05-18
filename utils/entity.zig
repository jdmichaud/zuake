// List entities in a bsp and provide utility function to load them in a hashmap
//
// reference:
//  https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_2.htm#CMFME
//
// cmd: clear && zig build-exe -freference-trace -fno-llvm -fno-lld entity.zig && ./entity ../data/pak/maps/start.bsp

const std = @import("std");
const bspModule = @import("bsp.zig");

const stdout = std.io.getStdOut().writer();

fn load(pathname: []const u8) ![]align(4096) const u8 {
  var file = try std.fs.cwd().openFile(pathname, .{});
  defer file.close();

  const size = try file.getEndPos();
  const buffer = try std.posix.mmap(
    null,
    size,
    std.posix.PROT.READ,
    .{ .TYPE = .SHARED },
    file.handle,
    0,
  );
  errdefer std.posix.munmap(buffer);

  return buffer;
}

pub const EntityValueE = enum {
  Float,
  Vector,
  String,
};

pub const EntityValue = union(EntityValueE) {
  Float: f32,
  Vector: @Vector(3, f32),
  String: []const u8,
};

pub const EntityField = struct {
  const Self = @This();

  value: EntityValue,

  pub fn isString(self: Self) bool {
    switch (self.value) {
      .String => return true,
      else => return false,
    }
  }

  pub fn toString(self: Self) ![]const u8 {
    switch (self.value) {
      .String => |s| return s,
      else => return error.Conversion,
    }
  }

  pub fn toFloat(self: Self) !f32 {
    switch (self.value) {
      .Float => |f| return f,
      else => return error.Conversion,
    }
  }
};


pub const Entity = std.StringHashMap(EntityField);

pub const EntityList = struct {
  const Self = @This();

  entities: []Entity,
  allocator: std.mem.Allocator,

  pub fn init(allocator: std.mem.Allocator, entities: bspModule.Entities) !Self {
    const State = enum {
      LOOKING_FOR_ENTITY,
      LOOKING_FOR_KEY,
      PARSING_KEY,
      LOOKING_FOR_VALUE,
      PARSING_VALUE,
    };

    var list = std.ArrayList(Entity).init(allocator);
    var entity: Entity = undefined;
    var state = State.LOOKING_FOR_ENTITY;
    var key: std.ArrayList(u8) = undefined;
    var value: std.ArrayList(u8) = undefined;
    for (entities.data) |c| {
      switch (state) {
        State.LOOKING_FOR_ENTITY => {
          if (c == '{') {
            entity = Entity.init(allocator);
            state = State.LOOKING_FOR_KEY;
            continue;
          } else if (!std.ascii.isWhitespace(c) and c != 0) {
            return error.ParseError;
          }
        },
        State.LOOKING_FOR_KEY => {
          if (c == '"') {
            state = State.PARSING_KEY;
            key = std.ArrayList(u8).init(allocator);
            continue;
          } else if (c == '}') {
            try list.append(entity);
            state = State.LOOKING_FOR_ENTITY;
          } else if (!std.ascii.isWhitespace(c)) {
            return error.ParseError;
          }
        },
        State.PARSING_KEY => {
          if (c == '"') {
            state = State.LOOKING_FOR_VALUE;
            continue;
          } else {
            try key.append(c);
          }
        },
        State.LOOKING_FOR_VALUE => {
          if (c == '"') {
            state = State.PARSING_VALUE;
            value = std.ArrayList(u8).init(allocator);
            continue;
          } else if (!std.ascii.isWhitespace(c)) {
            return error.ParseError;
          }
        },
        State.PARSING_VALUE => {
          if (c == '"') {
            state = State.LOOKING_FOR_KEY;
            const str = try value.toOwnedSlice();
            const floatValueTry = std.fmt.parseFloat(f32, str) catch null;
            const vectorValueTry = parseVector(str);
            const entityValue = if (floatValueTry) |floatValue| blk: {
              allocator.free(str);
              break :blk EntityValue{ .Float = floatValue };
            } else if (vectorValueTry) |vectorValue| blk: {
              allocator.free(str);
              break :blk EntityValue{ .Vector = vectorValue };
            } else |_| EntityValue{ .String = str };
            try entity.put(try key.toOwnedSlice(), EntityField {
              .value = entityValue,
            });
            key.deinit();
            value.deinit();
            continue;
          } else {
            try value.append(c);
          }
        },
      }
    }
    return Self {
      .allocator = allocator,
      .entities = try list.toOwnedSlice(),
    };
  }

  pub fn deinit(self: *Self) void {
    for (self.entities) |*entityMap| {
      var it = entityMap.iterator();
      while (it.next()) |entityValue| {
        self.allocator.free(entityValue.key_ptr.*);
        switch (entityValue.value_ptr.value) {
          .String => |s| self.allocator.free(s),
          else => {},
        }
      }
      entityMap.deinit();
    }
    self.allocator.free(self.entities);
    self.* = undefined;
  }

  pub fn get(self: Self, key: []const u8) ?Entity {
    for (self.entities) |entityMap| {
      if (entityMap.get("classname")) |entityValue| {
        if (entityValue.isString() and std.mem.eql(u8, entityValue.toString() catch unreachable, key)) {
          return entityMap;
        }
      }
    }

    return null;
  }

  pub fn parseVector(str: []const u8) !@Vector(3, f32) {
    var it = std.mem.splitScalar(u8, str, ' ');
    var part = it.next();
    const x = if (part) |p|
      std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
    part = it.next();
    const y = if (part) |p|
      std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
    part = it.next();
    const z = if (part) |p|
      std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
    return @Vector(3, f32){ x, y, z };
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
    try stdout.print("    {s} <bspfile> - Show entities\n", .{ args[0] });
    return;
  }

  const bspFilepath = args[1];

  const buffer = try load(bspFilepath);
  defer std.posix.munmap(buffer);

  var bsp = try bspModule.Bsp.init(allocator, buffer);
  defer bsp.deinit(allocator);

  var entities = try EntityList.init(allocator, bsp.entities);
  defer entities.deinit();

  std.log.debug("{s}", .{ try entities.get("worldspawn").?.get("message").?.toString() });
}
