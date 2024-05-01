// Computes a BSP tree from a Quake map
//
// reference:
//  Michael Abrash’s Graphics Programming Black Book, Special Edition
//  Chapter 60 – Compiling BSP Trees
//
// cmd: clear && zig build-exe -freference-trace bspc.zig && ./bspc ../../quake_map_source/start.map

const std = @import("std");
const misc = @import("misc.zig");
const mapModule = @import("map.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

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
    try stdout.print("    {s} <mapfile> - Show the map content\n", .{ args[0] });
    return;
  }
  const mapfilepath = args[1];
  const buffer = misc.load(mapfilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, args[1] });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);

  var mapError = mapModule.MapError{};
  var map = mapModule.Map.init(allocator, buffer, &mapError) catch |err| {
    if (err == error.ParseError) {
      try stderr.print("{s}:{}:{}: {s}\n", .{ mapfilepath, mapError.row, mapError.column, mapError.errorMessage });
      std.posix.exit(1);
    }
    return err;
  };
  defer map.deinit();

  for (map.entities) |entity| {
    std.log.debug("entity {s} brushes {}", .{ entity.get("classname").?.toString(), entity.brushes.len });
  }
}
