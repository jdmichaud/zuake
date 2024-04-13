const std = @import("std");
const zlm = @import("zlm.zig").SpecializeOn(f32);

const stdout = std.io.getStdOut().writer();

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  const x = try std.fmt.parseFloat(f32, args[1]);
  const y = try std.fmt.parseFloat(f32, args[2]);
  const z = try std.fmt.parseFloat(f32, args[3]);
  const w = try std.fmt.parseFloat(f32, args[4]);
  var v1 = zlm.vec2(x, y);
  const v2 = zlm.vec2(z, w);
  try stdout.print("JDJD {any}", .{ v1.add(v2) });
}
