const std = @import("std");

// Load a file into a buffer using mmap
pub fn load(pathname: []const u8) ![]align(4096) const u8 {
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

pub fn writePgm(width: usize, height: usize, pixels: []const u8, filepath: []const u8) !void {
  // Create file
  const file = try std.fs.cwd().createFile(
    filepath,
    .{ .read = true },
  );
  defer file.close();
  // Prepare PGM header (https://en.wikipedia.org/wiki/Netpbm)
  var buffer: [255]u8 = [_]u8{ 0 } ** 255;
  const pgmHeader = try std.fmt.bufPrint(&buffer, "P6\n{} {}\n255\n", .{ width, height });
  // Write to file
  try file.writeAll(pgmHeader);
  try file.writeAll(pixels);
}
