// zig build-exe -freference-trace pak.zig && ./pak ../data/quake106/id1/pak0.pak ../data/pak/
const std = @import("std");

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

const PakHeader = extern struct {
  magic: [4]u8,
  offset: u32,
  size: u32,
};

pub const EntryHeader = extern struct {
  pathname: [56]u8,
  offset: u32,
  size: u32,
};

pub fn loadPak(allocator: std.mem.Allocator, buffer: []const u8) ![]*align(1) const EntryHeader {
  const pakHeader: *align(1) const PakHeader = @alignCast(@ptrCast(&buffer[0]));
  if (!std.mem.eql(u8, &pakHeader.magic, "PACK")) {
    @panic("not a pak file");
  }

  var entries: []*align(1) const EntryHeader =
    try allocator.alloc(*align(1) const EntryHeader, pakHeader.size / @sizeOf(EntryHeader));
  const nbEntries = pakHeader.size / @sizeOf(EntryHeader);
  var i: u32 = 0;
  while (i < nbEntries) {
    const entryOffset = pakHeader.offset + i * @sizeOf(EntryHeader);
    entries[i] = @alignCast(@ptrCast(&buffer[entryOffset]));
    i += 1;
  }

  return entries;
}

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 3 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a pak tool\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <pakfile> <destination> - List and extract pak content\n", .{ args[0] });
    return;
  }

  const pak = try load(args[1]);
  defer std.posix.munmap(pak);

  const pakHeader: *const PakHeader = @ptrCast(&pak[0]);
  if (!std.mem.eql(u8, &pakHeader.magic, "PACK")) {
    @panic("not a pak file");
  }
  try stdout.print("magic: {s}\n", .{ pakHeader.magic });
  try stdout.print("offset: 0x{x}\n", .{ pakHeader.offset });
  try stdout.print("size: {} entries\n", .{ pakHeader.size / @sizeOf(EntryHeader) });

  const entries = try loadPak(allocator, pak);
  defer allocator.free(entries);

  for (entries, 0..) |entry, i| {
    try stdout.print(" {}: {s} (@0x{x} {}) -> ", .{ i, entry.pathname, entry.offset, entry.size });

    // Create the path
    const dirname = std.fs.path.dirname(&entry.pathname) orelse "";
    const path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ args[2], dirname });
    defer allocator.free(path);
    const basename = std.fs.path.basename(std.mem.span(@as([*:0]const u8, @ptrCast(&entry.pathname))));
    try stdout.print("{s}/{s}\n", .{ path, basename });
    try std.fs.cwd().makePath(path);
    var dir = try std.fs.cwd().openDir(path, .{ .access_sub_paths = true });
    // Write to file
    var file = try dir.createFile(basename, .{ .truncate = true });
    try file.writeAll(pak[entry.offset..entry.offset + entry.size]);
  }
}
