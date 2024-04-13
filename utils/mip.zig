// clear && zig build-exe -freference-trace mip.zig && time ./mip ../data/pak/maps/start.bsp ../data/pak/gfx/palette.lmp mips
const std = @import("std");
const bspModule = @import("bsp.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const debug = std.log.debug;

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

pub fn loadMipTexture(allocator: std.mem.Allocator, buffer: []const u8,
  mipTexture: *align(1) const bspModule.MipTexture, mipIndex: usize, palette: []const u8) ![]const u8 {
  const offset = switch (mipIndex) {
    0 => mipTexture.offset1,
    1 => mipTexture.offset2,
    2 => mipTexture.offset4,
    3 => mipTexture.offset8,
    else => unreachable,
  };
  const textureSize = (mipTexture.width * mipTexture.height) / std.math.pow(usize, 2, mipIndex * 2);
  const textureOffset = offset + @intFromPtr(mipTexture) - @intFromPtr(buffer.ptr);
  const texture = try allocator.alloc(u8, textureSize * 3);
  for (0..textureSize) |i| {
    const paletteIndex: u16 = buffer[textureOffset + i];
    texture[i * 3    ] = palette[paletteIndex * 3    ];
    texture[i * 3 + 1] = palette[paletteIndex * 3 + 1];
    texture[i * 3 + 2] = palette[paletteIndex * 3 + 2];
  }
  return texture;
}

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 4 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a bsp29 tool used to extract mip textures\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <bspfile> <palette.lmp> <output_folder> - Extract mip texture to output_folder\n", .{ args[0] });
    return;
  }

  const bspFilepath = args[1];
  const paletteFilepath = args[2];
  const outputFolder = args[3];

  // Load the BSP file
  const buffer = try load(bspFilepath);
  defer std.posix.munmap(buffer);
  var bsp = try bspModule.Bsp.init(allocator, buffer);
  defer bsp.deinit(allocator);
  // Load the palette
  // https://quakewiki.org/wiki/palette.lmp
  const palette = load(paletteFilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, paletteFilepath });
    std.posix.exit(1);
  };
  defer std.posix.munmap(palette);
  // Create output folder
  const cwd = std.fs.cwd();
  try cwd.makePath(outputFolder);
  // Loop through the textures
  for (0..bsp.mipTextures.len) |i| {
    const texture_name = std.mem.span(@as([*:0]const u8, @ptrCast(&bsp.mipTextures[i].name)));
    // Loop through the mip index
    for (0..4) |mipIndex| {
      // Compute data dependent on mip index
      const indexDependentData = switch (mipIndex) {
        0 => .{ "_1", bsp.mipTextures[i].width    , bsp.mipTextures[i].height     },
        1 => .{ "_2", bsp.mipTextures[i].width / 2, bsp.mipTextures[i].height / 2 },
        2 => .{ "_4", bsp.mipTextures[i].width / 4, bsp.mipTextures[i].height / 4 },
        3 => .{ "_8", bsp.mipTextures[i].width / 8, bsp.mipTextures[i].height / 8 },
        else => unreachable,
      };
      const width = indexDependentData[1];
      const height = indexDependentData[2];
      const filename = try std.mem.concatWithSentinel(allocator, u8, &.{ texture_name, indexDependentData[0], ".ppm" }, 0);
      defer allocator.free(filename);
      // Create file
      const filepath = try std.fs.path.join(allocator, &.{ outputFolder, filename });
      defer allocator.free(filepath);
      const file = try std.fs.cwd().createFile(
        filepath,
        .{ .read = true },
      );
      defer file.close();
      try stdout.print("{s}\n", .{ filepath });
      // Decode texture with the palette
      const texture = try loadMipTexture(allocator, buffer, bsp.mipTextures[i], mipIndex, palette);
      defer allocator.free(texture);
      // Prepare PGM header (https://en.wikipedia.org/wiki/Netpbm)
      const pgmHeader = try std.fmt.allocPrint(allocator, "P6\n{} {}\n255\n", .{ width, height });
      defer allocator.free(pgmHeader);
      // Write to file
      try file.writeAll(pgmHeader);
      try file.writeAll(texture);
    }

  }
}
