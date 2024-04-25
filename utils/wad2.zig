// Load a WAD2 file (Quake version of WAD)
//
// reference:
//  https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_7.htm#CWADF
//
// cmd: clear && zig build-exe -freference-trace wad2.zig && ./wad2 ../data/pak/gfx.wad ../data/pak/gfx/palette.lmp /tmp/wad2

const std = @import("std");
const misc = @import("misc.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdOut().writer();

const Wad2Header = extern struct {
  magic: [4]u8,          // "WAD2", Name of the new WAD format
  numentries: i32,       // Number of entries
  diroffset: i32,        // Position of WAD directory in file
};

const EntryType = enum(u8) {
  ColorPalette      = '@',  // Color Palette
  StatusBarPicture  = 'B',  // Pictures for status bar
  MipTexture        = 'D',  // Used to be Mip Texture
  ConsolePicture    = 'E',  // Console picture (flat)
};

const Wad2Entry = extern struct {
  offset: i32,           // Position of the entry in WAD
  dsize: i32,            // Size of the entry in WAD file
  size: i32,             // Size of the entry in memory
  type: EntryType,       // type of entry
  cmprs: i8,             // Compression. 0 if none.
  dummy: i16,            // Not used
  name: [16]u8,          // 1 to 16 characters, '\0'-padded
};

const PicHead = extern struct {
  const Self = @This();

  width: i32,            // Picture width
  height: i32,           // Picture height
  pixels: u8,

  // This returns a const pointer so .pixels address remains the actual address
  // of the pixels in the WAD2 file.
  fn init(entry: Wad2Entry, buffer: []const u8) *align(1) const Self {
    const picture: *align(1) const PicHead = @ptrCast(&buffer[@as(usize, @intCast(entry.offset))]);
    return picture;
  }

  // This takes a const pointer so .pixels address remains the actual address
  // of the pixels in the WAD2 file.
  fn loadTexture(self: *align(1) const Self, allocator: std.mem.Allocator, palette: []const u8) ![]const u8 {
    const textureSize: usize = @intCast(self.width * self.height);
    const pixels: []const u8 = @as([*]const u8, @ptrCast(&self.pixels))[0..textureSize];
    const texture = try allocator.alloc(u8, textureSize * 3);
    for (0..textureSize) |i| {
      const paletteIndex: u16 = pixels[i];
      texture[i * 3    ] = palette[paletteIndex * 3    ];
      texture[i * 3 + 1] = palette[paletteIndex * 3 + 1];
      texture[i * 3 + 2] = palette[paletteIndex * 3 + 2];
    }
    return texture;
  }
};

const Wad2 = struct {
  const Self = @This();

  header: Wad2Header,
  entries: []align(1) const Wad2Entry,
  // statusBarPictures: []align(1) const PicHead,

  pub fn init(buffer: []align(4096) const u8) !Self {
    const header: *const Wad2Header = @ptrCast(&buffer[0]);
    if (!std.mem.eql(u8, &header.magic, "WAD2")) {
      return error.NotWad2;
    }

    const diroffset: usize = @intCast(header.diroffset);
    const ts: [*]align(1) const Wad2Entry = @alignCast(@ptrCast(&buffer[diroffset]));
    const entries: []align(1) const Wad2Entry = ts[0..@as(usize, @intCast(header.numentries))];

    return Wad2{
      .header = header.*,
      .entries = entries,
    };
  }
};

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 4 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a bsp29 tool\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <wad2file> <palette.lmp> <output_folder> - Show the wad content\n", .{ args[0] });
    return;
  }
  const wadfilepath = args[1];
  const paletteFilepath = args[2];
  const outputFolder = args[3];

  // Load the Wad2 file
  const buffer = misc.load(wadfilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, args[1] });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);
  const wad2: Wad2 = try Wad2.init(buffer);
  // Load the palette
  // https://quakewiki.org/wiki/palette.lmp
  const palette = misc.load(paletteFilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, paletteFilepath });
    std.posix.exit(1);
  };
  defer std.posix.munmap(palette);
  // Create output folder
  const cwd = std.fs.cwd();
  try cwd.makePath(outputFolder);

  std.log.debug("numentries: {}", .{ wad2.header.numentries });
  std.log.debug("diroffset: 0x{x}", .{ wad2.header.diroffset });

  for (wad2.entries, 0..) |entry, i| {
    std.log.debug("entry {}", .{ i });
    std.log.debug(" name       : {s}", .{ entry.name });
    std.log.debug(" offset     : 0x{x}", .{ entry.offset });
    std.log.debug(" type       : {}", .{ entry.type });
    std.log.debug(" size       : {}", .{ entry.dsize });
    std.log.debug(" size in mem: {}", .{ entry.size });
    std.log.debug(" compression: {}", .{ entry.cmprs });
    switch (entry.type) {
      EntryType.StatusBarPicture => {
        const picture: *align(1) const PicHead = PicHead.init(entry, buffer);
        std.log.debug(" width      : {}", .{ picture.width });
        std.log.debug(" height     : {}", .{ picture.height });

        const texture = try picture.loadTexture(allocator, palette);
        defer allocator.free(texture);
        // Create filepath
        const texture_name = std.mem.span(@as([*:0]const u8, @ptrCast(&entry.name)));
        const filename = try std.mem.concatWithSentinel(allocator, u8, &.{ texture_name, ".ppm" }, 0);
        defer allocator.free(filename);
        const filepath = try std.fs.path.join(allocator, &.{ outputFolder, filename });
        // Create file
        defer allocator.free(filepath);

        try misc.writePgm(@intCast(picture.width), @intCast(picture.height), texture, filepath);
        std.log.debug(" written to  : {s}", .{ filepath });
      },
      else => {},
    }
  }
}
