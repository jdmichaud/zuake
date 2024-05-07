// clear && zig build-lib python.zig -dynamic

const std = @import("std");
const bspModule = @import("bsp.zig");
const misc = @import("misc.zig");
const mip = @import("mip");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = general_purpose_allocator.allocator();

export fn loadBsp(c_filepath: [*c]const u8) *bspModule.Bsp {
  const filepath = std.mem.span(@as([*:0]const u8, @ptrCast(c_filepath)));
  const bsp: *bspModule.Bsp = allocator.create(bspModule.Bsp) catch unreachable;
  bsp.* = bspModule.Bsp.init(allocator, misc.load(filepath) catch unreachable) catch unreachable;
  return bsp;
}

export fn getMipTextures(bsp: *bspModule.Bsp, index: usize) *align(1) const bspModule.MipTexture {
  return bsp.mipTextures[index];
}
