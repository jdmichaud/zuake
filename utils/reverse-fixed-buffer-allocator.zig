const std = @import("std");
const Allocator = std.mem.Allocator;

fn sliceContainsSlice(container: []u8, slice: []u8) bool {
  return @intFromPtr(slice.ptr) >= @intFromPtr(container.ptr) and
    (@intFromPtr(slice.ptr) + slice.len) <= (@intFromPtr(container.ptr) + container.len);
}

// Like FixedBufferAllocator but allocates starting at the end of the buffer.
pub const ReverseFixedBufferAllocator = struct {
  end_index: usize,
  buffer: []u8,

  pub fn init(buffer: []u8) ReverseFixedBufferAllocator {
    return ReverseFixedBufferAllocator{
      .buffer = buffer,
      .end_index = buffer.len,
    };
  }

  /// not thread safe
  pub fn allocator(self: *ReverseFixedBufferAllocator) Allocator {
    return .{
      .ptr = self,
      .vtable = &.{
        .alloc = alloc,
        .resize = Allocator.noResize,
        .free = free,
      },
    };
  }

  pub fn ownsSlice(self: *ReverseFixedBufferAllocator, slice: []u8) bool {
    return sliceContainsSlice(self.buffer, slice);
  }

  /// NOTE: this will not work in all cases, if the last allocation had an adjusted_index
  ///       then we won't be able to determine what the last allocation was.  This is because
  ///       the alignForward operation done in alloc is not reversible.
  pub fn isLastAllocation(self: *ReverseFixedBufferAllocator, buf: []u8) bool {
    return buf.ptr == self.buffer.ptr;
  }

  fn alloc(ctx: *anyopaque, n: usize, log2_ptr_align: u8, ra: usize) ?[*]u8 {
    const self: *ReverseFixedBufferAllocator = @ptrCast(@alignCast(ctx));
    _ = ra;
    const ptr_align = @as(usize, 1) << @as(Allocator.Log2Align, @intCast(log2_ptr_align));
    if (n > self.end_index) return null;
    const new_offset = self.end_index - n;
    const aligned_ptr = std.mem.alignBackward(usize, @intFromPtr(self.buffer.ptr + new_offset), ptr_align);
    if (@intFromPtr(self.buffer.ptr) > aligned_ptr) return null;
    const aligned_new_offset = aligned_ptr - @intFromPtr(self.buffer.ptr);
    self.end_index = aligned_new_offset;
    return self.buffer.ptr + self.end_index;
  }

  fn free(
    ctx: *anyopaque,
    buf: []u8,
    log2_buf_align: u8,
    return_address: usize,
  ) void {
    const self: *ReverseFixedBufferAllocator = @ptrCast(@alignCast(ctx));
    _ = log2_buf_align;
    _ = return_address;
    std.debug.assert(@inComptime() or self.ownsSlice(buf));

    if (self.isLastAllocation(buf)) {
      self.end_index += buf.len;
    }
  }

  pub fn reset(self: *ReverseFixedBufferAllocator) void {
    self.end_index = self.buffer.len;
  }
};


var test_fixed_buffer_allocator_memory: [800000 * @sizeOf(u64)]u8 = undefined;
test "ReverseFixedBufferAllocator" {
  var fixed_buffer_allocator = std.mem.validationWrap(ReverseFixedBufferAllocator.init(test_fixed_buffer_allocator_memory[0..]));
  const allocator = fixed_buffer_allocator.allocator();

  try std.heap.testAllocator(allocator);
  try std.heap.testAllocatorAligned(allocator);
  try std.heap.testAllocatorLargeAlignment(allocator);
  try std.heap.testAllocatorAlignedShrink(allocator);
}

