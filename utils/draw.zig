const std = @import("std");

const fontfile = @embedFile("dos_8x8_font_white.pbm");

pub fn DrawContext(comptime pwidth: u32, comptime pheight: u32) type {
  return struct {
    const Self = @This();

    pub const contextWidth = pwidth;
    pub const contextHeight = pheight;

    pub var buffer = [_]u32 { 0 } ** (pwidth * pheight);

    pub var color: u32 = 0xFFFFFFFF;
    pub var thickness: u32 = 0;

    var _transform: [6]f32 = .{ 1, 0, 0, 1, 0, 0 };
    const _a = 0; const _b = 1; const _c = 2; const _d = 3; const _e = 4; const _f = 5;

    var stack: [6]f32 = .{ 1, 0, 0, 1, 0, 0 };

    // pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    //   allocator.free(self.font);
    // }

    // resets (overrides) the current transformation to the identity matrix, and
    // then invokes a transformation described by the arguments of this method.
    // This lets you scale, rotate, translate (move), and skew the context.
    //                                              a c e
    // The transformation matrix is described by: [ b d f ]
    //                                              0 0 1
    pub fn setTransform(a: f32, b: f32, c: f32, d: f32, e: f32, f: f32) void {
      _transform = .{ a, b, c, d, e, f };
    }
    // multiplies the current transformation with the matrix described by the
    // arguments of this method. This lets you scale, rotate, translate (move),
    // and skew the context.
    pub fn transform(a: f32, b: f32, c: f32, d: f32, e: f32, f: f32) void {
      _transform[_a] = _transform[_a] * a + _transform[_c] * b;
      _transform[_b] = _transform[_b] * a + _transform[_d] * b;
      _transform[_c] = _transform[_a] * c + _transform[_c] * d;
      _transform[_d] = _transform[_b] * c + _transform[_d] * d;
      _transform[_e] = _transform[_a] * e + _transform[_c] * f + _transform[_e] * 1;
      _transform[_f] = _transform[_b] * e + _transform[_d] * f + _transform[_f] * 1;
    }
    // retrieves the current transformation matrix being applied to the context.
    pub fn getTransform() [6]f32 {
      return _transform;
    }
    // Saves the entire state of the canvas by pushing the current state onto a
    // stack.
    // ⚠️ Only one level of stack for now.
    pub fn save() !void {
      stack = _transform;
    }
    // Restores the most recently saved canvas state by popping the top entry
    // in the drawing state stack. If there is no saved state, this method does
    // nothing.
    pub fn restore() void {
      _transform = stack;
    }
    // resets the rendering context to its default state, allowing it to be
    // reused for drawing something else without having to explicitly reset all
    // the properties.
    pub fn reset() void {
      _transform = .{ 1, 0, 0, 1, 0, 0 };
    }
    // adds a translation transformation to the current matrix.
    pub fn translate(x: f32, y: f32) void {
      _transform[_e] += x;
      _transform[_f] += y;
    }
    // erases the pixels in a rectangular area by setting them to transparent
    // black.
    // ⚠️ Operates in buffer space (do not take the transformation matrix into account)
    pub fn clearRect(x: i16, y: i16, width: i16, height: i16) void {
      if (x == 0 and y == 0 and width == pwidth and height == pheight) {
        @memset(&buffer, color);
      } else {
        @panic("clearReact on sizes different from the canvas is not yet implemented");
      }
    }
    // Renders a rectangle with a starting point is at (x, y) and whose size is
    // specified by width and height.
    pub fn rect(x: i16, y: i16, width: i16, height: i16) void {
      line(x, y, x + width, y);
      line(x + width, y, x + width, y + height);
      line(x + width, y + height, x, y + height);
      line(x, y + height, x, y);
    }
    // Draws a line.
    pub fn line(startx: i16, starty: i16, endx: i16, endy: i16) void {
      // std.debug.assert(startx >= 0 and starty >= 0 and endx >= 0 and endy >= 0);

      // This is commented because, although debug performances are much better,
      // release performances are worst!
      const builtin = @import("builtin");
      if (builtin.mode == .Debug) {
        // This whole block is an optimization for vertical and horizontal line
        const ux =
          _transform[_a] * @as(f32, @floatFromInt(startx)) +
          _transform[_c] * @as(f32, @floatFromInt(starty)) + _transform[_e];
        var uy =
          _transform[_b] * @as(f32, @floatFromInt(startx)) +
          _transform[_d] * @as(f32, @floatFromInt(starty)) + _transform[_f];
        const vx =
          _transform[_a] * @as(f32, @floatFromInt(endx)) +
          _transform[_c] * @as(f32, @floatFromInt(endy)) + _transform[_e];
        var vy =
          _transform[_b] * @as(f32, @floatFromInt(endx)) +
          _transform[_d] * @as(f32, @floatFromInt(endy)) + _transform[_f];

        if (ux > 0 and uy > 0 and vx > 0 and vy > 0 and
            ux < contextWidth and uy < contextHeight and vx < contextWidth and vy < contextHeight) {
          // If a line is entirely in the canvas
          if (ux == vx) {
            // vertical line
            const x: u16 = @intFromFloat(ux);
            if (uy > vy) std.mem.swap(@TypeOf(uy), &uy, &vy);
            var y: u16 = @intFromFloat(uy);
            while (y < @as(u16, @intFromFloat(vy))) : (y += 1) {
              buffer[y * contextWidth + x] = color;
            }
            return;
          } else if (uy == vy) {
            // horizontal line
            var startBuffer = @as(u16, @intFromFloat(uy)) * contextWidth + @as(u16, @intFromFloat(ux));
            var endBuffer = @as(u16, @intFromFloat(vy)) * contextWidth + @as(u16, @intFromFloat(vx));
            if (startBuffer > endBuffer) std.mem.swap(@TypeOf(startBuffer), &startBuffer, &endBuffer);
            @memset(buffer[startBuffer..endBuffer], color);
            return;
          }
        }
      }
      // Otherwise, we use a general but slow algorithm
      drawThickLine(startx, starty, endx, endy);
      // drawLineOverlap(startx, starty, endx, endy, 0);
      // drawLineWu(startx, starty, endx, endy, 0);
    }
    // Draws a point.
    pub inline fn plot(x: i16, y: i16, acolor: u32) void {
      // std.log.debug("plot x {} y {} width {} height {} index {} buffer.len {}", .{
      //   x, y, contextWidth, contextHeight,
      //   @as(u16, @bitCast(y)) * width + @as(u16, @bitCast(x)),
      //   buffer.len,
      // });
      const vx = _transform[_a] * @as(f32, @floatFromInt(x)) + _transform[_c] * @as(f32, @floatFromInt(y)) + _transform[_e];
      const vy = _transform[_b] * @as(f32, @floatFromInt(x)) + _transform[_d] * @as(f32, @floatFromInt(y)) + _transform[_f];
      if (vx >= 0 and vx < contextWidth and vy >= 0 and vy < contextHeight) {
        buffer[@as(u16, @intFromFloat(vy)) * contextWidth + @as(u16, @intFromFloat(vx))] = acolor;
      }
    }
    // Writes text at the specified position. x and y specifies the top left
    // corner of the text box to be printed.
    pub fn printText(x: i16, y: i16, text: []const u8) void {
      // @setEvalBranchQuota(10000);
      const fontparams = comptime lbl: {
        // Check we deal with a P1 netpbm file (ASCII text)
        std.debug.assert(fontfile[0] == 'P' and fontfile[1] == '1');
        // Retrieve width and height
        var i = 2;
        while (std.ascii.isWhitespace(fontfile[i])) i += 1;
        var j = i;
        while (!std.ascii.isWhitespace(fontfile[j])) j += 1;
        const fontwidth = try std.fmt.parseInt(usize, fontfile[i..j], 10);
        i = j;
        while (std.ascii.isWhitespace(fontfile[i])) i += 1;
        j = i;
        while (!std.ascii.isWhitespace(fontfile[j])) j += 1;
        const fontheight = try std.fmt.parseInt(usize, fontfile[i..j], 10);
        // Get position of first value
        while (std.ascii.isWhitespace(fontfile[j])) j += 1;
        break :lbl .{ fontwidth, fontheight, j };
      };

      const fontwidth = fontparams[0];
      const fontindex = fontparams[2];

      for (text, 0..) |c, cindex| {
        const cusize = @as(usize, @intCast(c));
        for (0..8) |j| {
          for (0..8) |i| {
            // fontwidth + 1 because of the \n
            if (fontfile[fontindex + j * (fontwidth + 1) + cusize * 8 + i] != '0') {
              plot(x + @as(i16, @intCast(i)) + @as(i16, @intCast(cindex * 8)), y + @as(i16, @intCast(j)), color);
            }
          }
        }
      }
    }
    // Modified Bresenham draw(line) with optional overlap. Required for drawThickLine().
    // Overlap draws additional pixel when changing minor direction. For standard bresenham overlap, choose LINE_OVERLAP_NONE (0).
    //
    //  Sample line:
    //
    //    00+
    //     -0000+
    //         -0000+
    //             -00
    //
    //  0 pixels are drawn for normal line without any overlap LINE_OVERLAP_NONE
    //  + pixels are drawn if LINE_OVERLAP_MAJOR
    //  - pixels are drawn if LINE_OVERLAP_MINOR

    const Overlap = enum(u8) {
      LINE_OVERLAP_NONE = 0,
      LINE_OVERLAP_MINOR = 1,
      LINE_OVERLAP_MAJOR = 2,
      LINE_OVERLAP_BOTH = 3,
    };

    fn drawLineOverlap(pstartx: i16, pstarty: i16, endx: i16, endy: i16, aOverlap: u8) void {
      var tStepX: i16 = 0;
      var tStepY: i16 = 0;
      var tDeltaXTimes2: i16 = 0;
      var tDeltaYTimes2: i16 = 0;
      var tError: i16 = 0;
      var startx = pstartx;
      var starty = pstarty;
      // calculate direction
      var tDeltaX = endx - startx;
      var tDeltaY = endy - starty;
      if (tDeltaX < 0) {
        tDeltaX = -tDeltaX;
        tStepX = -1;
      } else {
        tStepX = 1;
      }
      if (tDeltaY < 0) {
        tDeltaY = -tDeltaY;
        tStepY = -1;
      } else {
        tStepY = 1;
      }
      tDeltaXTimes2 = tDeltaX << 1;
      tDeltaYTimes2 = tDeltaY << 1;
      // draw start pixel
      plot(startx, starty, color);
      if (tDeltaX > tDeltaY) {
        // start value represents a half step in Y direction
        tError = tDeltaYTimes2 - tDeltaX;
        while (startx != endx) {
          // step in main direction
          startx += tStepX;
          if (tError >= 0) {
            if (aOverlap & @intFromEnum(Overlap.LINE_OVERLAP_MAJOR) != 0) {
              // draw pixel in main direction before changing
              plot(startx, starty, color);
            }
            // change Y
            starty += tStepY;
            if (aOverlap & @intFromEnum(Overlap.LINE_OVERLAP_MINOR) != 0) {
              // draw pixel in minor direction before changing
              plot(startx - tStepX, starty, color);
            }
            tError -= tDeltaXTimes2;
          }
          tError += tDeltaYTimes2;
          plot(startx, starty, color);
        }
      } else {
        tError = tDeltaXTimes2 - tDeltaY;
        while (starty != endy) {
          starty += tStepY;
          if (tError >= 0) {
            if (aOverlap & @intFromEnum(Overlap.LINE_OVERLAP_MAJOR) != 0) {
              // draw pixel in main direction before changing
              plot(startx, starty, color);
            }
            startx += tStepX;
            if (aOverlap & @intFromEnum(Overlap.LINE_OVERLAP_MINOR) != 0) {
              // draw pixel in minor direction before changing
              plot(startx, starty - tStepY, color);
            }
            tError -= tDeltaYTimes2;
          }
          tError += tDeltaXTimes2;
          plot(startx, starty, color);
        }
      }
    }

    // fractional part of x
    fn fpart(x: f32) f32 {
      return x - @floor(x);
    }

    fn rfpart(x: f32) f32 {
      return 1 - fpart(x);
    }

    fn shadeColor(R: f32, G: f32, B: f32, ratio: f32) u32 {
      return @as(u32, @intFromFloat(R * ratio))  |
        @as(u32, @intFromFloat(G * ratio)) << 8  |
        @as(u32, @intFromFloat(B * ratio)) << 16 |
        0xFF000000; // Always full alpha
    }

    fn drawLineWu(px0: i16, py0: i16, px1: i16, py1: i16, unused: u8) void {
      std.log.debug("drawLineWu {} {} {} {}", .{ px0, py0, px1, py1 });
      _ = unused;
      var x0: f32 = @floatFromInt(px0);
      var y0: f32 = @floatFromInt(py0);
      var x1: f32 = @floatFromInt(px1);
      var y1: f32 = @floatFromInt(py1);
      const R: f32 = @floatFromInt(color & 0x000000FF);
      const G: f32 = @floatFromInt((color & 0x0000FF00) >> 8);
      const B: f32 = @floatFromInt((color & 0x00FF0000) >> 16);

      const steep = @abs(y1 - y0) > @abs(x1 - x0);

      if (steep) {
        std.mem.swap(f32, &x0, &y0);
        std.mem.swap(f32, &x1, &y1);
        std.log.debug("drawLineWu2 {d} {d} {d} {d}", .{ x0, y0, x1, y1 });
      }
      if (x0 > x1) {
        std.mem.swap(f32, &x0, &x1);
        std.mem.swap(f32, &y0, &y1);
        std.log.debug("drawLineWu3 {d} {d} {d} {d}", .{ x0, y0, x1, y1 });
      }

      const dx = x1 - x0;
      const dy = y1 - y0;

      var gradient: f32 = 1.0;
      if (dx != 0.0) {
        gradient = dy / dx;
      }

      // handle first endpoint
      var xend = @round(x0);
      var yend = y0 + gradient * (xend - x0);
      var xgap = rfpart(x0 + 0.5);
      const xpxl1: i16 = @intFromFloat(xend); // this will be used in the main loop
      const ypxl1: i16 = @intFromFloat(@floor(yend));
      if (steep) {
        plot(ypxl1    , xpxl1    , shadeColor(R, G, B, rfpart(yend) * xgap));
        plot(ypxl1 + 1, xpxl1    , shadeColor(R, G, B,  fpart(yend) * xgap));
      } else {
        plot(xpxl1    , ypxl1    , shadeColor(R, G, B, rfpart(yend) * xgap));
        plot(xpxl1    , ypxl1 + 1, shadeColor(R, G, B,  fpart(yend) * xgap));
      }
      var intery = yend + gradient; // first y-intersection for the main loop

      // handle second endpoint
      std.log.debug("x1 {d} y1 {d}", .{ x1, y1 });
      xend = @round(x1);
      yend = y1 + gradient * (xend - x1);
      std.log.debug("xend {d} yend {d}", .{ xend, yend });
      xgap = fpart(x1 + 0.5);
      const xpxl2: i16 = @intFromFloat(xend); //this will be used in the main loop
      const ypxl2: i16 = @intFromFloat(@floor(yend));
      std.log.debug("xpxl2 {} ypxl2 {}", .{ xpxl2, ypxl2 });
      if (steep) {
        plot(ypxl2    , xpxl2    , shadeColor(R, G, B, rfpart(yend) * xgap));
        plot(ypxl2 + 1, xpxl2    , shadeColor(R, G, B,  fpart(yend) * xgap));
      } else {
        plot(xpxl2    , ypxl2    , shadeColor(R, G, B, rfpart(yend) * xgap));
        plot(xpxl2    , ypxl2 + 1, shadeColor(R, G, B,  fpart(yend) * xgap));
      }

      // main loop
      if (steep) {
        var i = xpxl1 + 1;
        while (i < xpxl2 - 1) {
          plot(@as(i16, @intFromFloat(@floor(intery)))    , i, shadeColor(R, G, B, rfpart(intery)));
          plot(@as(i16, @intFromFloat(@floor(intery))) + 1, i, shadeColor(R, G, B,  fpart(intery)));
          intery = intery + gradient;
          i += 1;
        }
      } else {
        var i = xpxl1 + 1;
        while (i < xpxl2 - 1) {
          plot(i, @as(i16, @intFromFloat(@floor(intery)))    , shadeColor(R, G, B, rfpart(intery)));
          plot(i, @as(i16, @intFromFloat(@floor(intery))) + 1, shadeColor(R, G, B,  fpart(intery)));
          intery = intery + gradient;
          i += 1;
        }
      }
    }


    //
    // The same as before, but no clipping to display range, some pixel are drawn twice (because of using LINE_OVERLAP_BOTH)
    // and direction of thickness changes for each octant (except for LINE_THICKNESS_MIDDLE and thickness value is odd)
    // thicknessMode can be LINE_THICKNESS_MIDDLE or any other value
    //
    fn drawThickLine(pstartx: i16, pstarty: i16, pendx: i16, pendy: i16) void {
      var tStepX: i16 = 0;
      var tStepY: i16 = 0;
      var tDeltaXTimes2: i16 = 0;
      var tDeltaYTimes2: i16 = 0;
      var tError: i16 = 0;
      var startx = pstartx;
      var starty = pstarty;
      var endx = pendx;
      var endy = pendy;

      var tDeltaY = startx - endx;
      var tDeltaX = endy - startx;
      // mirror 4 quadrants to one and adjust deltas and stepping direction
      if (tDeltaX < 0) {
        tDeltaX = -tDeltaX;
        tStepX = -1;
      } else {
        tStepX = 1;
      }
      if (tDeltaY < 0) {
        tDeltaY = -tDeltaY;
        tStepY = -1;
      } else {
        tStepY = 1;
      }
      tDeltaXTimes2 = tDeltaX << 1;
      tDeltaYTimes2 = tDeltaY << 1;
      var tOverlap: Overlap = Overlap.LINE_OVERLAP_NONE;
      // which octant are we now
      if (tDeltaX > tDeltaY) {
        // if (we want to draw the original coordinate in the middle of the thick line)
        {
          // adjust draw start point
          tError = tDeltaYTimes2 - tDeltaX;
          var i = thickness / 2;
          while (i > 0) {
            // change X (main direction here)
            startx -= tStepX;
            endx -= tStepX;
            if (tError >= 0) {
                // change Y
                starty -= tStepY;
                endy -= tStepY;
                tError -= tDeltaXTimes2;
            }
            tError += tDeltaYTimes2;
            i -= 1;
          }
        }
        drawLineOverlap(startx, starty, endx, endy, @intFromEnum(tOverlap));
        // drawLineWu(startx, starty, endx, endy, @intFromEnum(tOverlap));
        // draw thickness lines
        tError = tDeltaYTimes2 - tDeltaX;
        var i = thickness;
        while (i > 1) {
          // change X (main direction here)
          startx += tStepX;
          endx += tStepX;
          tOverlap = Overlap.LINE_OVERLAP_NONE;
          if (tError >= 0) {
            // change Y
            startx += tStepY;
            endy += tStepY;
            tError -= tDeltaXTimes2;
            tOverlap = Overlap.LINE_OVERLAP_BOTH;
          }
          tError += tDeltaYTimes2;
          drawLineOverlap(startx, starty, endx, endy, @intFromEnum(tOverlap));
          // drawLineWu(startx, starty, endx, endy, @intFromEnum(tOverlap));
          i -= 1;
        }
      } else {
        // if (we want to draw the original coordinate in the middle of the thick line)
        {
          tError = tDeltaXTimes2 - tDeltaY;
          var i = thickness / 2;
          while (i > 0) {
            starty -= tStepY;
            endy -= tStepY;
            if (tError >= 0) {
                startx -= tStepX;
                endx -= tStepX;
                tError -= tDeltaYTimes2;
            }
            tError += tDeltaXTimes2;
            i -= 1;
          }
        }
        drawLineOverlap(startx, starty, endx, endy, @intFromEnum(tOverlap));
        // drawLineWu(startx, starty, endx, endy, @intFromEnum(tOverlap));
        tError = tDeltaXTimes2 - tDeltaY;
        var i = thickness;
        while (i > 1) {
          starty += tStepY;
          endy += tStepY;
          tOverlap = Overlap.LINE_OVERLAP_NONE;
          if (tError >= 0) {
            startx += tStepX;
            endx += tStepX;
            tError -= tDeltaYTimes2;
            tOverlap = Overlap.LINE_OVERLAP_BOTH;
          }
          tError += tDeltaXTimes2;
          drawLineOverlap(startx, starty, endx, endy, @intFromEnum(tOverlap));
          // drawLineWu(startx, starty, endx, endy, @intFromEnum(tOverlap));
          i -= 1;
        }
      }
    }
  };
}
