const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub const OptionDescription = struct {
  short: ?[]const u8,
  long: []const u8,
  arg: ?struct { name: []const u8, type: type } = null,
  help: []const u8,
};

pub const ArgDescriptor = struct {
  bufferSize: usize = 1024,
  name: []const u8,
  description: ?[]const u8 = null,
  withHelp: bool = true,
  version: ?[]const u8 = null,
  expectArgs: []const []const u8 = &[_][]const u8{},
  options: []const OptionDescription,
};

pub fn findOption(comptime T: type, value: anytype, argsInfo: std.builtin.Type.Struct,
    name: []const u8) ?type {
  inline for (argsInfo.fields) |field| {
    if (std.mem.eql(u8, field.name, name) and field.type == T) {
      return @field(value, field.name);
    }
  }
  return null;
}

pub fn printUsage(allocator: std.mem.Allocator, argsDescriptor: ArgDescriptor) void {
  stdout.print("Usage: {s}{s}{s}\n", .{
    argsDescriptor.name,
    if (argsDescriptor.options.len > 0) " [OPTIONS]" else "",
    if (argsDescriptor.expectArgs.len > 0) blk: {
      const argsStr = std.mem.join(allocator, " ", argsDescriptor.expectArgs)
        catch @panic("increase fixed buffer size");
      break :blk std.fmt.allocPrint(allocator, " {s}", .{ argsStr })
        catch @panic("increase fixed buffer size");
    } else "",
  }) catch unreachable;
}

pub fn printHelp(allocator: std.mem.Allocator, argsDescriptor: ArgDescriptor) void {
  stdout.print("{s}{s} {s}\n", .{
    argsDescriptor.name,
    if (argsDescriptor.version) |version| " (" ++ version ++ ")" else "",
    argsDescriptor.description orelse "",
  }) catch unreachable;

  stdout.print("\n", .{}) catch unreachable;
  printUsage(allocator, argsDescriptor);
  stdout.print("\nOptions:\n", .{}) catch unreachable;
  inline for (argsDescriptor.options) |option| {
    var buffer: [argsDescriptor.bufferSize]u8 = undefined;
    const printed = std.fmt.bufPrint(&buffer, "    {s}{s}{s}", .{
      if (option.short) |short| "-" ++ short ++ "," else "   ",
      "--" ++ option.long,
      if (option.arg) |arg| " " ++ arg.name else "",
    }) catch @panic("increase fixed buffer size");
    if (printed.len > 23) {
      stdout.print("{s}\n                         {s}\n", .{ printed, option.help })
        catch unreachable;
    } else {
      stdout.print("{s: <24} {s}\n", .{ printed, option.help }) catch unreachable;
    }
  }
}

pub const Args = struct {
  const Self = @This();

  switchMap: std.StringHashMap(bool),
  optionMap: std.StringHashMap([]const u8),
  arguments: std.ArrayList([]const u8),

  pub fn getSwitch(self: Self, name: []const u8) bool {
    return self.switchMap.get(name) orelse false;
  }

  pub fn getOption(self: Self, comptime T: type, name: []const u8) ?T {
    return self.optionMap.get(name);
  }
};

pub fn parser(argsDescriptor: ArgDescriptor) type {
  return struct {
    var buffer: [argsDescriptor.bufferSize]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();
    var argsStore = Args{
      .switchMap = std.StringHashMap(bool).init(allocator),
      .optionMap = std.StringHashMap([]const u8).init(allocator),
      .arguments = std.ArrayList([]const u8).init(allocator),
    };

    pub fn parse(args: [][:0]u8) Args {
      if (argsDescriptor.withHelp) {
        // Look for help and print it
        for (args) |arg| {
          if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printHelp(allocator, argsDescriptor);
            std.posix.exit(0);
          }
        }

        var i: u16 = 1;
        while (i < args.len) {
          const arg = args[i];
          if (arg[0] == '-') {
            // Handle option in the block. i might be incremented additionally
            // it the option expects an argument.
            inline for (argsDescriptor.options) |option| {
              if ((option.short != null and std.mem.eql(u8, arg[1..], option.short.?)) or
                std.mem.eql(u8, arg[2..], option.long)) {
                argsStore.switchMap.put(option.long, true)
                    catch @panic("increase fixed buffer size");
                if (option.arg) |optionArg| {
                  _ = optionArg;
                  // We have an argument to the option
                  if (i > args.len - 1 or args[i + 1][0] == '-') {
                    // Missing argument
                    stderr.print("error: option {s} expected an argument\n", .{ arg })
                        catch unreachable;
                    printUsage(allocator, argsDescriptor);
                  }
                  argsStore.optionMap.put(option.long, args[i + 1])
                    catch @panic("increase fixed buffer size");
                  i += 1;
                }
                break;
              }
            } else {
              // An option was provided but not described.
              stderr.print("error: unknown option {s}\n", .{ arg }) catch unreachable;
              printUsage(allocator, argsDescriptor);
              std.posix.exit(1);
            }
          } else {
            // Here are the argument to the program.
            argsStore.arguments.append(args[i]) catch unreachable;
          }
          i += 1;
        }
      }

      if (argsStore.arguments.items.len != argsDescriptor.expectArgs.len) {
        stderr.print("error: incorrect number of arguments. Expected {} arguments, {} given.\n", .{
          argsDescriptor.expectArgs.len, argsStore.arguments.items.len,
        }) catch unreachable;
        printUsage(allocator, argsDescriptor);
        std.posix.exit(1);
      }
      return argsStore;
    }
  };
}
