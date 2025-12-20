const std = @import("std");
const print = std.debug.print;

const sample =
    \\.......S.......
    \\...............
    \\.......^.......
    \\...............
    \\......^.^......
    \\...............
    \\.....^.^.^.....
    \\...............
    \\....^.^...^....
    \\...............
    \\...^.^...^.^...
    \\...............
    \\..^...^.....^..
    \\...............
    \\.^.^.^.^.^...^.
    \\...............
;

const Diagram = struct {
    input_buffer: []const u8,
    width: usize,
    fn init(input_buffer: []const u8) Diagram {
        return .{ .input_buffer = input_buffer, .width = std.mem.indexOfScalar(u8, input_buffer, '\n').? };
    }
    fn lines(self: Diagram) std.mem.SplitIterator(u8, .scalar) {
        return std.mem.splitScalar(u8, self.input_buffer, '\n');
    }
};

fn beamSplittersHit(gpa: std.mem.Allocator, diagram: Diagram) !u64 {
    const beams = try gpa.alloc(bool, diagram.width);
    defer gpa.free(beams);
    @memset(beams, false);

    var result: u64 = 0;

    var lineIt = diagram.lines();
    while (lineIt.next()) |line| {
        for (line, 0..) |char, i| {
            switch (char) {
                'S' => beams[i] = true,
                '^' => if (beams[i]) {
                    result += 1;
                    beams[i] = false;
                    beams[i - 1] = true;
                    beams[i + 1] = true;
                },
                else => {},
            }
        }
    }

    return result;
}

test beamSplittersHit {
    const diagram = Diagram.init(sample);
    try std.testing.expectEqual(21, try beamSplittersHit(std.testing.allocator, diagram));
}

fn timelines(gpa: std.mem.Allocator, diagram: Diagram) !u64 {
    const timelinesPerCol = try gpa.alloc(u64, diagram.width);
    defer gpa.free(timelinesPerCol);
    @memset(timelinesPerCol, 0);

    var lineIt = diagram.lines();
    while (lineIt.next()) |line| {
        for (line, 0..) |char, i| {
            switch (char) {
                'S' => timelinesPerCol[i] = 1,
                '^' => {
                    const n = timelinesPerCol[i];
                    timelinesPerCol[i] = 0;
                    timelinesPerCol[i - 1] += n;
                    timelinesPerCol[i + 1] += n;
                },
                else => {},
            }
        }
    }

    var result: u64 = 0;
    for (timelinesPerCol) |n| {
        result += n;
    }
    return result;
}

test timelines {
    const diagram = Diagram.init(sample);
    try std.testing.expectEqual(40, try timelines(std.testing.allocator, diagram));
}

pub fn main() !void {
    // const startns = std.time.nanoTimestamp();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day7.txt", 10 * 1024 * 1024);
    const diagram = Diagram.init(input);
    const part1 = try beamSplittersHit(allocator, diagram);
    const part2 = try timelines(allocator, diagram);
    // const part1endns = std.time.nanoTimestamp();
    // const part2 = try grandTotal2(input);
    // const part2endns = std.time.nanoTimestamp();
    std.debug.print("part 1: {}\n", .{part1});
    // std.debug.print("took {}ns\n", .{part1endns - startns});
    std.debug.print("part 2: {}\n", .{part2});
    // std.debug.print("took {}ns\n", .{part2endns - part1endns});
}
