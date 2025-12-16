const std = @import("std");

const Layout = struct {
    positions: []const bool,
    width: usize,
    fn get(self: Layout, x: usize, y: usize) ?bool {
        self.positions[y * self.width + x];
    }
    fn expectEqual(expected: Layout, actual: Layout) !void {
        try std.testing.expectEqualSlices(bool, expected.positions, actual.positions);
        try std.testing.expectEqual(expected.width, actual.width);
    }
    fn deinit(self: Layout, gpa: std.mem.Allocator) void {
        gpa.free(self.positions);
    }
};

fn parse(gpa: std.mem.Allocator, buffer: []const u8) !Layout {
    const width: usize = std.mem.indexOfScalar(u8, buffer, '\n').?;
    var height: usize = 0;
    var lineIt = std.mem.splitScalar(u8, buffer, '\n');
    while (lineIt.next()) |_| {
        height += 1;
    }

    var positions = try gpa.alloc(bool, width * height);
    var idx: usize = 0;
    lineIt.reset();
    while (lineIt.next()) |line| {
        for (line) |char| {
            positions[idx] = switch (char) {
                '.' => false,
                '@' => true,
                else => return error.UnrecognisedChatacter,
            };
            idx += 1;
        }
    }
    return Layout{ .positions = positions, .width = width };
}
const sample: []const u8 =
    \\..@@.@@@@.
    \\@@@.@.@.@@
    \\@@@@@.@.@@
    \\@.@@@@..@.
    \\@@.@@@@.@@
    \\.@@@@@@@.@
    \\.@.@.@.@@@
    \\@.@@@.@@@@
    \\.@@@@@@@@.
    \\@.@.@@@.@.
;

const sampleAsLayout: Layout = .{ .positions = &.{
    false, false, true,  true,  false, true,  true,  true,  true,  false,
    true,  true,  true,  false, true,  false, true,  false, true,  true,
    true,  true,  true,  true,  true,  false, true,  false, true,  true,
    true,  false, true,  true,  true,  true,  false, false, true,  false,
    true,  true,  false, true,  true,  true,  true,  false, true,  true,
    false, true,  true,  true,  true,  true,  true,  true,  false, true,
    false, true,  false, true,  false, true,  false, true,  true,  true,
    true,  false, true,  true,  true,  false, true,  true,  true,  true,
    false, true,  true,  true,  true,  true,  true,  true,  true,  false,
    true,  false, true,  false, true,  true,  true,  false, true,  false,
}, .width = 10 };

test "parse" {
    const layout = try parse(std.testing.allocator, sample);
    defer layout.deinit(std.testing.allocator);
    try Layout.expectEqual(sampleAsLayout, layout);
}
