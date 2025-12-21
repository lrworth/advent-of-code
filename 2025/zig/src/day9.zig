const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;

const sample =
    \\7,1
    \\11,1
    \\11,7
    \\9,7
    \\9,5
    \\2,5
    \\2,3
    \\7,3
;

const Vec2 = @Vector(2, i64);

const sampleAsVecs = &[_]Vec2{
    .{ 7, 1 },
    .{ 11, 1 },
    .{ 11, 7 },
    .{ 9, 7 },
    .{ 9, 5 },
    .{ 2, 5 },
    .{ 2, 3 },
    .{ 7, 3 },
};

fn parse(gpa: Allocator, buffer: []const u8) ![]Vec2 {
    var al: ArrayList(Vec2) = .{};
    defer al.deinit(gpa);
    var lineIt = mem.tokenizeScalar(u8, buffer, '\n');
    while (lineIt.next()) |line| {
        var coordIt = mem.tokenizeScalar(u8, line, ',');
        const x = try fmt.parseInt(i64, coordIt.next().?, 10);
        const y = try fmt.parseInt(i64, coordIt.next().?, 10);
        try al.append(gpa, Vec2{ x, y });
    }
    return al.toOwnedSlice(gpa);
}

test parse {
    const vecs = try parse(testing.allocator, sample);
    defer testing.allocator.free(vecs);
    try testing.expectEqualSlices(Vec2, sampleAsVecs, vecs);
}

fn largestSquare(corners: []const Vec2) u64 {
    var result: u64 = 0;
    for (corners, 0..) |corner_1, corner_1_idx| {
        const x_1, const y_1 = corner_1;
        for (corners[corner_1_idx + 1 ..]) |corner_2| {
            const x_2, const y_2 = corner_2;
            const topLeft = Vec2{ @min(x_1, x_2), @min(y_1, y_2) };
            const bottomRight = Vec2{ @max(x_1, x_2), @max(y_1, y_2) };
            const size = @reduce(.Mul, @abs(bottomRight + Vec2{ 1, 1 } - topLeft));
            if (size > result) result = size;
        }
    }
    return result;
}

test "part 1 sample" {
    try testing.expectEqual(50, largestSquare(sampleAsVecs));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day9.txt", 10 * 1024 * 1024);
    const vecs = try parse(allocator, input);

    const part1 = largestSquare(vecs);
    // const part2 = try lastConnectedXCoordsMultiplied(allocator, vecs);
    std.debug.print("part 1: {}\n", .{part1});
    // std.debug.print("part 2: {}\n", .{part2});
}
