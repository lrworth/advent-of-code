const std = @import("std");

pub fn main() !void {
    const input = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, "data/day1.txt", 10 * 1024 * 1024);
    std.debug.print("Part 1: {}\n", .{try solve1(input)});
    std.debug.print("Part 2: {}\n", .{try solve2(input)});
}

fn solve1(input: []const u8) !u32 {
    const rotations = try parse(input);
    const numZeroes = applyRotations(rotations);
    return numZeroes[0];
}

fn solve2(input: []const u8) !u32 {
    const rotations = try parse(input);
    const numZeroes = applyRotations(rotations);
    return numZeroes[1];
}

fn Dial(size: u32) type {
    return struct {
        const Self = @This();
        value: u32,
        fn invert(self: Self) Self {
            return Self{ .value = (size - self.value) % size };
        }
        fn rotate(self: Self, rotation: Rotation, zeroPasses: *u32) Self {
            return switch (rotation.direction) {
                .left => self.rotateLeft(rotation.clicks, zeroPasses),
                .right => self.rotateRight(rotation.clicks, zeroPasses),
            };
        }
        fn rotateRight(self: Self, clicks: u32, zeroPasses: *u32) Self {
            zeroPasses.* = (self.value + clicks) / size;
            return Self{ .value = (self.value + clicks) % size };
        }
        fn rotateLeft(self: Self, clicks: u32, zeroPasses: *u32) Self {
            return self.invert().rotateRight(clicks, zeroPasses).invert();
        }
    };
}

test {
    const dial = Dial(3){ .value = 1 };
    var zeroPasses: u32 = undefined;
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 1 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 1 }, &zeroPasses);
        try std.testing.expectEqual(0, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 2 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 2 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 3 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 3 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 4 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 4 }, &zeroPasses);
        try std.testing.expectEqual(1, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 5 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 5 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 6 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 6 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
    }
    {
        _ = dial.rotate(Rotation{ .direction = .left, .clicks = 7 }, &zeroPasses);
        try std.testing.expectEqual(3, zeroPasses);
        _ = dial.rotate(Rotation{ .direction = .right, .clicks = 7 }, &zeroPasses);
        try std.testing.expectEqual(2, zeroPasses);
    }
}

const Direction = enum { left, right };
const Rotation = struct { direction: Direction, clicks: u32 };
const Rotations = []const Rotation;

fn parse(input: []const u8) !Rotations {
    var linesIt = std.mem.splitScalar(u8, input, '\n');
    var result = std.ArrayList(Rotation).empty;
    while (linesIt.next()) |line| {
        if (line.len == 0) break;
        try result.append(std.heap.page_allocator, .{ .direction = switch (line[0]) {
            'L' => .left,
            'R' => .right,
            else => return error.UnknownDirection,
        }, .clicks = try std.fmt.parseUnsigned(u32, line[1..], 10) });
    }
    return result.toOwnedSlice(std.heap.page_allocator);
}

fn applyRotations(rotations: Rotations) struct { u32, u32 } {
    var zeroPasses: u32 = 0;
    var zeroes: u32 = 0;
    var dial = Dial(100){ .value = 50 };
    for (rotations) |rotation| {
        var newZeroPasses: u32 = undefined;
        dial = dial.rotate(rotation, &newZeroPasses);
        if (dial.value == 0) {
            zeroes += 1;
        }
        zeroPasses += newZeroPasses;
    }
    return .{ zeroes, zeroPasses };
}

test "Day 1 example" {
    const input =
        \\L68
        \\L30
        \\R48
        \\L5
        \\R60
        \\L55
        \\L1
        \\L99
        \\R14
        \\L82
    ;
    try std.testing.expectEqual(3, solve1(input));
    try std.testing.expectEqual(6, solve2(input));
}
