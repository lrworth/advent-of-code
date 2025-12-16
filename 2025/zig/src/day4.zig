const std = @import("std");
const print = std.debug.print;

const Coordinates = struct { x: usize, y: usize };
const Grid = struct {
    positions: []bool,
    width: usize,
    height: usize,
    fn get(self: Grid, coords: Coordinates) bool {
        return self.positions[self.indexForCoordinates(coords)];
    }
    fn put(self: *Grid, coords: Coordinates, newVal: bool) void {
        self.positions[self.indexForCoordinates(coords)] = newVal;
    }
    fn indexForCoordinates(self: Grid, coords: Coordinates) usize {
        return coords.y * self.width + coords.x;
    }
    fn expectEqual(expected: Grid, actual: Grid) !void {
        try std.testing.expectEqualSlices(bool, expected.positions, actual.positions);
        try std.testing.expectEqual(expected.width, actual.width);
    }
    fn clone(self: Grid, gpa: std.mem.Allocator) !Grid {
        return Grid{ .positions = blk: {
            const positions = try gpa.alloc(bool, self.positions.len);
            @memcpy(positions, self.positions);
            break :blk positions;
        }, .width = self.width, .height = self.height };
    }
    fn deinit(self: Grid, gpa: std.mem.Allocator) void {
        gpa.free(self.positions);
    }

    fn nw(_: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x > 0 and coords.y > 0)
            Coordinates{ .x = coords.x - 1, .y = coords.y - 1 }
        else
            null;
    }
    fn n(_: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.y > 0)
            Coordinates{ .x = coords.x, .y = coords.y - 1 }
        else
            null;
    }
    fn ne(self: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x < self.width - 1 and coords.y > 0)
            Coordinates{ .x = coords.x + 1, .y = coords.y - 1 }
        else
            null;
    }
    fn w(_: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x > 0)
            Coordinates{ .x = coords.x - 1, .y = coords.y }
        else
            null;
    }
    fn e(self: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x < self.width - 1)
            Coordinates{ .x = coords.x + 1, .y = coords.y }
        else
            null;
    }
    fn sw(self: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x > 0 and coords.y < self.height - 1)
            Coordinates{ .x = coords.x - 1, .y = coords.y + 1 }
        else
            null;
    }
    fn s(self: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.y < self.height - 1)
            Coordinates{ .x = coords.x, .y = coords.y + 1 }
        else
            null;
    }
    fn se(self: Grid, coords: Coordinates) ?Coordinates {
        return if (coords.x < self.width - 1 and coords.y < self.height - 1)
            Coordinates{ .x = coords.x + 1, .y = coords.y + 1 }
        else
            null;
    }
};

fn parse(gpa: std.mem.Allocator, buffer: []const u8) !Grid {
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
    return Grid{ .positions = positions, .width = width, .height = height };
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

var samplePositions = [_]bool{
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
};
const sampleAsGrid: Grid = .{ .positions = &samplePositions, .width = 10, .height = 10 };

test "parse" {
    const grid = try parse(std.testing.allocator, sample);
    defer grid.deinit(std.testing.allocator);
    try Grid.expectEqual(sampleAsGrid, grid);
}

fn numAdjacentOccupied(grid: Grid, coords: Coordinates) u64 {
    var result: u64 = 0;
    if (grid.nw(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.n(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.ne(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.w(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.e(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.sw(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.s(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    if (grid.se(coords)) |c| {
        if (grid.get(c))
            result += 1;
    }
    return result;
}

fn rollAccessible(grid: Grid, coords: Coordinates) bool {
    return numAdjacentOccupied(grid, coords) < 4;
}

fn numRollsAccessible(grid: Grid) u64 {
    var result: u64 = 0;
    for (0..grid.height) |y| {
        for (0..grid.width) |x| {
            const coords = Coordinates{ .x = x, .y = y };
            if (grid.get(coords) and rollAccessible(grid, coords)) {
                result += 1;
            }
        }
    }
    return result;
}

test "numRollsAccessible" {
    try std.testing.expectEqual(13, numRollsAccessible(sampleAsGrid));
}

fn removeAccessible(grid: *Grid) u64 {
    var numRemoved: u64 = 0;
    for (0..grid.height) |y| {
        for (0..grid.width) |x| {
            const coords = Coordinates{ .x = x, .y = y };
            if (grid.get(coords) and rollAccessible(grid.*, coords)) {
                grid.put(coords, false);
                numRemoved += 1;
            }
        }
    }
    return numRemoved;
}

fn repeatRemoveAccessible(grid: *Grid) u64 {
    var numRemoved: u64 = 0;
    var numJustRemoved = removeAccessible(grid);
    while (numJustRemoved > 0) {
        numRemoved += numJustRemoved;
        numJustRemoved = removeAccessible(grid);
    }

    return numRemoved;
}

test "repeatRemoveAccessible" {
    var mutableGrid = try sampleAsGrid.clone(std.testing.allocator);
    defer mutableGrid.deinit(std.testing.allocator);
    try std.testing.expectEqual(43, repeatRemoveAccessible(&mutableGrid));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day4.txt", 10 * 1024 * 1024);
    var grid = try parse(allocator, input);
    std.debug.print("part 1: {}\n", .{numRollsAccessible(grid)});
    std.debug.print("part 2: {}\n", .{repeatRemoveAccessible(&grid)});
}
