const std = @import("std");
const print = std.debug.print;

const Range = struct { first: u64, last: u64 };

const Database = struct {
    ranges: []const Range,
    available: []const u64,
    fn deinit(self: *const Database, gpa: std.mem.Allocator) void {
        gpa.free(self.ranges);
        gpa.free(self.available);
    }
    fn expectEqual(expected: Database, actual: Database) !void {
        try std.testing.expectEqualSlices(Range, expected.ranges, actual.ranges);
        try std.testing.expectEqualSlices(u64, expected.available, actual.available);
    }
};

fn rangeLessThan(_: void, lhs: Range, rhs: Range) bool {
    return lhs.first < rhs.first;
}

fn parse(gpa: std.mem.Allocator, buffer: []const u8) !Database {
    const trimmed = std.mem.trim(u8, buffer, &.{'\n'});
    var sectionsIt = std.mem.splitSequence(u8, trimmed, "\n\n");
    const rangesBuf = sectionsIt.next() orelse return error.MissingRangesSection;
    var ranges = std.ArrayList(Range).empty;
    var rangesIt = std.mem.splitScalar(u8, rangesBuf, '\n');
    while (rangesIt.next()) |rangeBuf| {
        var endpointIt = std.mem.splitScalar(u8, rangeBuf, '-');
        const firstBuf = endpointIt.next() orelse return error.MissingFirst;
        const first = try std.fmt.parseInt(u64, firstBuf, 10);
        const lastBuf = endpointIt.next() orelse return error.MissingLast;
        const last = try std.fmt.parseInt(u64, lastBuf, 10);

        try ranges.append(gpa, Range{ .first = first, .last = last });
    }

    const availableBuf = sectionsIt.next() orelse return error.MissingAvailableSection;
    var available = std.ArrayList(u64).empty;
    var availableIt = std.mem.splitScalar(u8, availableBuf, '\n');
    while (availableIt.next()) |availableIdBuf| {
        const availableId = try std.fmt.parseInt(u64, availableIdBuf, 10);
        try available.append(gpa, availableId);
    }

    const sortedRanges = try ranges.toOwnedSlice(gpa);
    std.sort.block(Range, sortedRanges, {}, rangeLessThan);
    return Database{ .ranges = sortedRanges, .available = try available.toOwnedSlice(gpa) };
}

const sample: []const u8 =
    \\3-5
    \\10-14
    \\16-20
    \\12-18
    \\
    \\1
    \\5
    \\8
    \\11
    \\17
    \\32
;

const sampleAsDatabase: Database = .{
    .ranges = &.{
        .{ .first = 3, .last = 5 },
        .{ .first = 10, .last = 14 },
        .{ .first = 12, .last = 18 },
        .{ .first = 16, .last = 20 },
    },
    .available = &.{ 1, 5, 8, 11, 17, 32 },
};

test "parse" {
    const database = try parse(std.testing.allocator, sample);
    defer database.deinit(std.testing.allocator);
    try Database.expectEqual(sampleAsDatabase, database);
}

fn inRange(ingredient: u64, range: Range) bool {
    return range.first <= ingredient and ingredient <= range.last;
}

fn inRanges(ingredient: u64, ranges: []const Range) bool {
    for (ranges) |range| {
        if (inRange(ingredient, range)) return true;
    }
    return false;
}

fn countFresh(database: Database) u64 {
    var result: u64 = 0;
    for (database.available) |id| {
        if (inRanges(id, database.ranges)) result += 1;
    }
    return result;
}

test "inRanges" {
    try std.testing.expectEqual(3, countFresh(sampleAsDatabase));
}

fn toNonOverlapping(gpa: std.mem.Allocator, sortedRanges: []const Range) ![]const Range {
    var nonOverlappingRanges = try std.ArrayList(Range).initCapacity(gpa, sortedRanges.len);
    for (sortedRanges) |newRange| {
        var newRangeTrimmed = newRange;
        for (nonOverlappingRanges.items) |range| {
            if (newRangeTrimmed.first <= range.last) {
                newRangeTrimmed.first = range.last + 1;
            }
        }
        if (newRangeTrimmed.first <= newRangeTrimmed.last) {
            nonOverlappingRanges.appendAssumeCapacity(newRangeTrimmed);
        }
    }
    return nonOverlappingRanges.toOwnedSlice(gpa);
}

test toNonOverlapping {
    const sortedRanges: []const Range = &.{ .{ .first = 1, .last = 5 }, .{ .first = 2, .last = 3 }, .{ .first = 4, .last = 8 }, .{ .first = 5, .last = 5 }, .{ .first = 6, .last = 7 }, .{ .first = 10, .last = 12 } };
    const expectedNonOverlapping: []const Range = &.{ .{ .first = 1, .last = 5 }, .{ .first = 6, .last = 8 }, .{ .first = 10, .last = 12 } };
    const nonOverlapping = try toNonOverlapping(std.testing.allocator, sortedRanges);
    defer std.testing.allocator.free(nonOverlapping);
    try std.testing.expectEqualSlices(Range, expectedNonOverlapping, nonOverlapping);
}

fn numFreshIngredients(gpa: std.mem.Allocator, database: Database) !u64 {
    var result: u64 = 0;
    const nonOverlappingRanges = try toNonOverlapping(gpa, database.ranges);
    defer gpa.free(nonOverlappingRanges);
    for (nonOverlappingRanges) |range| {
        result += (range.last + 1) - range.first;
    }
    return result;
}

test "numFreshIngredients" {
    try std.testing.expectEqual(14, try numFreshIngredients(std.testing.allocator, sampleAsDatabase));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day5.txt", 10 * 1024 * 1024);
    const database = try parse(allocator, input);
    std.debug.print("part 1: {}\n", .{countFresh(database)});
    std.debug.print("part 2: {}\n", .{try numFreshIngredients(allocator, database)});
}
