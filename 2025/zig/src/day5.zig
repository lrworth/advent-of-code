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

fn parse(gpa: std.mem.Allocator, buffer: []const u8) !Database {
    var sectionsIt = std.mem.splitSequence(u8, buffer, "\n\n");
    const rangesBuf = sectionsIt.next() orelse return error.MissingRangesSection;
    var ranges = std.ArrayList(Range){};
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
    var available = std.ArrayList(u64){};
    var availableIt = std.mem.splitScalar(u8, availableBuf, '\n');
    while (availableIt.next()) |availableIdBuf| {
        const availableId = try std.fmt.parseInt(u64, availableIdBuf, 10);
        try available.append(gpa, availableId);
    }

    return Database{ .ranges = try ranges.toOwnedSlice(gpa), .available = try available.toOwnedSlice(gpa) };
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
        .{ .first = 16, .last = 20 },
        .{ .first = 12, .last = 18 },
    },
    .available = &.{ 1, 5, 8, 11, 17, 32 },
};

test "parse" {
    const database = try parse(std.testing.allocator, sample);
    defer database.deinit(std.testing.allocator);
    try Database.expectEqual(sampleAsDatabase, database);
}
