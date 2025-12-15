const std = @import("std");

const Range = struct { firstId: u64, lastId: u64 };
const Ranges = []const Range;

fn parse(gpa: std.mem.Allocator, buffer: []const u8) !Ranges {
    var result = std.ArrayList(Range){};
    errdefer result.deinit(gpa);
    const trimmed = std.mem.trim(u8, buffer, &.{'\n'});
    var commaIt = std.mem.splitScalar(u8, trimmed, ',');
    while (commaIt.next()) |commaBuf| {
        var hyphenIt = std.mem.splitScalar(u8, commaBuf, '-');
        const firstIdTxt = hyphenIt.next() orelse return error.MissingFirstId;
        const firstId = try std.fmt.parseInt(u64, firstIdTxt, 10);
        const lastIdTxt = hyphenIt.next() orelse return error.MissingLastId;
        const lastId = try std.fmt.parseInt(u64, lastIdTxt, 10);
        try result.append(gpa, Range{ .firstId = firstId, .lastId = lastId });
    }
    return result.toOwnedSlice(gpa);
}

const sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

const sampleAsRanges: Ranges =
    &.{ .{ .firstId = 11, .lastId = 22 }, .{ .firstId = 95, .lastId = 115 }, .{ .firstId = 998, .lastId = 1012 }, .{ .firstId = 1188511880, .lastId = 1188511890 }, .{ .firstId = 222220, .lastId = 222224 }, .{ .firstId = 1698522, .lastId = 1698528 }, .{ .firstId = 446443, .lastId = 446449 }, .{ .firstId = 38593856, .lastId = 38593862 }, .{ .firstId = 565653, .lastId = 565659 }, .{ .firstId = 824824821, .lastId = 824824827 }, .{ .firstId = 2121212118, .lastId = 2121212124 } };

test "parse" {
    const ranges = try parse(std.testing.allocator, sample);
    try std.testing.expectEqualSlices(Range, sampleAsRanges, ranges);
    std.testing.allocator.free(ranges);
}

const SortedHalves = struct { small: u64, large: u64 };
fn sortHalves(n: u64) SortedHalves {
    const nFloat: f64 = @floatFromInt(n);
    const width = @ceil(@log10(nFloat));
    const halfWidth: u64 = @intFromFloat(@ceil(width / 2));
    const halfPower = std.math.pow(u64, 10, halfWidth);
    const leftHalf = n / halfPower;
    const rightHalf = n - leftHalf * halfPower;
    return .{ .small = @min(leftHalf, rightHalf), .large = @max(leftHalf, rightHalf) };
}

test sortHalves {
    try std.testing.expectEqual(SortedHalves{ .small = 1, .large = 1 }, sortHalves(11));
    try std.testing.expectEqual(SortedHalves{ .small = 1, .large = 2 }, sortHalves(12));
    try std.testing.expectEqual(SortedHalves{ .small = 1, .large = 2 }, sortHalves(21));
    try std.testing.expectEqual(SortedHalves{ .small = 1, .large = 23 }, sortHalves(123));
    try std.testing.expectEqual(SortedHalves{ .small = 12, .large = 34 }, sortHalves(1234));
    try std.testing.expectEqual(SortedHalves{ .small = 21, .large = 43 }, sortHalves(4321));
}

fn copyHalf(n: u64) u64 {
    if (n == 0) return 0;
    const nFloat: f64 = @floatFromInt(n);
    const width: u64 = @intFromFloat(@floor(@log10(nFloat) + 1));
    return n * std.math.pow(u64, 10, width) + n;
}

test copyHalf {
    try std.testing.expectEqual(0, copyHalf(0));
    try std.testing.expectEqual(11, copyHalf(1));
    try std.testing.expectEqual(22, copyHalf(2));
    try std.testing.expectEqual(1212, copyHalf(12));
    try std.testing.expectEqual(9876598765, copyHalf(98765));
}

// Look for numbers n that are, for some h, n = h + h * (10 ^ floor(1 + log_10 h))

const RangeInvalidIdIterator = struct {
    const Self = @This();

    range: Range,
    halfId: ?u64,
    fn init(range: Range) Self {
        var halfId = sortHalves(range.firstId).small;
        while (copyHalf(halfId) < range.firstId) {
            halfId += 1;
        }
        return .{ .range = range, .halfId = halfId };
    }
    fn next(self: *Self) ?u64 {
        if (self.halfId) |halfId| {
            const result = copyHalf(halfId);
            const newHalfId = halfId + 1;
            self.halfId = if (copyHalf(newHalfId) <= self.range.lastId) newHalfId else null;
            return if (result <= self.range.lastId) result else null;
        } else return null;
    }
};

test RangeInvalidIdIterator {
    var it = RangeInvalidIdIterator.init(Range{ .firstId = 21, .lastId = 1223 });
    try std.testing.expectEqual(22, it.next());
    try std.testing.expectEqual(33, it.next());
    try std.testing.expectEqual(44, it.next());
    try std.testing.expectEqual(55, it.next());
    try std.testing.expectEqual(66, it.next());
    try std.testing.expectEqual(77, it.next());
    try std.testing.expectEqual(88, it.next());
    try std.testing.expectEqual(99, it.next());
    try std.testing.expectEqual(1010, it.next());
    try std.testing.expectEqual(1111, it.next());
    try std.testing.expectEqual(1212, it.next());
    try std.testing.expectEqual(null, it.next());
}

fn sumInvalidIds(ranges: Ranges) u64 {
    var result: u64 = 0;
    for (ranges) |range| {
        var it = RangeInvalidIdIterator.init(range);
        while (it.next()) |invalidId| {
            result += invalidId;
        }
    }
    return result;
}

test "sumInvalidIds" {
    try std.testing.expectEqual(1227775554, sumInvalidIds(sampleAsRanges));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day2.txt", 10 * 1024 * 1024);
    const ranges = try parse(allocator, input);
    std.debug.print("part 1: {}\n", .{sumInvalidIds(ranges)});
}
