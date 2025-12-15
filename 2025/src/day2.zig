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

fn smallestPiece(n: u64, numPieces: u64) ?u64 {
    const nFloat: f64 = @floatFromInt(n);
    const numPiecesFloat: f64 = @floatFromInt(numPieces);
    const width = @ceil(@log10(nFloat));
    const widthPerPiece: u64 = @intFromFloat(@ceil(width / numPiecesFloat));
    const powerPerPiece = std.math.pow(u64, 10, widthPerPiece);
    // std.debug.print("widthPerPiece {}, powerPerPiece {}\n", .{ widthPerPiece, powerPerPiece });

    var nRemaining = n;
    var smallest: ?u64 = null;
    for (0..numPieces) |idx| {
        const divisor = std.math.pow(u64, powerPerPiece, numPieces - idx - 1);
        const thisPiece = if (divisor == 0) nRemaining else (nRemaining / divisor);
        // std.debug.print("piece {}, nRemaining {}, divisor {}, thisPiece {}, smallest {?}\n", .{ idx, nRemaining, divisor, thisPiece, smallest });
        nRemaining -= thisPiece * divisor;
        smallest = if (smallest) |sp| @min(sp, thisPiece) else thisPiece;
    }
    return smallest;
}

test smallestPiece {
    try std.testing.expectEqual(1, smallestPiece(11, 2));
    try std.testing.expectEqual(1, smallestPiece(12, 2));
    try std.testing.expectEqual(1, smallestPiece(21, 2));
    try std.testing.expectEqual(1, smallestPiece(123, 2));
    try std.testing.expectEqual(12, smallestPiece(1234, 2));
    try std.testing.expectEqual(21, smallestPiece(4321, 2));

    try std.testing.expectEqual(101, smallestPiece(101112, 2));
    try std.testing.expectEqual(111, smallestPiece(111210, 2));
    try std.testing.expectEqual(11, smallestPiece(121011, 2));

    try std.testing.expectEqual(10, smallestPiece(101112, 3));
    try std.testing.expectEqual(10, smallestPiece(111210, 3));
    try std.testing.expectEqual(10, smallestPiece(121011, 3));

    try std.testing.expectEqual(0, smallestPiece(101112, 6));
    try std.testing.expectEqual(0, smallestPiece(111210, 6));
    try std.testing.expectEqual(0, smallestPiece(121011, 6));
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

fn repeatPiece(n: u64, numPieces: u64) u64 {
    if (n == 0) return 0;
    const nFloat: f64 = @floatFromInt(n);
    const width: u64 = @intFromFloat(@floor(@log10(nFloat) + 1));
    const widthPower: u64 = std.math.pow(u64, 10, width);
    var result: u64 = 0;
    for (0..numPieces) |_| {
        result *= widthPower;
        result += n;
    }
    return result;
}

test repeatPiece {
    try std.testing.expectEqual(0, repeatPiece(0, 2));
    try std.testing.expectEqual(11, repeatPiece(1, 2));
    try std.testing.expectEqual(22, repeatPiece(2, 2));
    try std.testing.expectEqual(1212, repeatPiece(12, 2));
    try std.testing.expectEqual(9876598765, repeatPiece(98765, 2));
    try std.testing.expectEqual(111, repeatPiece(1, 3));
    try std.testing.expectEqual(222, repeatPiece(2, 3));
    try std.testing.expectEqual(121212, repeatPiece(12, 3));
    try std.testing.expectEqual(987659876598765, repeatPiece(98765, 3));
}

const RangeInvalidIdIterator = struct {
    const Self = @This();

    range: Range,
    repeats: u64,
    generator: ?u64,
    fn init(range: Range, repeats: u64) Self {
        var generator: ?u64 = null;
        if (smallestPiece(range.firstId, repeats)) |smallest| {
            var g = smallest;
            while (repeatPiece(g, repeats) < range.firstId) {
                g += 1;
            }
            generator = g;
        }
        return .{ .range = range, .repeats = repeats, .generator = generator };
    }
    fn next(self: *Self) ?u64 {
        if (self.generator) |generator| {
            const result = repeatPiece(generator, self.repeats);
            const newGenerator = generator + 1;
            self.generator = if (repeatPiece(newGenerator, self.repeats) <= self.range.lastId) newGenerator else null;
            return if (result <= self.range.lastId) result else null;
        } else return null;
    }
};

test RangeInvalidIdIterator {
    var it = RangeInvalidIdIterator.init(Range{ .firstId = 21, .lastId = 1223 }, 2);
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
        var it = RangeInvalidIdIterator.init(range, 2);
        while (it.next()) |invalidId| {
            result += invalidId;
        }
    }
    return result;
}

test "sumInvalidIds" {
    try std.testing.expectEqual(1227775554, sumInvalidIds(sampleAsRanges));
}

fn sumInvalidIdsAny(gpa: std.mem.Allocator, ranges: Ranges) !u64 {
    var result: u64 = 0;
    for (ranges) |range| {
        const lastIdFloat: f64 = @floatFromInt(range.lastId);
        const lastIdWidth: u64 = @intFromFloat(@floor(@log10(lastIdFloat) + 1));
        var seenIds = std.hash_map.AutoHashMapUnmanaged(u64, void).empty;
        defer seenIds.deinit(gpa);
        for (2..lastIdWidth + 1) |repeats| {
            var it = RangeInvalidIdIterator.init(range, repeats);
            while (it.next()) |invalidId| {
                // std.debug.print("invalid id {}\n", .{invalidId});
                if (!seenIds.contains(invalidId)) {
                    result += invalidId;
                    try seenIds.put(gpa, invalidId, {});
                }
            }
        }
    }
    return result;
}

test "sumInvalidIdsAny" {
    try std.testing.expectEqual(4174379265, sumInvalidIdsAny(std.testing.allocator, sampleAsRanges));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day2.txt", 10 * 1024 * 1024);
    const ranges = try parse(allocator, input);
    std.debug.print("part 1: {}\n", .{sumInvalidIds(ranges)});
    std.debug.print("part 2: {}\n", .{try sumInvalidIdsAny(allocator, ranges)});
}
