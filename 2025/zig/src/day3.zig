const std = @import("std");

const sample =
    \\987654321111111
    \\811111111111119
    \\234234234234278
    \\818181911112111
;

const sampleAsBanks: []const Bank = &.{
    &.{ 9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1 },
    &.{ 8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9 },
    &.{ 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8 },
    &.{ 8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1 },
};

const Battery = u64;
const Bank = []const Battery;

fn freeBanks(gpa: std.mem.Allocator, banks: []Bank) void {
    for (banks) |bank| {
        gpa.free(bank);
    }
    gpa.free(banks);
}

var cursor: usize = undefined;

fn parse(gpa: std.mem.Allocator, buffer: []const u8) ![]Bank {
    var banks = std.ArrayList(Bank){};
    var bankInIt = std.mem.splitScalar(u8, buffer, '\n');
    while (bankInIt.next()) |bankIn| {
        if (bankIn.len > 0) {
            var bank = std.ArrayList(Battery){};
            for (bankIn) |battery| {
                try bank.append(gpa, battery - '0');
            }
            try banks.append(gpa, try bank.toOwnedSlice(gpa));
        }
    }
    return banks.toOwnedSlice(gpa);
}

test "parse" {
    const banks = try parse(std.testing.allocator, sample);
    defer freeBanks(std.testing.allocator, banks);
    for (banks, sampleAsBanks) |bank, sampleAsBank| {
        try std.testing.expectEqualSlices(Battery, sampleAsBank, bank);
    }
}

fn largestJoltage(bank: Bank, numBatteries: usize) !u64 {
    if (numBatteries == 0) return 0;
    const haystack = bank[0 .. bank.len - (numBatteries - 1)];
    if (haystack.len == 0) {
        return error.HaystackTooSmall;
    }
    const maxIdx = std.mem.indexOfMax(Battery, haystack);
    const battery = haystack[maxIdx];
    const remainingJoltage = try largestJoltage(bank[maxIdx + 1 .. bank.len], numBatteries - 1);
    return battery * std.math.pow(Battery, 10, numBatteries - 1) + remainingJoltage;
}

test "largestJoltage" {
    try std.testing.expectEqual(98, largestJoltage(sampleAsBanks[0], 2));
    try std.testing.expectEqual(89, largestJoltage(sampleAsBanks[1], 2));
    try std.testing.expectEqual(78, largestJoltage(sampleAsBanks[2], 2));
    try std.testing.expectEqual(92, largestJoltage(sampleAsBanks[3], 2));
}

fn totalJoltage(banks: []const Bank) !u64 {
    var result: u64 = 0;
    for (banks) |bank| {
        result += try largestJoltage(bank, 2);
    }
    return result;
}

test "totalJoltage" {
    try std.testing.expectEqual(357, totalJoltage(sampleAsBanks));
}

fn totalJoltageOverriden(banks: []const Bank) !u64 {
    var result: u64 = 0;
    for (banks) |bank| {
        result += try largestJoltage(bank, 12);
    }
    return result;
}

test "totalJoltageOverriden" {
    try std.testing.expectEqual(3121910778619, totalJoltageOverriden(sampleAsBanks));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day3.txt", 10 * 1024 * 1024);
    const banks = try parse(allocator, input);
    std.debug.print("part 1: {}\n", .{try totalJoltage(banks)});
    std.debug.print("part 2: {}\n", .{try totalJoltageOverriden(banks)});
}
