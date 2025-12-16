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

const Battery = i64;
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
