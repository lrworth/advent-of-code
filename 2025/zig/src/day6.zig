const std = @import("std");
const print = std.debug.print;

const Operation = enum { add, multiply };

const Problem = struct { numbers: []const u64, operation: Operation };

const Result = struct { add: u64, multiply: u64 };

fn grandTotal(gpa: std.mem.Allocator, buffer: []const u8) !u64 {
    var lineIt = std.mem.splitScalar(u8, buffer, '\n');
    const firstLine = lineIt.next().?;
    var results: std.ArrayList(Result) =
        // There won't be more numbers than characters in the line.
        try std.ArrayList(Result).initCapacity(gpa, firstLine.len);
    defer results.deinit(gpa);

    var firstLineTokenIt = std.mem.splitScalar(u8, firstLine, ' ');
    while (firstLineTokenIt.next()) |tokenBuf| {
        if (tokenBuf.len == 0) continue;
        const num = try std.fmt.parseInt(u64, tokenBuf, 10);
        results.appendAssumeCapacity(Result{ .add = num, .multiply = num });
    }

    const operatorLineBuf = lines: while (lineIt.next()) |lineBuf| {
        var resultIdx: usize = 0;
        var tokenIt = std.mem.splitScalar(u8, lineBuf, ' ');
        while (tokenIt.next()) |tokenBuf| {
            if (tokenBuf.len == 0) continue;
            if (std.mem.eql(u8, tokenBuf, "*") or std.mem.eql(u8, tokenBuf, "+")) break :lines lineBuf;
            const num = try std.fmt.parseInt(u64, tokenBuf, 10);
            results.items[resultIdx].add += num;
            results.items[resultIdx].multiply *= num;
            resultIdx += 1;
        }
    } else return error.NoOperatorLine;

    var result: u64 = 0;
    var resultIdx: usize = 0;
    var tokenIt = std.mem.splitScalar(u8, operatorLineBuf, ' ');
    while (tokenIt.next()) |tokenBuf| {
        if (tokenBuf.len == 0) continue;
        result += if (std.mem.eql(u8, tokenBuf, "+"))
            results.items[resultIdx].add
        else if (std.mem.eql(u8, tokenBuf, "*"))
            results.items[resultIdx].multiply
        else
            return error.UnrecognisedOperator;
        resultIdx += 1;
    }
    return result;
}

const sample: []const u8 =
    \\123 328  51 64 
    \\ 45 64  387 23 
    \\  6 98  215 314
    \\*   +   *   +  
;

test grandTotal {
    try std.testing.expectEqual(4277556, try grandTotal(std.testing.allocator, sample));
}

const XYBuffer = struct {
    width: usize,
    height: usize,
    fn init(buffer: []const u8) ?XYBuffer {
        const width = std.mem.indexOfScalar(u8, buffer, '\n').?;
        // if (((buffer.len + 1) % (width + 1)) != 0) {
        //     print("uh oh, buffer.len+1 is {}, width+1 is {}\n", .{ buffer.len + 1, width + 1 });
        //     return null;
        // }
        const height: usize = @divTrunc(buffer.len + 1, width + 1);
        return .{ .width = width, .height = height };
    }
    fn getX(self: XYBuffer, index: usize) usize {
        return index % (self.width + 1);
    }
    fn getY(self: XYBuffer, index: usize) usize {
        return index / (self.width + 1);
    }
    fn getIndex(self: XYBuffer, x: usize, y: usize) usize {
        return (self.width + 1) * y + x;
    }
};

test "XYBuffer.getIndex" {
    const xyb = XYBuffer.init(sample).?;
    try std.testing.expectEqual(0, xyb.getIndex(0, 0));
    try std.testing.expectEqual(16, xyb.getIndex(0, 1));
    try std.testing.expectEqual(48, xyb.getIndex(0, xyb.height - 1));
    try std.testing.expectEqual(49, xyb.getIndex(1, xyb.height - 1));
}

const XYBufferRegion = struct {
    left: usize,
    width: usize,
    top: usize,
    height: usize,
    const RowIterator = struct {
        row: usize,
        xybr: XYBufferRegion,
        fn next(self: *RowIterator) ?usize {
            if (self.row < self.xybr.top + self.xybr.height) {
                const row = self.row;
                self.row += 1;
                return row;
            } else return null;
        }
    };
    const ColIterator = struct {
        col: usize,
        xybr: XYBufferRegion,
        fn next(self: *ColIterator) ?usize {
            if (self.col < self.xybr.left + self.xybr.width) {
                const col = self.col;
                self.col += 1;
                return col;
            } else return null;
        }
    };
    fn rowIterator(self: XYBufferRegion) RowIterator {
        return .{ .row = self.top, .xybr = self };
    }
    fn colIterator(self: XYBufferRegion) ColIterator {
        return .{ .col = self.left, .xybr = self };
    }
};

/// Returns an index into `buffer`.
fn nextOp(buffer: []const u8, startIdx: usize) ?usize {
    return std.mem.indexOfAnyPos(u8, buffer, startIdx, &.{ '+', '*' });
}

// test nextOp {
//     const xyb = XYBuffer.init(sample).?;
//     try std.testing.expectEqual(48, nextOp(sample, 0));
//     try std.testing.expectEqual(52, nextOp(sample, 1));
//     try std.testing.expectEqual(56, nextOp(sample, 5));
//     try std.testing.expectEqual(60, nextOp(sample, 9));
//     try std.testing.expectEqual(null, nextOp(sample, 13));
// }

fn grandTotal2(buffer: []const u8) !u64 {
    var total: u64 = 0;
    const xyb = XYBuffer.init(buffer) orelse return error.NonRectangularBuffer;
    var opIdx: ?usize = nextOp(buffer, xyb.getIndex(0, xyb.height - 1));
    while (opIdx) |opIdxNN| : (opIdx = nextOp(buffer, opIdxNN + 1)) {
        // print("opIdx {}\n", .{opIdxNN});
        const operation = switch (buffer[opIdxNN]) {
            '+' => Operation.add,
            '*' => Operation.multiply,
            else => return error.UnrecognisedOperator,
        };

        const nextOpIdx = nextOp(buffer, opIdxNN + 1);

        const opNumbersRegion =
            XYBufferRegion{ .left = xyb.getX(opIdxNN), .top = 0, .width = if (nextOpIdx) |noi|
                xyb.getX(noi) - 1 - xyb.getX(opIdxNN)
            else
                xyb.width - xyb.getX(opIdxNN), .height = xyb.height - 1 };
        // print("opNumbersRegion {}\n", .{opNumbersRegion});

        var subtotal: u64 = switch (operation) {
            .add => 0,
            .multiply => 1,
        };
        var opNumberColIt = opNumbersRegion.colIterator();
        while (opNumberColIt.next()) |opNumberCol| {
            var num: u64 = 0;
            var opNumberRowIt = opNumbersRegion.rowIterator();
            while (opNumberRowIt.next()) |opNumberRow| {
                const digitChar = buffer[xyb.getIndex(opNumberCol, opNumberRow)];
                if (digitChar == ' ' or digitChar == '\n') continue;
                // print("at index {} which is {}\n", .{ xyb.getIndex(opNumberCol, opNumberRow), digitChar });
                const digit = digitChar - '0';
                num *= 10;
                num += digit;
            }
            // print("found num {}\n", .{num});
            switch (operation) {
                .add => subtotal += num,
                .multiply => subtotal *= num,
            }
        }
        // print("adding {}\n", .{subtotal});
        total += subtotal;
    }
    return total;
}

test grandTotal2 {
    try std.testing.expectEqual(3263827, try grandTotal2(sample));
}

pub fn main() !void {
    const startns = std.time.nanoTimestamp();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day6.txt", 10 * 1024 * 1024);
    const part1 = try grandTotal(allocator, input);
    const part1endns = std.time.nanoTimestamp();
    const part2 = try grandTotal2(input);
    const part2endns = std.time.nanoTimestamp();
    std.debug.print("part 1: {}\n", .{part1});
    std.debug.print("took {}ns\n", .{part1endns - startns});
    std.debug.print("part 2: {}\n", .{part2});
    std.debug.print("took {}ns\n", .{part2endns - part1endns});
}
