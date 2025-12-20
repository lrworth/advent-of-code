const std = @import("std");
const ArrayList = std.ArrayList;
const fmt = std.fmt;
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const print = std.debug.print;

const sample =
    \\162,817,812
    \\57,618,57
    \\906,360,560
    \\592,479,940
    \\352,342,300
    \\466,668,158
    \\542,29,236
    \\431,825,988
    \\739,650,466
    \\52,470,668
    \\216,146,977
    \\819,987,18
    \\117,168,530
    \\805,96,715
    \\346,949,466
    \\970,615,88
    \\941,993,340
    \\862,61,35
    \\984,92,344
    \\425,690,689
;

const sampleAsVecs = &.{ .{ 162, 817, 812 }, .{ 57, 618, 57 }, .{ 906, 360, 560 }, .{ 592, 479, 940 }, .{ 352, 342, 300 }, .{ 466, 668, 158 }, .{ 542, 29, 236 }, .{ 431, 825, 988 }, .{ 739, 650, 466 }, .{ 52, 470, 668 }, .{ 216, 146, 977 }, .{ 819, 987, 18 }, .{ 117, 168, 530 }, .{ 805, 96, 715 }, .{ 346, 949, 466 }, .{ 970, 615, 88 }, .{ 941, 993, 340 }, .{ 862, 61, 35 }, .{ 984, 92, 344 }, .{ 425, 690, 689 } };

const Vec3 = @Vector(3, i64);

fn parse(gpa: Allocator, buffer: []const u8) ![]Vec3 {
    var al: ArrayList(Vec3) = .{};
    defer al.deinit(gpa);
    var lineIt = mem.tokenizeScalar(u8, buffer, '\n');
    while (lineIt.next()) |line| {
        var coordIt = mem.tokenizeScalar(u8, line, ',');
        const x = try fmt.parseInt(i64, coordIt.next().?, 10);
        const y = try fmt.parseInt(i64, coordIt.next().?, 10);
        const z = try fmt.parseInt(i64, coordIt.next().?, 10);
        try al.append(gpa, Vec3{ x, y, z });
    }
    return al.toOwnedSlice(gpa);
}

test parse {
    const vecs = try parse(testing.allocator, sample);
    defer testing.allocator.free(vecs);
    try testing.expectEqualSlices(Vec3, sampleAsVecs, vecs);
}

const DistanceTable = struct {
    width: usize,
    distances: []i64,
    fn init(gpa: Allocator, coords: []const Vec3) !DistanceTable {
        const table: DistanceTable = .{ .width = coords.len, .distances = try gpa.alloc(i64, coords.len * coords.len) };
        for (coords, 0..) |a, a_index| {
            table.set(a_index, a_index, 0);
            for (coords[a_index + 1 ..], a_index + 1..) |b, b_index| {
                const diff = b - a;
                const distance_sq: i64 = @reduce(.Add, diff * diff);
                table.set(a_index, b_index, distance_sq);
                table.set(b_index, a_index, distance_sq);
            }
        }
        return table;
    }
    fn deinit(self: DistanceTable, gpa: Allocator) void {
        gpa.free(self.distances);
    }
    fn set(self: DistanceTable, x: usize, y: usize, val: i64) void {
        self.distances[y * self.width + x] = val;
    }
    fn get(self: DistanceTable, x: usize, y: usize) i64 {
        return self.distances[y * self.width + x];
    }
    fn expectEqual(expected: DistanceTable, actual: DistanceTable) !void {
        try testing.expectEqual(expected.width, actual.width);
        try testing.expectEqualSlices(i64, expected.distances, actual.distances);
    }
};

test DistanceTable {
    const coords = [_]Vec3{ .{ 1, 1, 1 }, .{ 5, 0, 3 }, .{ 0, 0, 1 } };
    const table = try DistanceTable.init(testing.allocator, &coords);
    defer table.deinit(testing.allocator);
    var expected_distances = [_]i64{ 0, 21, 2, 21, 0, 29, 2, 29, 0 };
    try DistanceTable.expectEqual(table, DistanceTable{ .width = coords.len, .distances = &expected_distances });
}

pub fn main() !void {
    // const startns = std.time.nanoTimestamp();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day8.txt", 10 * 1024 * 1024);
    const vecs = try parse(allocator, input);
    defer allocator.free(vecs);
    const table = try DistanceTable.init(allocator, vecs);
    defer table.deinit(allocator);
    print("{}", .{table});
    //
    // const diagram = Diagram.init(input);
    // const part1 = try beamSplittersHit(allocator, diagram);
    // const part2 = try timelines(allocator, diagram);
    // // const part1endns = std.time.nanoTimestamp();
    // // const part2 = try grandTotal2(input);
    // // const part2endns = std.time.nanoTimestamp();
    // std.debug.print("part 1: {}\n", .{part1});
    // // std.debug.print("took {}ns\n", .{part1endns - startns});
    // std.debug.print("part 2: {}\n", .{part2});
    // std.debug.print("took {}ns\n", .{part2endns - part1endns});
}
