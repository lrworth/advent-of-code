const std = @import("std");
const DisjointSetSized = @import("./DisjointSetSized.zig");
const ArrayList = std.ArrayList;
const fmt = std.fmt;
const hash_map = std.hash_map;
const AutoHashMapUnmanaged = hash_map.AutoHashMapUnmanaged;
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

const sampleAsVecs = &[_]Vec3{ .{ 162, 817, 812 }, .{ 57, 618, 57 }, .{ 906, 360, 560 }, .{ 592, 479, 940 }, .{ 352, 342, 300 }, .{ 466, 668, 158 }, .{ 542, 29, 236 }, .{ 431, 825, 988 }, .{ 739, 650, 466 }, .{ 52, 470, 668 }, .{ 216, 146, 977 }, .{ 819, 987, 18 }, .{ 117, 168, 530 }, .{ 805, 96, 715 }, .{ 346, 949, 466 }, .{ 970, 615, 88 }, .{ 941, 993, 340 }, .{ 862, 61, 35 }, .{ 984, 92, 344 }, .{ 425, 690, 689 } };

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

const IndexPair = struct {
    a: usize,
    b: usize,
    distance_sq: i64,
    fn init(vecs: []const Vec3, a: usize, b: usize) IndexPair {
        const diff = vecs[b] - vecs[a];
        const distance_sq: i64 = @reduce(.Add, diff * diff);

        return .{ .a = a, .b = b, .distance_sq = distance_sq };
    }
    fn distanceLessThan(_: void, lhs: IndexPair, rhs: IndexPair) bool {
        return lhs.distance_sq < rhs.distance_sq;
    }
};

fn indexPairsSortedByDistance(gpa: Allocator, vecs: []const Vec3) ![]IndexPair {
    if (vecs.len == 0) return try gpa.alloc(IndexPair, 0);
    const pairs = try gpa.alloc(IndexPair, ((vecs.len - 1) * vecs.len) / 2);
    errdefer gpa.free(pairs);
    var pairs_idx: usize = 0;
    for (0..vecs.len) |a_idx| {
        for (a_idx + 1..vecs.len) |b_idx| {
            pairs[pairs_idx] = IndexPair.init(vecs, a_idx, b_idx);
            pairs_idx += 1;
        }
    }

    mem.sortUnstable(IndexPair, pairs, {}, IndexPair.distanceLessThan);
    return pairs;
}

test indexPairsSortedByDistance {
    const coords = [_]Vec3{ .{ 1, 1, 1 }, .{ 5, 0, 3 }, .{ 0, 0, 1 } };
    const pairs = try indexPairsSortedByDistance(testing.allocator, &coords);
    defer testing.allocator.free(pairs);
    const expected_pairs = [_]IndexPair{ .{ .a = 0, .b = 2, .distance_sq = 2 }, .{ .a = 0, .b = 1, .distance_sq = 21 }, .{ .a = 1, .b = 2, .distance_sq = 29 } };
    try testing.expectEqualSlices(IndexPair, &expected_pairs, pairs);
}

fn multiplyThreeLargestCircuits(gpa: Allocator, index_pairs: []const IndexPair, size: usize) !u64 {
    const ds = try DisjointSetSized.init(gpa, size);
    defer ds.deinit(gpa);
    for (index_pairs) |ip| {
        ds.merge(ip.a, ip.b);
    }

    const sizes = try gpa.alloc(u64, size);
    defer gpa.free(sizes);

    for (ds.nodes, 0..) |node, i| {
        sizes[i] = node.size;
    }

    var threeBiggest = [3]u64{ 0, 0, 0 };
    for (sizes) |s| {
        if (s > threeBiggest[0]) {
            threeBiggest[0] = s;
            mem.sortUnstable(u64, &threeBiggest, {}, u64LessThan);
        }
    }
    var result: u64 = 1;
    for (threeBiggest) |b| {
        result *= b;
    }
    return result;
}

test "part 1 sample" {
    const gpa = testing.allocator;
    const index_pairs = try indexPairsSortedByDistance(gpa, sampleAsVecs);
    defer gpa.free(index_pairs);
    try testing.expectEqual(40, try multiplyThreeLargestCircuits(gpa, index_pairs[0..10], index_pairs.len));
}

fn disjointSetConnected(ds: DisjointSetSized) bool {
    const first = ds.findSet(0);
    for (1..ds.nodes.len) |i| {
        if (ds.findSet(i) != first) return false;
    }
    return true;
}

fn lastConnectionToConnected(gpa: Allocator, index_pairs: []const IndexPair, size: usize) !usize {
    const ds = try DisjointSetSized.init(gpa, size);
    defer ds.deinit(gpa);
    for (index_pairs, 0..) |ip, i| {
        // print("iteration {}: merging {} and {}\n", .{ i, ip.a, ip.b });
        ds.merge(ip.a, ip.b);
        if (disjointSetConnected(ds)) return i;
        // ds.print();
    }
    return error.NotConnectable;
}

fn lastConnectedXCoordsMultiplied(gpa: Allocator, vecs: []const Vec3) !i64 {
    const index_pairs = try indexPairsSortedByDistance(gpa, vecs);
    defer gpa.free(index_pairs);
    const last_index_pair = try lastConnectionToConnected(gpa, index_pairs, vecs.len);
    return vecs[index_pairs[last_index_pair].a][0] * vecs[index_pairs[last_index_pair].b][0];
}

test "part 2 sample" {
    const gpa = testing.allocator;
    try testing.expectEqual(25272, try lastConnectedXCoordsMultiplied(gpa, sampleAsVecs));
}

fn u64LessThan(_: void, lhs: u64, rhs: u64) bool {
    return lhs < rhs;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const input = try std.fs.cwd().readFileAlloc(allocator, "data/day8.txt", 10 * 1024 * 1024);
    const vecs = try parse(allocator, input);
    defer allocator.free(vecs);
    const index_pairs = try indexPairsSortedByDistance(allocator, vecs);
    defer allocator.free(index_pairs);

    const part1 = try multiplyThreeLargestCircuits(allocator, index_pairs[0..1000], index_pairs.len);
    const part2 = try lastConnectedXCoordsMultiplied(allocator, vecs);
    std.debug.print("part 1: {}\n", .{part1});
    std.debug.print("part 2: {}\n", .{part2});
}
