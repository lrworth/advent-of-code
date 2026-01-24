//! Algorithms due to W. Lipski, Jr and F. P. Preparata.
//! https://scispace.com/pdf/efficient-algorithms-for-finding-maximum-matchings-in-convex-411ieyldz5.pdf

const std = @import("std");

const BipartiteGraphConvexOnA = struct {
    const Interval = struct { beg: usize, end: usize };
    a_count: usize,
    b: []const ?Interval,

    fn numEdges(self: BipartiteGraphConvexOnA) usize {
        var result: usize = 0;
        for (self.b) |b_edges| {
            if (b_edges) |interval| {
                result += (interval.end + 1) - interval.beg;
            }
        }
        return result;
    }

    /// O(|E|). Let m be the result. m represents a maximum matching of this
    /// graph. Caller owns the result.
    /// Algorithm 0 in the paper.
    pub fn maximumMatchingGloverSimple(self: BipartiteGraphConvexOnA, gpa: std.mem.Allocator) std.mem.Allocator.Error![]?usize {
        const b_deleted = try gpa.alloc(bool, self.b.len);
        defer gpa.free(b_deleted);
        for (b_deleted, 0..) |*it, i| it.* = (self.b[i] == null);

        var u = try std.ArrayList(usize).initCapacity(gpa, self.numEdges());
        defer u.deinit(gpa);

        const match = try gpa.alloc(?usize, self.a_count);
        errdefer comptime unreachable;
        for (0..self.a_count) |i| {
            {
                u.clearRetainingCapacity();
                for (self.b, 0..) |b_k, k| {
                    if (!b_deleted[k] and b_k.?.beg <= i and i <= b_k.?.end) {
                        u.appendAssumeCapacity(k);
                    }
                }
            }
            const j = blk: {
                var min_end: usize = std.math.maxInt(usize);
                var j: ?usize = null;
                for (u.items) |u_| {
                    if (self.b[u_].?.end < min_end) {
                        min_end = self.b[u_].?.end;
                        j = u_;
                    }
                }
                break :blk j;
            };
            if (j) |j_found| {
                match[i] = j_found;
                b_deleted[j_found] = true;
            } else {
                match[i] = null;
            }
        }
        return match;
    }

    test maximumMatchingGloverSimple {
        const gpa = std.testing.allocator;
        const g = BipartiteGraphConvexOnA{ .a_count = 5, .b = &.{
            .{ .beg = 1, .end = 2 },
            .{ .beg = 0, .end = 3 },
            .{ .beg = 0, .end = 4 },
            .{ .beg = 1, .end = 2 },
            .{ .beg = 4, .end = 4 },
        } };
        const match = try g.maximumMatchingGloverSimple(gpa);
        defer gpa.free(match);
        try std.testing.expectEqualDeep(&[_]?usize{ 1, 0, 3, 2, 4 }, match);
    }
};

/// Relabels indices of `beg` and `end` such that `end[0] <= ... <= end[n]`.
fn sortBegEnd(beg: []usize, end: []usize) void {
    std.debug.assert(beg.len == end.len);
    const Context = struct {
        beg: []usize,
        end: []usize,
        pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
            return ctx.end[a] < ctx.end[b];
        }
        pub fn swap(ctx: @This(), a: usize, b: usize) void {
            std.mem.swap(usize, &ctx.beg[a], &ctx.beg[b]);
            std.mem.swap(usize, &ctx.end[a], &ctx.end[b]);
        }
    };
    std.sort.pdqContext(0, beg.len, Context{ .beg = beg, .end = end });
}

test sortBegEnd {
    var beg = [_]usize{ 1, 2, 3, 4, 5, 6 };
    var end = [_]usize{ 1, 3, 2, 4, 3, 5 };
    sortBegEnd(&beg, &end);
    try std.testing.expectEqual([_]usize{ 1, 2, 3, 3, 4, 5 }, end);
    try std.testing.expectEqual([_]usize{ 1, 3, 2, 5, 4, 6 }, beg);
}

test {
    var beg = [_]usize{};
    var end = [_]usize{};
    sortBegEnd(&beg, &end);
    try std.testing.expectEqual([_]usize{}, end);
    try std.testing.expectEqual([_]usize{}, beg);
}

/// `ordbeg` is filled with indices of `beg` ordered such that
/// `beg[ordbeg[0]] <= ... <= beg[ordbeg[n]]`.
fn makeOrdbeg(beg: []const usize, ordbeg: []usize) void {
    std.debug.assert(beg.len == ordbeg.len);
    for (beg, 0..) |_, i| ordbeg[i] = i;
    const Context = struct {
        beg: []const usize,
        ordbeg: []usize,
        pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
            return ctx.beg[ctx.ordbeg[a]] < ctx.beg[ctx.ordbeg[b]];
        }
        pub fn swap(ctx: @This(), a: usize, b: usize) void {
            std.mem.swap(usize, &ctx.ordbeg[a], &ctx.ordbeg[b]);
        }
    };
    std.sort.pdqContext(0, ordbeg.len, Context{ .beg = beg, .ordbeg = ordbeg });
}

test makeOrdbeg {
    var beg = [_]usize{ 1, 3, 2, 4, 3, 5 };
    var ordbeg: [beg.len]usize = undefined;
    makeOrdbeg(&beg, &ordbeg);
    try std.testing.expectEqual([_]usize{ 0, 2, 1, 4, 3, 5 }, ordbeg);
}

/// Algorithm 1 in the paper.
/// Calculates maximum matching for a bipartite graph G = (A,B,E) convex on A,
/// where m = |A| and n = |B|. Assumes that every vertex has an adjacent edge
/// (any vertices without an adjacent edge would not be matched anyway).
///
/// For all k in 0..n, [`beg[k]`,`end[k]`] is an interval of vertices in A.
///
/// Size of `beg`, `end`, `ordbeg`, and `queue_buffer` are n. Size of `match` is m.
///
/// `beg` and `end` are reordered in-place. `ordbeg` and `queue_buffer` are
/// working buffers which do not need to be initialised. `match` is the result.
///
/// Note that relabelling vertices and computing ordbeg are done with PDQ sort
/// which are both O(n*log(n)). The paper recommends bucket sort as O(m + n)
/// but this requires (m + n)^2 space which is unacceptable.
pub fn maximumMatchingFast(beg: []usize, end: []usize, ordbeg: []usize, queue_buffer: []usize, match: []?usize) void {
    const m = match.len;
    const n = beg.len;
    std.debug.assert(end.len == n);
    std.debug.assert(ordbeg.len == n);
    std.debug.assert(match.len == n);

    sortBegEnd(beg, end);
    // std.debug.print("beg = {any}; end = {any}\n", .{ beg, end });
    makeOrdbeg(beg, ordbeg);
    // std.debug.print("ordbeg = {any}\n", .{ordbeg});

    const compareFn = struct {
        fn lessThan(context: void, a: usize, b: usize) std.math.Order {
            _ = context;
            // Lower numbers have higher priority.
            return std.math.order(a, b);
        }
    };

    var queue_alloc = std.heap.FixedBufferAllocator.init(@ptrCast(queue_buffer));

    var queue = std.PriorityQueue(usize, void, compareFn.lessThan).init(queue_alloc.allocator(), {});
    queue.ensureTotalCapacityPrecise(m) catch unreachable;
    var nb: usize = 0;
    var ne: usize = 0;

    for (0..m) |i| {
        // std.debug.print("i = {}\n", .{i});
        // std.debug.print("queue = {any}\n", .{queue.items});
        // Find vertex to be matched to i
        while (nb < n and beg[ordbeg[nb]] == i) {
            // std.debug.print("nb = {}\n", .{nb});
            queue.add(ordbeg[nb]) catch unreachable;
            // std.debug.print("queue = {any}\n", .{queue.items});
            nb += 1;
        }
        match[i] = queue.removeOrNull();
        // std.debug.print("queue = {any}\n", .{queue.items});
        while (ne < n and end[ne] == i) {
            // std.debug.print("ne = {}\n", .{ne});
            if (std.mem.indexOfScalar(usize, queue.items, ne)) |ne_i| {
                _ = queue.removeIndex(ne_i);
            }
            // std.debug.print("queue = {any}\n", .{queue.items});
            ne += 1;
        }
    }
}

test maximumMatchingFast {
    var beg = [_]usize{ 1, 0, 0, 1, 4 };
    var end = [_]usize{ 2, 3, 4, 2, 4 };
    var ordbeg: [beg.len]usize = undefined;
    var queue_buffer: [beg.len]usize = undefined;
    var match: [5]?usize = undefined;

    maximumMatchingFast(&beg, &end, &ordbeg, &queue_buffer, &match);
    try std.testing.expectEqual([_]?usize{ 2, 0, 1, 3, 4 }, match);
}

test {
    _ = BipartiteGraphConvexOnA;
}
