//! Algorithms due to W. Lipski, Jr and F. P. Preparata.
//! https://scispace.com/pdf/efficient-algorithms-for-finding-maximum-matchings-in-convex-411ieyldz5.pdf

const std = @import("std");

const Interval = struct { beg: usize, end: usize };
const BipartiteGraphConvexOnA = struct {
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
    std.debug.assert(queue_buffer.len == n);

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
    queue.ensureTotalCapacityPrecise(n) catch unreachable;
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

const IntervalIterator = struct {
    max: usize,
    next_beg: usize = 0,
    next_end: usize = 0,
    fn next(self: *IntervalIterator) ?Interval {
        if (self.next_beg > self.max) return null;
        defer {
            if (self.next_end < self.max) {
                self.next_end += 1;
            } else {
                self.next_beg += 1;
                self.next_end = self.next_beg;
            }
        }
        return .{ .beg = self.next_beg, .end = self.next_end };
    }

    fn reset(self: *IntervalIterator) void {
        self.next_beg = 0;
        self.next_end = 0;
    }
};

test IntervalIterator {
    var it = IntervalIterator{ .max = 3 };
    try std.testing.expectEqualDeep(Interval{ .beg = 0, .end = 0 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 0, .end = 1 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 0, .end = 2 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 0, .end = 3 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 1, .end = 1 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 1, .end = 2 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 1, .end = 3 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 2, .end = 2 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 2, .end = 3 }, it.next());
    try std.testing.expectEqualDeep(Interval{ .beg = 3, .end = 3 }, it.next());
    try std.testing.expectEqualDeep(null, it.next());
    it.reset();
    try std.testing.expectEqualDeep(Interval{ .beg = 0, .end = 0 }, it.next());
}

/// IT should support next() and reset().
/// next() should return a T.
fn iteratorList(comptime IT: type, comptime T: type) type {
    return struct {
        const Self = @This();
        iterators: []IT,
        values: []T,
        /// `iterators` and `values` should have the same cardinality. `values` can be uninitialised. `iterators` and `values` are owned by the called.
        /// Initially, and after calling `reset()`, `next()` should return a value (not null).
        fn init(iterators: []IT, values: []T) Self {
            for (iterators, values) |*it, *val| {
                val.* = it.next().?;
            }
            return .{ .iterators = iterators, .values = values };
        }
        fn next(self: *Self) bool {
            for (self.iterators, self.values) |*it, *val| {
                if (it.next()) |x| {
                    val.* = x;
                    return true;
                } else {
                    it.reset();
                    val.* = it.next().?;
                }
            }
            return false;
        }
    };
}

test iteratorList {
    const IL = iteratorList(IntervalIterator, Interval);
    var iterators = [_]IntervalIterator{ .{ .max = 1 }, .{ .max = 1 } };
    var values: [2]Interval = undefined;
    var il = IL.init(&iterators, &values);
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 0 }, .{ .beg = 0, .end = 0 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 1 }, .{ .beg = 0, .end = 0 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 1, .end = 1 }, .{ .beg = 0, .end = 0 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 0 }, .{ .beg = 0, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 1 }, .{ .beg = 0, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 1, .end = 1 }, .{ .beg = 0, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 0 }, .{ .beg = 1, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 0, .end = 1 }, .{ .beg = 1, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(true, il.next());
    try std.testing.expectEqualDeep(&[2]Interval{ .{ .beg = 1, .end = 1 }, .{ .beg = 1, .end = 1 } }, &values);
    try std.testing.expectEqualDeep(false, il.next());
    try std.testing.expectEqualDeep(true, il.next());
}

test "maximumMatchingFast extensionally equal to maximumMatchingGloverSimple" {
    const IntervalIteratorList = iteratorList(IntervalIterator, Interval);
    const max_m = 6;
    const max_n = 6;
    inline for (1..max_m) |m| {
        inline for (1..max_n) |n| {
            // std.debug.print("m={} n={}\n", .{ m, n });

            var iterators: [n]IntervalIterator = undefined;
            for (&iterators) |*it| it.* = .{ .max = m - 1 };
            var values: [n]Interval = undefined;
            var iterator_list = IntervalIteratorList.init(&iterators, &values);
            while (true) : (if (!iterator_list.next()) break) {
                // `values` is the next list of intervals to check.

                // std.debug.print("values={any}\n", .{&values});

                var beg: [n]usize = undefined;
                var end: [n]usize = undefined;
                var ordbeg: [n]usize = undefined;
                var queue_buffer: [n]usize = undefined;
                var match: [m]?usize = undefined;
                for (values, &beg, &end) |v, *b, *e| {
                    b.* = v.beg;
                    e.* = v.end;
                }

                var simple_b: [n]?Interval = undefined;
                for (values, &simple_b) |v, *b| {
                    b.* = v;
                }
                var simple = BipartiteGraphConvexOnA{ .a_count = m, .b = &simple_b };

                const glover = try simple.maximumMatchingGloverSimple(std.testing.allocator);
                defer std.testing.allocator.free(glover);
                maximumMatchingFast(&beg, &end, &ordbeg, &queue_buffer, &match);

                var glover_cardinality: usize = 0;
                for (glover) |glover_| {
                    if (glover_) |_| glover_cardinality += 1;
                }

                var match_cardinality: usize = 0;
                for (match) |match_| {
                    if (match_) |_| match_cardinality += 1;
                }

                // const LessThanFn = struct {
                //     fn call(_: void, lhs: ?usize, rhs: ?usize) bool {
                //         if (lhs) |lhs_| {
                //             return if (rhs) |rhs_|
                //                 lhs_ < rhs_
                //             else
                //                 false;
                //         } else {
                //             return if (rhs) |_| true else false;
                //         }
                //     }
                // };

                // std.mem.sort(?usize, glover, {}, LessThanFn.call);
                // std.mem.sort(?usize, &match, {}, LessThanFn.call);

                // std.debug.print("glover={any}\n", .{glover});
                // std.debug.print("match={any}\n", .{&match});
                try std.testing.expectEqual(glover_cardinality, match_cardinality);
            }
        }
    }
}

test {
    _ = BipartiteGraphConvexOnA;
}
