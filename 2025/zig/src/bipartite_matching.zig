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

    var queue = std.PriorityQueue(usize, void, compareFn.lessThan).empty;
    queue.ensureTotalCapacityPrecise(queue_alloc.allocator(), n) catch unreachable;
    var nb: usize = 0;
    var ne: usize = 0;

    for (0..m) |i| {
        // std.debug.print("i = {}\n", .{i});
        // std.debug.print("queue = {any}\n", .{queue.items});
        // Find vertex to be matched to i
        while (nb < n and beg[ordbeg[nb]] == i) {
            // std.debug.print("nb = {}\n", .{nb});
            queue.push(queue_alloc.allocator(), ordbeg[nb]) catch unreachable;
            // std.debug.print("queue = {any}\n", .{queue.items});
            nb += 1;
        }
        match[i] = queue.pop();
        // std.debug.print("queue = {any}\n", .{queue.items});
        while (ne < n and end[ne] == i) {
            // std.debug.print("ne = {}\n", .{ne});
            if (std.mem.indexOfScalar(usize, queue.items, ne)) |ne_i| {
                _ = queue.popIndex(ne_i);
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
    const max_m = 5;
    const max_n = 5;
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

/// Algorithm 2 in the paper.
/// For all k in 0..n, [`beg[k]`,`end[k]`] is an interval of vertices in A.
///
/// Size of `beg`, `end`, `s_buffer`, `stack`, `beg_relabelled`, `end_relabelled`, `sub1`, `sub2`, and `y` are n.
/// `beg` and `end` are reordered in-place. `s_buffer`, `stack`, `sub1`, and `sub2` are working buffers which do not need to be initialised.
/// `y` is the output. It does not need to be initialised.
///
/// For each segment j, y[j] is the *placement* of that segment in the final
/// arrangement, that is, producing a convex polygon when plotted on a graph.
///
/// e.g. if y = { 0, 2, 3, 1 }, then plotted from bottom to top:
///
///   segment 2
///   segment 1
///   segment 3
///   segment 0
///
/// TODO: There are a number of deficiencies with this:
/// - It should assert its postcondition, that beg/end have been ordered as
///   intended and that the resultant ordering in y produces the kind of shape we
///   are after.
/// - Surely we don't need all of those temporary buffers to be passed in.
/// - We only need to run a full sorting algorithm once and the other two can
///   be replaced with an interval-reversing algorithm, since the new ordering
///   is trivial given the original sorting.
/// - It is weirdly called "test" but does not produce a decision. This is the
///   original naming from the paper.
/// - I have left the commented-out debugging code there because it is so hard
///   to write. We should have a better way of logging progress through the
///   algorithm. This would be useful if we ever want to produce a
///   visualisation of the operation of the algorithm.
pub fn testDoubleConvexity(beg: []usize, end: []usize, s_buffer: []usize, stack: []usize, beg_relabelled: []usize, end_relabelled: []usize, sub1_buffer: []usize, sub2_buffer: []usize, y: []usize) void {
    const n: usize = beg.len;
    std.debug.assert(end.len == n);
    std.debug.assert(s_buffer.len == n);
    std.debug.assert(stack.len == n);
    std.debug.assert(beg_relabelled.len == n);
    std.debug.assert(end_relabelled.len == n);
    std.debug.assert(sub1_buffer.len == n);
    std.debug.assert(sub2_buffer.len == n);
    std.debug.assert(y.len == n);

    const SortBoth = struct {
        beg: []usize,
        end: []usize,
        pub fn swap(self: @This(), a: usize, b: usize) void {
            std.mem.swap(usize, &self.beg[a], &self.beg[b]);
            std.mem.swap(usize, &self.end[a], &self.end[b]);
        }
        pub fn lessThan(self: @This(), a: usize, b: usize) bool {
            return switch (std.math.order(self.beg[a], self.beg[b])) {
                .gt => false,
                .lt => true,
                .eq => self.end[a] < self.end[b],
            };
        }
    };
    // std.debug.print("beg paired with end:\n", .{});
    // printPaired(usize, usize, beg, end);
    // std.debug.print("\n", .{});

    std.sort.pdqContext(0, n, SortBoth{ .beg = beg, .end = end });

    // std.debug.print("after sorting\n", .{});
    // printPaired(usize, usize, beg, end);
    // std.debug.print("\n", .{});

    // Find the last segment jm of middle region.
    const jm = blk: {
        var jm: usize = 0;
        for (1..n) |j| {
            if (end[j] >= end[jm]) jm = j;
        }
        break :blk jm;
    };

    // std.debug.print("jm = {}\n", .{jm});

    // Extract all elements in the top and bottom regions, and the extremities
    // of the middle region, into s.
    const s: []usize = blk: {
        // Extract segments not in internal part of middle region into s.
        var e: usize = end[0];
        var l: usize = 0;
        // s contains the start and end of the middle region, and all elements not in the middle region.
        for (0..n) |j| {
            if (end[j] >= e and j != 0 and j != jm) {
                e = end[j];
            } else {
                s_buffer[l] = j;
                l += 1;
            }
        }

        break :blk s_buffer[0..l];
    };

    // {
    //     std.debug.print("initial build of s\n", .{});
    //     std.debug.print("s* = ", .{});
    //     var first = true;
    //     for (s, 0..) |s_, i| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("[{}]({})(({}, {}))", .{ i, s_, beg[s_], end[s_] });
    //     }
    //     std.debug.print("\n", .{});
    // }

    // Relabel the elements of B so that, for 0 <= j < n,
    // (beg[j] == beg[j+1]) implies (end[j] >= end[j+1])
    // Because s contains indexes into B, we need to apply the relabelling to s.
    {
        // XXX: we are using `stack` to stay safe. It should be possible to
        // store indices in beg or end, if that saves space overall.
        for (0..n) |i| {
            stack[i] = i;
        }
        const SortStackIndices = struct {
            stack: []usize,
            beg: []const usize,
            end: []const usize,

            pub fn swap(self: @This(), a: usize, b: usize) void {
                std.mem.swap(usize, &self.stack[a], &self.stack[b]);
            }

            pub fn lessThan(self: @This(), a: usize, b: usize) bool {
                return switch (std.math.order(self.beg[self.stack[a]], self.beg[self.stack[b]])) {
                    .gt => false,
                    .lt => true,
                    .eq => self.end[self.stack[a]] >= self.end[self.stack[b]],
                };
            }
        };
        // TODO: This should not need a full sort; we only need to reverse intervals having the same value for beg.
        std.sort.pdqContext(0, n, SortStackIndices{ .stack = stack, .beg = beg, .end = end });

        // Sort {beg,end} per the ordering in stack.
        for (stack, 0..) |st, i| {
            beg_relabelled[i] = beg[st];
            end_relabelled[i] = end[st];
        }
        @memcpy(beg, beg_relabelled);
        @memcpy(end, end_relabelled);

        // Apply relabelling to s.
        for (s) |*s_| {
            s_.* = stack[s_.*];
        }
    }

    // std.debug.print("after relabelling B\n", .{});
    // std.debug.print("beg paired with end:\n", .{});
    // printPaired(usize, usize, beg, end);
    // std.debug.print("\n", .{});
    // {
    //     std.debug.print("s* = ", .{});
    //     var first = true;
    //     for (s, 0..) |s_, i| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("[{}]({})(({}, {}))", .{ i, s_, beg[s_], end[s_] });
    //     }
    //     std.debug.print("\n", .{});
    // }

    // {
    //     std.debug.print("s* = ", .{});
    //     var first = true;
    //     for (s, 0..) |s_, i| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("[{}]({})(({}, {}))", .{ i, s_, beg[s_], end[s_] });
    //     }
    //     std.debug.print("\n", .{});
    // }

    // Reorder s so that, for 0 <= p < s.len,
    // (beg[s[p]] == beg[s[p+1]]) implies (end[s[p]] >= end[s[p+1]])
    {
        const SortS = struct {
            s: []usize,
            beg: []const usize,
            end: []const usize,

            pub fn swap(self: @This(), a: usize, b: usize) void {
                std.mem.swap(usize, &self.s[a], &self.s[b]);
            }

            pub fn lessThan(self: @This(), a: usize, b: usize) bool {
                return switch (std.math.order(self.beg[self.s[a]], self.beg[self.s[b]])) {
                    .gt => false,
                    .lt => true,
                    .eq => self.end[self.s[a]] >= self.end[self.s[b]],
                };
            }
        };
        // TODO: see above. This should not require a full sorting algorithm
        // because we are only reversing intervals having the same value of
        // beg.
        std.sort.pdqContext(0, s.len, SortS{ .s = s, .beg = beg, .end = end });
    }

    // {
    //     std.debug.print("after sorting s\n", .{});
    //     std.debug.print("s* = ", .{});
    //     var first = true;
    //     for (s, 0..) |s_, i| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("[{}]({})(({}, {}))", .{ i, s_, beg[s_], end[s_] });
    //     }
    //     std.debug.print("\n", .{});
    // }

    // Partition s into two subsequences sub1[0..l1] and sub2[0..l2],
    // such that end[sub1[0]] >= ... >= end[sub1[l1]]
    // and end[sub2[0]] >= ... >= end[sub2[l2]]
    const sub1, const sub2 = blk: {
        var l1: usize = 0;
        var l2: usize = 0;
        {
            var first_1 = true;
            var first_2 = true;

            for (s) |s_i| {
                // std.debug.print("s_i = {}; first_1 = {}; first_2 = {}; l1 = {}; l2 = {}\n", .{ s_i, first_1, first_2, l1, l2 });
                if (first_1 or end[s_i] <= end[sub1_buffer[l1 - 1]]) {
                    first_1 = false;
                    sub1_buffer[l1] = s_i;
                    l1 += 1;
                } else if (first_2 or end[s_i] <= end[sub2_buffer[l2 - 1]]) {
                    first_2 = false;
                    sub2_buffer[l2] = s_i;
                    l2 += 1;
                } else break;
            }
        }
        break :blk .{ sub1_buffer[0..l1], sub2_buffer[0..l2] };
    };

    // {
    //     std.debug.print("sub1 = {{ ", .{});
    //     var first = true;
    //     for (sub1) |sub1_| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("{}({}, {})", .{ sub1_, beg[sub1_], end[sub1_] });
    //     }
    //     std.debug.print(" }}\n", .{});
    // }
    // {
    //     std.debug.print("sub2 = {{ ", .{});
    //     var first = true;
    //     for (sub2) |sub2_| {
    //         if (first) {
    //             first = false;
    //         } else {
    //             std.debug.print(", ", .{});
    //         }
    //
    //         std.debug.print("{}({}, {})", .{ sub2_, beg[sub2_], end[sub2_] });
    //     }
    //     std.debug.print(" }}\n", .{});
    // }
    // {
    //     printPaired(usize, usize, beg, end);
    //     std.debug.print("\n", .{});
    // }

    {
        // Bottom region counter.
        var k1: usize = 0;
        // Top region counter.
        var k2: usize = 0;
        // Middle region counter.
        var k3: usize = 0;
        for (0..n) |j| {
            // Determine y[j]
            // std.debug.print("{} belongs to ", .{j});
            if (k1 < sub1.len and sub1[k1] == j) {
                // j belongs to the bottom region.
                // std.debug.print("bottom\n", .{});
                y[j] = sub1.len - k1 - 1;
                k1 += 1;
            } else if (k2 < sub2.len and sub2[k2] == j) {
                // j belongs to the top region.
                // std.debug.print("top\n", .{});
                y[j] = (n - sub2.len) + k2;
                k2 += 1;
            } else {
                // j belongs to the middle region.
                // std.debug.print("middle\n", .{});
                y[j] = sub1.len + k3;
                k3 += 1;
            }
            // std.debug.print("y[{}] is now {}\n", .{ j, y[j] });
        }
    }
}

test testDoubleConvexity {
    // Example from the paper using measurements from figure 2(b) (which is
    // subtly different to 2(a) and 2(c)...)
    const n = 14;
    var beg = [n]usize{ 2, 1, 2, 2, 0, 3, 2, 1, 0, 2, 3, 4, 3, 4 };
    var end = [n]usize{ 10, 10, 7, 11, 10, 8, 8, 7, 9, 11, 6, 7, 6, 5 };
    var y: [n]usize = undefined;
    {
        var s_buffer: [n]usize = undefined;
        var stack: [n]usize = undefined;
        var beg_relabelled: [n]usize = undefined;
        var end_relabelled: [n]usize = undefined;
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &beg_relabelled, &end_relabelled, &sub1, &sub2, &y);
    }
    const expected_y = [n]usize{ 6, 5, 7, 4, 10, 8, 9, 11, 3, 12, 2, 1, 13, 0 };
    try std.testing.expectEqualDeep(&expected_y, &y);
}

test "testDoubleConvexity small complete" {
    // 3 in A, 3 in B, complete graph.
    const n = 3;
    var beg = [n]usize{ 0, 0, 0 };
    var end = [n]usize{ 2, 2, 2 };
    var y: [n]usize = undefined;
    {
        var s_buffer: [n]usize = undefined;
        var stack: [n]usize = undefined;
        var beg_relabelled: [n]usize = undefined;
        var end_relabelled: [n]usize = undefined;
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &beg_relabelled, &end_relabelled, &sub1, &sub2, &y);
    }
    const expected_y = [n]usize{ 1, 2, 0 };
    try std.testing.expectEqualDeep(&expected_y, &y);
}

test "testDoubleConvexity small not doubly-convex" {
    const n = 4;
    var beg = [n]usize{ 0, 0, 1, 2 };
    var end = [n]usize{ 0, 2, 1, 3 };
    var y: [n]usize = undefined;
    {
        var s_buffer: [n]usize = undefined;
        var stack: [n]usize = undefined;
        var beg_relabelled: [n]usize = undefined;
        var end_relabelled: [n]usize = undefined;
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &beg_relabelled, &end_relabelled, &sub1, &sub2, &y);
    }
    // TODO: This should have failed. This is not a valid ordering.
    const expected_y = [n]usize{ 1, 0, 3, 2 };
    try std.testing.expectEqualDeep(&expected_y, &y);
}

fn partitionSequenceIntoTwoNonIncreasingSubsequences(s: []usize, sub1: []usize, l1: *usize, sub2: []usize, l2: *usize) void {
    l1.* = 0;
    l2.* = 0;
    sub1[0] = std.math.maxInt(usize);
    sub2[0] = std.math.maxInt(usize);

    for (s) |s_i| {
        if (s_i <= sub1[l1.*]) {
            l1.* += 1;
            sub1[l1.*] = s_i;
        } else if (s_i <= sub2[l2.*]) {
            l2.* += 1;
            sub2[l2.*] = s_i;
        } else break;
    }
}

fn printPaired(comptime T: type, comptime U: type, a: []T, b: []U) void {
    var first = true;
    std.debug.print("{{ ", .{});
    for (a, b, 0..) |a_, b_, i| {
        if (first) {
            first = false;
        } else {
            std.debug.print(", ", .{});
        }
        std.debug.print("[{}]({}, {})", .{ i, a_, b_ });
    }
    std.debug.print(" }}", .{});
}

fn isBegIncreasingEndNonincreasing(beg: []const usize, end: []const usize) bool {
    const n = beg.len;
    std.debug.assert(end.len == n);
    for (0..n - 1) |j| {
        if (beg[j] < beg[j + 1] or (beg[j] == beg[j + 1] and end[j] >= end[j + 1])) {
            continue;
        } else {
            return false;
        }
    } else return true;
}

fn isNonDecreasing(xs: []const usize) bool {
    for (0..xs.len - 1) |i| {
        if (xs[i] <= xs[i + 1]) {
            continue;
        } else {
            return false;
        }
    } else return true;
}

/// TODO: do not use heap allocation. We know how big the queue has to be.
pub fn findMaximumMatchingDoublyConvexBipartite(gpa: std.mem.Allocator, beg: []const usize, end: []const usize, y: []const usize, match: []usize) !void {
    const n = beg.len;
    std.debug.assert(end.len == n);
    std.debug.assert(y.len == n);
    std.debug.assert(isBegIncreasingEndNonincreasing(beg, end));
    const m = match.len;

    var deq = std.Deque(usize).empty;
    defer deq.deinit(gpa);

    var j: usize = 0;
    for (0..m) |i| {
        // Find element in B to be matched to i in A.
        while (j < n and beg[j] == i) {
            // Insert j into deq.
            if (deq.front()) |top| {
                if (y[j] > y[top]) {
                    try deq.pushFront(gpa, j);
                } else {
                    try deq.pushBack(gpa, j);
                }
            } else {
                try deq.pushBack(gpa, j);
            }
            j += 1;
        }
        if (deq.front()) |top| {
            // There was a front so there must be a back.
            const bottom = deq.back().?;
            if (end[top] < end[bottom]) {
                match[i] = top;
                _ = deq.popFront();
            } else {
                match[i] = bottom;
                _ = deq.popBack();
            }
        } else {
            // i unmatched.
            match[i] = std.math.maxInt(usize);
        }
        while (deq.front()) |top| {
            if (end[top] == i) {
                _ = deq.popFront();
            } else {
                break;
            }
        }
        while (deq.back()) |bottom| {
            if (end[bottom] == i) {
                _ = deq.popBack();
            } else {
                break;
            }
        }
    }
}

test findMaximumMatchingDoublyConvexBipartite {
    const gpa = std.testing.allocator;

    // Example from the paper using measurements from figure 2(b) (which is
    // subtly different to 2(a) and 2(c)...)
    const n = 14;
    var beg = [n]usize{ 2, 1, 2, 2, 0, 3, 2, 1, 0, 2, 3, 4, 3, 4 };
    var end = [n]usize{ 10, 10, 7, 11, 10, 8, 8, 7, 9, 11, 6, 7, 6, 5 };
    var y: [n]usize = undefined;
    {
        var s_buffer: [n]usize = undefined;
        var stack: [n]usize = undefined;
        var beg_relabelled: [n]usize = undefined;
        var end_relabelled: [n]usize = undefined;
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &beg_relabelled, &end_relabelled, &sub1, &sub2, &y);
    }

    var match: [n]usize = undefined;

    try findMaximumMatchingDoublyConvexBipartite(gpa, &beg, &end, &y, &match);
    const expected_match = [_]usize{ 1, 3, 8, 11, 13, 10, 12, 9, 7, 6, 5, 4, std.math.maxInt(usize), std.math.maxInt(usize) };
    try std.testing.expectEqualDeep(&expected_match, &match);
}

// fn invertDoublyConvex(gpa: std.mem.Allocator, beg_b: []const usize, end_b: []const usize, beg_a: []usize, end_a: []usize) !bool {
//     const n = beg_b.len;
//     std.debug.assert(end_b.len == n);
//
//     const m = beg_a.len;
//     std.debug.assert(end_a.len == m);
//
//     const Edge = struct { usize, usize };
//     var edges = std.ArrayList(Edge).empty;
//     defer edges.deinit(gpa);
//
//     for (0..n) |b| {
//         for (beg_b[b]..end_b[b]) |a| {
//             try edges.append(gpa, .{ a, b });
//         }
//     }
//
//     const SortA = struct {
//         pub fn lessThan(_: void, lhs: Edge, rhs: Edge) bool {
//             return if (lhs[0] < rhs[0])
//                 true
//             else
//                 lhs[1] < rhs[1];
//         }
//     };
//
//     std.sort.pdq(Edge, edges.items, {}, SortA.lessThan);
//
//     {
//         const State = struct { a: usize, b_beg: usize, b_end: usize };
//         var state: ?State = null;
//         for (edges.items) |edge| {
//             if (state) |*state_| {
//                 if (edge[0] == state_.a) {
//                     if (edge[1] == state_.b_end + 1) {
//                         state_.b_end += 1;
//                     } else {
//                         return false;
//                     }
//                 } else {
//                     beg_a[state_.a] = state_.b_beg;
//                     end_a[state_.a] = state_.b_end;
//                 }
//             } else {
//                 std.debug.assert(edge[0] < m);
//                 state = .{ .a = edge[0], .b_beg = edge[1], .b_end = edge[1] };
//             }
//         }
//         if (state) |state_| {
//             beg_a[state_.a] = state_.b_beg;
//             end_a[state_.a] = state_.b_end;
//         }
//     }
//     return true;
// }
//
// test invertDoublyConvex {
//     const gpa = std.testing.allocator;
//     const n = 14;
//     const beg_b = [n]usize{ 2, 1, 2, 2, 0, 3, 2, 1, 0, 2, 3, 4, 3, 4 };
//     const end_b = [n]usize{ 10, 10, 7, 11, 10, 8, 8, 7, 9, 11, 6, 7, 6, 5 };
//     var beg_a: [n]usize = undefined;
//     var end_a: [n]usize = undefined;
//
//     try std.testing.expectEqual(true, invertDoublyConvex(gpa, &beg_b, &end_b, &beg_a, &end_a));
//
//     printPaired(usize, usize, &beg_a, &end_a);
//     std.debug.print("\n", .{});
//
//     // const expected_beg_a = [n]usize{};
//     // const expected_beg_b = [n]usize{};
//     //
//     // std.testing.expectEqualDeep(
// }

test "findMaximumMatchingDoublyConvexBipartite extensionally equal to maximumMatchingGloverSimple for doubly convex bipartite graphs" {
    const IntervalIteratorList = iteratorList(IntervalIterator, Interval);
    const max_m = 5;
    const max_n = 5;
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

// Algorithm 4 in the paper
// 1. Let A and B be the vertices in the bipartite graph, E be the edges, and M⊂E be a maximum matching.
// 2. Direct every edge e∈M from A to B, and any e∈(E-M) from B to A.
// 3. Let B_0 denote the set of unmatched vertices in B - that is, the vertices touching no edge in M.
// 4. Find the sets A_1⊆A and B_1 (B_0⊆B_1⊆B) of vertices reachable from B_0 following the directed edges.
// 5. Construct the maximum independent set as I = B_1∪(A-A_1).
//
// Thus we need to find all vertices in G reachable from B_0. When G is convex, this can be obtained in O(m+n) time.
//
// Because G i convex, edges are represented by intervals: the vertices in A connected to some b∈B are represented by A[beg[b]..end[b]].
// The maximum matching has chosen one edge from this interval for each, so for all a∈A, match[a]∈B is the element matched with that element in the maximum matching.
//
// To do this:
// 1. We assume the elements of B are ordered so that beg[i]≤beg[i+1].
// 2. We iterate over elements b∈B_0 in ascending order, building up an "extended
//    interval" A*(b) of elements of A reachable from b. We call this an "extended interval"
//    because we start with interval A(b) of elements reachable by following one
//    edge from b, and as we follow edges from that set back to B and then back to
//    A, we necessarily find overlaps with that interval and we extend it
//    either direction to include the expanded interval. This is because G is
//    convex.
// 3. For a given b∈B, we alternate between decreasing and increasing traversal over A*(b).
// 3a. First, set A*(b) to A(b). Start at the maximum element a.
// 3b. For this element, if there is an element match[a]∈B, then expand A*(b) to include A(match[a]).
// 3c. Repeat 3b with the next lowest element in A*(b), if there was one.
// 3d. Once we have expanded with the lowest element in A*(b), begin from the element above where we were.
// 3e. Repeat 3b and 3c except incrementing the element of A*(b) we are operating on.
// 3f. Once we hit the top, if there are new elements at the bottom of A*(b), operate on those; then if there are new elements at the top, operate on those; and so on.
// 3g. Once we have stopped adding elements to A*(b), push the endpoints of A*(b) onto a stack. This stack should be checked during downward traversal to ensure we do not double-process elements of A. If the interval does meet another interval, the two intervals can be combined.
//
// TODO: the stack return pointer thing is silly, and so is allocating a queue.
//
// stack becomes a sequence of [i_1, e_1, i_2, e_2, ...] of start and end of
// intervals in A representing the set of vertices reachable from B_0 (i.e.
// A_1). B_1 can be found by following the matched edges in A_1.
pub fn findMaximumIndependentSet(gpa: std.mem.Allocator, beg: []const usize, end: []const usize, match: []const usize, stack: *std.Deque(usize)) !void {
    const n = beg.len;
    std.debug.assert(end.len == n);
    std.debug.assert(isNonDecreasing(beg));
    // const m = match.len;

    // TODO find a fast way to construct this.
    // @breakpoint();
    var queue = std.Deque(usize).empty;
    defer queue.deinit(gpa);
    for (0..n) |j| {
        if (std.mem.indexOfScalar(usize, match, j)) |_| {} else {
            try queue.pushBack(gpa, j);
        }
    }

    // std.debug.print("queue {any}", .{queue.buffer});
    stack.* = .empty;

    // @breakpoint();
    while (queue.popFront()) |j| {
        // Find vertices reachable from first(queue).
        if (if (stack.back()) |t| end[j] > t else true) {
            // New vertices to be scanned.
            // l and u are pointers used in scanning; they go downward and upward respectively.
            var l = end[j] + 1;
            var u = end[j];
            // lower and upper denote the current boundaires of the extended interval being constructed. They start out as the interval of edges adjacent to j.
            var lower = beg[j];
            var upper = end[j];
            // std.debug.print("beginning of new verts: [l,u]=[{},{}] [lower,upper]=[{},{}]\n", .{ l, u, lower, upper });

            // Extend interval of vertices reached from j.
            while (true) : (if (l == lower and u == upper)
                // l and u, the scanning pointers, have reached the endpoints
                // of the extended interval, so we have finished extending it.
                break)
            {
                // Scan downward.
                while (l > lower) {
                    l -= 1;
                    if (match[l] != std.math.maxInt(usize)) {
                        // l is matched.
                        // std.debug.print("scan downward: match[l]={} lower={} beg[match[l]]={} upper={} end[match[l]]={}\n", .{ match[l], lower, beg[match[l]], upper, end[match[l]] });
                        lower = @min(lower, beg[match[l]]);
                        upper = @max(upper, end[match[l]]);
                    }
                    if (if (stack.back()) |t| l < t + 1 else false) {
                        // The l pointer has entered the range of the previous extended interval. We pop that interval, then act like l and lower are at its lower end.
                        // (TODO: it is weird and inefficient popping twice. We should use an interval struct and just pop one of them.)
                        l = stack.popBack().?;
                        l = stack.popBack().?;
                        lower = @min(lower, l);
                    }
                }

                // Scan upward.
                while (u < upper) {
                    u += 1;
                    if (match[u] != std.math.maxInt(usize)) {
                        // u is matched.
                        lower = @min(lower, beg[match[u]]);
                        upper = @max(upper, end[match[u]]);
                    }
                }
                // std.debug.print("j={} l={} lower={} u={} upper={}\n", .{ j, l, lower, u, upper });
            }
            try stack.pushBack(gpa, lower);
            try stack.pushBack(gpa, upper);
        }
    }
}

test findMaximumIndependentSet {
    const gpa = std.testing.allocator;

    // Example from the paper using measurements from figure 2(b) (which is
    // subtly different to 2(a) and 2(c)...)
    const m = 3;
    const n = 2;
    var beg = [n]usize{ 0, 2 };
    var end = [n]usize{ 1, 2 };
    var y: [n]usize = undefined;
    {
        var s_buffer: [n]usize = undefined;
        var stack: [n]usize = undefined;
        var beg_relabelled: [n]usize = undefined;
        var end_relabelled: [n]usize = undefined;
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &beg_relabelled, &end_relabelled, &sub1, &sub2, &y);
    }

    var match: [m]usize = undefined;

    try findMaximumMatchingDoublyConvexBipartite(gpa, &beg, &end, &y, &match);

    std.debug.print("match: {any}\n", .{match});

    var stack: std.Deque(usize) = undefined;
    defer stack.deinit(gpa);
    try findMaximumIndependentSet(gpa, &beg, &end, &match, &stack);

    {
        std.debug.print("stack:\n", .{});
        var it = stack.iterator();
        while (it.next()) |s| {
            std.debug.print("{}\n", .{s});
        }
    }
}

test "findMaximumIndependentSet 2" {
    const gpa = std.testing.allocator;

    // Example from the paper using measurements from figure 2(b) (which is
    // subtly different to 2(a) and 2(c)...)
    const m = 3;
    const n = 2;
    var beg = [n]usize{ 0, 2 };
    var end = [n]usize{ 1, 2 };
    var match = [m]usize{ std.math.maxInt(usize), std.math.maxInt(usize), std.math.maxInt(usize) };

    var stack: std.Deque(usize) = undefined;
    defer stack.deinit(gpa);
    try findMaximumIndependentSet(gpa, &beg, &end, &match, &stack);

    {
        var stackIt = stack.iterator();
        for ([_]usize{ 0, 1, 2, 2 }) |expected| {
            if (stackIt.next()) |s| {
                try std.testing.expectEqual(expected, s);
            } else {
                try std.testing.expect(false);
            }
        }
    }
}
