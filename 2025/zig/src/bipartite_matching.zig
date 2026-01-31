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
/// Size of `beg`, `end`, `s_buffer`, `stack`, `sub1`, `sub2`, and `y` are n.
/// `beg` and `end` are reordered in-place. `s_buffer`, `stack`, `sub1`, and `sub2` are working buffers which do not need to be initialised.
/// `y` is the output. It does not need to be initialised.
pub fn testDoubleConvexity(beg: []usize, end: []usize, s_buffer: []usize, stack: []usize, sub1_buffer: []usize, sub2_buffer: []usize, y: []usize) void {
    const n: usize = beg.len;
    std.debug.assert(end.len == n);
    std.debug.assert(s_buffer.len == n);
    std.debug.assert(stack.len == n);
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
    // printPaired(usize, usize, beg, end);
    // std.debug.print("\n", .{});

    std.sort.pdqContext(0, n, SortBoth{ .beg = beg, .end = end });

    // std.debug.print("after sorting\n", .{});
    // printPaired(usize, usize, beg, end);
    // std.debug.print("\n", .{});

    // Find last segment jm of middle region.
    var jm: usize = 0;
    for (1..n) |j| {
        if (end[j] >= end[jm]) jm = j;
    }

    // std.debug.print("jm = {}\n", .{jm});

    // Extract the elements in the top and bottom regions, and the extremities
    // of the middle region, into s.
    //
    // l becomes the number of elements in the top and bottom regions,
    // including the two ends of the middle region.
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

    // Reorder the elements belonging to the top and bottom regions so that,
    // for 0 <= j < s.len - 1, (beg[s[j]] = beg[s[j+1]]) implies (end[j] >= end[j+1]).
    // The paper merely says "this can obviously be done in linear time by
    // straightforward use of a stack". This needs to be done by changing the
    // order of elements in beg/end, but only relative to s: s will still
    // reference the same set but the order they appear in will change.
    // The "obvious" method for doing this is to push elements onto a stack
    // while beg[s[j]] remains the same, then when it changes we iterate over
    // the same elements of s in order and pop values onto them.
    {
        var top: usize = 0;
        var curr_beg = beg[s[0]];
        var s_start: usize = 0;
        // std.debug.print("reordering\n", .{});
        for (0..s.len) |j| {
            // std.debug.print("{} (top = {}, curr_beg = {}, s_start = {}):\n", .{ j, top, curr_beg, s_start });
            if (beg[s[j]] != curr_beg) {
                // std.debug.print("dumping stack {any}\n", .{stack[0..top]});
                for (s_start..j) |k| {
                    top -= 1;
                    end[s[k]] = stack[top];
                }
                curr_beg = beg[s[j]];
                s_start = j;
            }
            stack[top] = end[s[j]];
            top += 1;
        }
        // std.debug.print("dumping stack {any}\n", .{stack[0..top]});
        for (s_start..s.len) |k| {
            top -= 1;
            end[s[k]] = stack[top];
        }
    }

    // {
    //     std.debug.print("after sorting subsequences\n", .{});
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
        var k1: usize = 0;
        var k2: usize = 0;
        var k3: usize = 0;
        for (0..n) |j| {
            // Determine y[j]
            // std.debug.print("{} belongs to ", .{j});
            if (sub1[k1] == j) {
                // j belongs to the bottom region.
                // std.debug.print("bottom\n", .{});
                y[sub1.len - k1 - 1] = j;
                k1 += 1;
            } else if (sub2[k2] == j) {
                // j belongs to the top region.
                // std.debug.print("top\n", .{});
                y[(n - sub2.len) + k2] = j;
                k2 += 1;
            } else {
                // j belongs to the middle region.
                // std.debug.print("middle\n", .{});
                y[sub1.len + k3] = j;
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
        var sub1: [n]usize = undefined;
        var sub2: [n]usize = undefined;
        testDoubleConvexity(&beg, &end, &s_buffer, &stack, &sub1, &sub2, &y);
    }
    const expected_beg = [n]usize{ 4, 3, 3, 2, 1, 0, 0, 1, 2, 2, 2, 2, 3, 4 };
    const expected_end = [n]usize{ 5, 6, 6, 7, 7, 9, 10, 10, 10, 11, 11, 8, 8, 7 };
    for (0..n) |i| {
        try std.testing.expectEqual(expected_beg[i], beg[y[i]]);
        try std.testing.expectEqual(expected_end[i], end[y[i]]);
    }
    // std.debug.print("{any}\n", .{y});
    // for (y) |y_| {
    //     std.debug.print("{}: beg = {}, end = {}\n", .{ y_, beg[y_], end[y_] });
    // }
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
