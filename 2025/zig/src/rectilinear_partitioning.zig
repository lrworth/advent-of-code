const std = @import("std");

const Edge = struct { usize, usize };

const BipartiteGraph = struct {
    h_len: usize,
    v_len: usize,
    e: []const Edge,
};

const Orientation = enum { h, v };

const BipartiteVertex = struct { orientation: Orientation, i: usize };

// A bipartite graph G = ( H∪V, E) is convex on V iff the vertices in V can be
// ordered such that for every h ∈ H, NEB(h) = [FIRST(h), LAST(h)] (i.e. all
// vertices in the ordering beginning at vertex FIRST(h) and going on to vertex
// LAST(h)) or null.
const BipartiteGraphConcaveV = struct {
    const Interval = struct { first: usize, last: usize };
    h_neb: []?Interval,
    v_neb: [][]?usize,

    fn deinit(self: *BipartiteGraphConcaveV, gpa: std.mem.Allocator) void {
        gpa.free(self.h_neb);
        for (self.v_neb) |v| {
            gpa.free(v);
        }
        gpa.free(self.v_neb);
    }

    fn vNebFromHNeb(gpa: std.mem.Allocator, h_neb: []const Interval) ![][]?usize {
        var max_v: usize = 0;
        for (h_neb) |interval| {
            max_v = @max(max_v, interval.last);
        }

        const v_neb = try gpa.alloc(std.ArrayList(?usize), max_v + 1);
        defer {
            for (v_neb) |*neb| {
                neb.deinit(gpa);
            }
            gpa.free(v_neb);
        }
        for (v_neb) |*v_item| {
            v_item.* = std.ArrayList(?usize).empty;
        }

        for (h_neb, 0..) |interval, h_ind| {
            for (interval.first..interval.last + 1) |v_ind| {
                try v_neb[v_ind].append(gpa, h_ind);
            }
        }

        const result = try gpa.alloc([]?usize, v_neb.len);
        for (result, v_neb) |*slice, *array| {
            slice.* = try array.toOwnedSlice(gpa);
        }

        return result;
    }

    test vNebFromHNeb {
        const gpa = std.testing.allocator;
        const h_neb = [_]BipartiteGraphConcaveV.Interval{ .{ .first = 0, .last = 2 }, .{ .first = 1, .last = 3 }, .{ .first = 1, .last = 2 }, .{ .first = 2, .last = 3 }, .{ .first = 2, .last = 2 }, .{ .first = 3, .last = 4 }, .{ .first = 3, .last = 3 } };
        const v_neb = try BipartiteGraphConcaveV.vNebFromHNeb(gpa, &h_neb);
        defer {
            for (v_neb) |g_| {
                gpa.free(g_);
            }
            gpa.free(v_neb);
        }
        const expected_v_neb: []const []const ?usize = &.{ &.{0}, &.{ 0, 1, 2 }, &.{ 0, 1, 2, 3, 4 }, &.{ 1, 3, 5, 6 }, &.{5} };
        try std.testing.expectEqualDeep(expected_v_neb, v_neb);
    }

    /// Each h_neb[h] establishes a set of edges from h to each vertical vertex
    /// in the interval [ h_neb[h].first, h_neb[h].last ].
    /// h_neb is placed into the return value but ownership retained by caller. Caller owns the return value.
    fn fromHNeb(gpa: std.mem.Allocator, h_neb: []const Interval) !BipartiteGraphConcaveV {
        const v_neb = try vNebFromHNeb(gpa, h_neb);
        const result_h_neb = try gpa.alloc(?Interval, h_neb.len);
        for (result_h_neb, h_neb) |*out, in| {
            out.* = in;
        }
        return .{ .h_neb = result_h_neb, .v_neb = v_neb };
    }

    test fromHNeb {
        const gpa = std.testing.allocator;
        const h_neb = [_]BipartiteGraphConcaveV.Interval{ .{ .first = 0, .last = 2 }, .{ .first = 1, .last = 3 }, .{ .first = 1, .last = 2 }, .{ .first = 2, .last = 3 }, .{ .first = 2, .last = 2 }, .{ .first = 3, .last = 4 }, .{ .first = 3, .last = 3 } };
        var g = try BipartiteGraphConcaveV.fromHNeb(gpa, &h_neb);
        defer g.deinit(gpa);
        var expected_g: BipartiteGraphConcaveV = .{ .h_neb = blk: {
            const data = [_]?Interval{ .{ .first = 0, .last = 2 }, .{ .first = 1, .last = 3 }, .{ .first = 1, .last = 2 }, .{ .first = 2, .last = 3 }, .{ .first = 2, .last = 2 }, .{ .first = 3, .last = 4 }, .{ .first = 3, .last = 3 } };
            break :blk try gpa.dupe(?Interval, &data);
        }, .v_neb = blk: {
            const data = [_][]const ?usize{ &.{0}, &.{ 0, 1, 2 }, &.{ 0, 1, 2, 3, 4 }, &.{ 1, 3, 5, 6 }, &.{5} };
            const result = try gpa.alloc([]?usize, data.len);
            for (result, data) |*out, in| {
                out.* = try gpa.dupe(?usize, in);
            }
            break :blk result;
        } };
        defer expected_g.deinit(gpa);
        try std.testing.expectEqualDeep(expected_g, g);
    }
};

test {
    _ = BipartiteGraphConcaveV;
}

// Procedure MaxMatch(G,M);
// {G = ( H∪V, E) is a bipartite graph that is convex on V. V = {v1, ..., vnV } is ordered as required by
// the convex property. A maximum matching M is computed.}
// begin
//   M := ∅;
//   for i := 1 to nV do
//     if NEB(vi) ≠ ∅ then
//     begin
//       Let hj ∈ H be such that LAST(hj) = min(h ∈ NEB (vi)) {LAST(h)}
//       M := M ∪ {(hj, vi)};
//       Delete hj, vi, and incident edges from G;
//     end
//   end;{of if & for}
// end;{of MaxMatch}

/// g is destroyed in the process.
pub fn maxMatch(gpa: std.mem.Allocator, g: *BipartiteGraphConcaveV) ![]Edge {
    var m = std.ArrayList(Edge).empty;
    defer m.deinit(gpa);

    for (0..g.v_neb.len) |i| {
        const neb_vi = g.v_neb[i];
        if (neb_vi.len > 0) {
            var last_hj: ?usize = null;
            for (neb_vi) |h| {
                if (h) |h_known| {
                    if (g.h_neb[h_known]) |neb_h_last| {
                        last_hj = if (last_hj) |last_hj_known|
                            @min(last_hj_known, neb_h_last.last)
                        else
                            neb_h_last.last;
                    }
                }
            }
            const j: usize = for (0..g.h_neb.len) |j| {
                if (g.h_neb[j]) |hj| {
                    if (hj.last == last_hj) break j;
                }
            } else return error.MalformedGraph;

            try m.append(gpa, .{ j, i });

            if (g.h_neb[j]) |*hj| {
                for (hj.first..hj.last) |v| {
                    for (g.v_neb[v]) |*v_neb| {
                        if (v_neb.* == j) v_neb.* = null;
                    }
                }
                g.h_neb[j] = null;
            }

            for (g.v_neb[i]) |*v_neb| {
                v_neb.* = null;
            }
        }
    }

    return m.toOwnedSlice(gpa);
}

test maxMatch {
    const gpa = std.testing.allocator;
    // Example from figure 7.
    var h_neb = [_]BipartiteGraphConcaveV.Interval{ .{ .first = 0, .last = 1 }, .{ .first = 1, .last = 3 }, .{ .first = 1, .last = 2 }, .{ .first = 2, .last = 3 }, .{ .first = 2, .last = 2 }, .{ .first = 3, .last = 4 }, .{ .first = 3, .last = 3 } };
    var g = try BipartiteGraphConcaveV.fromHNeb(gpa, &h_neb);
    defer g.deinit(gpa);

    const m = try maxMatch(gpa, &g);
    defer gpa.free(m);

    const expected_edges = [_]Edge{ .{ 0, 0 }, .{ 2, 1 }, .{ 4, 2 }, .{ 1, 3 }, .{ 5, 4 } };
    try std.testing.expectEqualDeep(&expected_edges, m);
}

/// m is a maximum matching of the bipartite graph. Returns a minimum
/// independent set of vertices. Caller owns the result.
// Procedure MaxInd(G,M,S);
// {Given a bipartite graph G = ( H∪V, E) and a maximum matching M ⊆ E. Find an MIS S such that
// S ⊆ H∪V}
// begin
//     S := ∅;
//     F := {u u ∈ H∪V and (u, x) / ∈ M for any x}; {Free vertices}
//     while (F ≠ ∅ ) or (M ≠ ∅) do
//     begin
//         if F ≠ ∅ then
//         begin {add a free vertex to S}
//             Let u ∈ F; F := F- {u}; S := S ∪ {u};
//         end
//         else {add a vertex in M to S}
//         begin
//             Let (u, v) ∈ M; M := M- {(u, v)};
//             E := E- {(u, v)}; S := S ∪ {u};
//         end;
//         { Process vertex u }
//         for all (u, v) ∈ E do
//         begin
//             E := E- {(u, v)};
//             if there is an h such that (v, h) ∈ M then
//             begin
//             M := M- {(v, h)}; F := F ∪ {h}; {h is free}
//             end;
//         end; {of for}
//     end;{of while}
// end;{of MaxInd}
pub fn maxInd(gpa: std.mem.Allocator, g: BipartiteGraph, m: []const Edge) []BipartiteVertex {
    // Result
    var s = try std.ArrayList(BipartiteVertex).initCapacity(gpa, g.h_len + g.v_len);
    defer s.deinit();

    // Free vertices
    var f = try std.ArrayList(BipartiteVertex).initCapacity(gpa, g.h_len + g.v_len);
    defer f.deinit();

    for (0..g.h_len) |i| {
        const has_edge = for (g.e) |edge| {
            if (edge[0] == i) break true;
        } else false;
        if (!has_edge) f.append(gpa, .{ .orientation = .h, .i = i });
    }

    for (0..g.v_len) |i| {
        const has_edge = for (g.e) |edge| {
            if (edge[1] == i) break true;
        } else false;
        if (!has_edge) f.append(gpa, .{ .orientation = .v, .i = i });
    }

    // while (f.items.len != 0 or m

    _ = m;

    return s.toOwnedSlice;
}
