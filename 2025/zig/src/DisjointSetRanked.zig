const std = @import("std");

const Node = struct { parent: usize, rank: u64 };

nodes: []Node,

pub fn init(gpa: std.mem.Allocator, size: usize) !@This() {
    const nodes = try gpa.alloc(Node, size);
    for (nodes, 0..) |*node, i| {
        node.parent = i;
        node.rank = 0;
    }
    return .{ .nodes = nodes };
}

pub fn deinit(self: @This(), gpa: std.mem.Allocator) void {
    gpa.free(self.nodes);
}

pub fn findSet(self: @This(), u: usize) usize {
    var x = u;
    while (self.nodes[x].parent != x) {
        const newU = self.nodes[x].parent;
        self.nodes[x].parent = self.nodes[self.nodes[x].parent].parent;
        x = newU;
    }
    return x;
}

pub fn merge(self: @This(), u: usize, v: usize) void {
    const u_set = self.findSet(u);
    const v_set = self.findSet(v);
    if (self.nodes[u_set].rank == self.nodes[v_set].rank) {
        self.nodes[u_set].rank += 1;
        self.nodes[v_set].parent = u_set;
    } else if (self.nodes[u_set].rank > self.nodes[v_set].rank) {
        self.nodes[v_set].parent = u_set;
    } else {
        self.nodes[u_set].parent = v_set;
    }
}

test {
    const gpa = std.testing.allocator;
    const ds = try init(gpa, 10);
    defer ds.deinit(gpa);
    ds.merge(0, 1);
    ds.merge(2, 1);
    ds.merge(3, 0);
    ds.merge(3, 2);
    ds.merge(4, 5);
    ds.merge(6, 8);
    ds.merge(7, 9);
    ds.merge(8, 9);
    try std.testing.expectEqual(0, ds.findSet(0));
    try std.testing.expectEqual(0, ds.findSet(1));
    try std.testing.expectEqual(0, ds.findSet(2));
    try std.testing.expectEqual(0, ds.findSet(3));
    try std.testing.expectEqual(4, ds.findSet(4));
    try std.testing.expectEqual(4, ds.findSet(5));
    try std.testing.expectEqual(6, ds.findSet(6));
    try std.testing.expectEqual(6, ds.findSet(7));
    try std.testing.expectEqual(6, ds.findSet(8));
    try std.testing.expectEqual(6, ds.findSet(9));
}
