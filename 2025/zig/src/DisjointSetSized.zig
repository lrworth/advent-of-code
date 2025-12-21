const std = @import("std");

const Node = struct { parent: usize, size: usize };

nodes: []Node,

pub fn init(gpa: std.mem.Allocator, size: usize) !@This() {
    const nodes = try gpa.alloc(Node, size);
    for (nodes, 0..) |*node, i| {
        node.parent = i;
        node.size = 1;
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
    var u_set = self.findSet(u);
    var v_set = self.findSet(v);

    if (u_set == v_set) return;

    if (self.nodes[u_set].size < self.nodes[v_set].size) {
        std.mem.swap(usize, &u_set, &v_set);
    }
    self.nodes[v_set].parent = u_set;
    self.nodes[u_set].size += self.nodes[v_set].size;
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

    try std.testing.expectEqual(4, ds.nodes[ds.findSet(0)].size);
    try std.testing.expectEqual(2, ds.nodes[ds.findSet(4)].size);
    try std.testing.expectEqual(4, ds.nodes[ds.findSet(6)].size);
}

pub fn print(self: @This()) void {
    for (0..self.nodes.len) |i| {
        std.debug.print("Node {}:\t{}\n", .{ i, self.findSet(i) });
    }
}
