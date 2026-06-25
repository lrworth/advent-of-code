//! AVL tree as described in The Art Of Computer Programming, Volume 3, 6.2.3
const std = @import("std");

pub fn AvlTree(comptime Key: type) type {
    return struct {
        /// Node details, intended to be intrusively embedded. Access data with @fieldParentPtr.
        const Node = struct {
            const Self = @This();
            const LLinkHack = union { node: ?*Self, height: usize };
            key: Key,
            l_link: LLinkHack,
            r_link: ?*Self,
            b: i2,
        };

        fn debugPrint(gpa: std.mem.Allocator, head: *Node) error{OutOfMemory}!void {
            std.debug.print("height: {d}\n", .{head.l_link.height});
            try traverseInOrder(error{}, gpa, head, {}, struct {
                fn f(_: void, key: Key) error{}!void {
                    std.debug.print("key: {d}\n", .{key});
                }
            }.f);
        }

        fn traverseInOrder(comptime Errors: type, gpa: std.mem.Allocator, head: *Node, context: anytype, comptime f: *const fn (context: @TypeOf(context), key: Key) Errors!void) (error{OutOfMemory} || Errors)!void {
            var stack = try std.ArrayList(*Node).initCapacity(gpa, head.l_link.height);
            defer stack.deinit(gpa);

            var current: ?*Node = head.r_link;

            while (true) {
                while (current) |current_exists| {
                    stack.appendAssumeCapacity(current_exists);
                    current = current_exists.l_link.node;
                }
                if (stack.pop()) |popped_node| {
                    try f(context, popped_node.key);
                    current = popped_node.r_link;
                } else {
                    break;
                }
            }
        }

        fn insert(head: *Node, k: Key, new: *Node) void {
            // A1. [Initialize.]

            // p will move down the tree.
            var p = head.r_link;
            // s will point to the place where rebalancing may be necessary.
            var s = p;
            // t always points to the parent of s
            var t = head;
            // q becomes the found node, if one was inserted.
            var q: ?*Node = undefined;

            // A2. [Compare.]
            while (true) {
                if (k < p.?.key) {
                    // A3. [Move left.]
                    q = p.?.l_link.node;
                    if (q) |q_| {
                        if (q_.b != 0) {
                            t = p.?;
                            s = q_;
                        }
                        p = q_;
                    } else {
                        q = new;
                        p.?.l_link.node = q;
                        break;
                    }
                } else if (k > p.?.key) {
                    // A4. [Move right.]
                    q = p.?.r_link;
                    if (q) |q_| {
                        if (q_.b != 0) {
                            t = p.?;
                            s = q_;
                        }
                        p = q_;
                    } else {
                        q = new;
                        p.?.r_link = q;
                        break;
                    }
                } else {
                    return;
                }
            }

            // A5. [Insert.]
            // We have just linked q into the tree and its fields need to be initialized.
            q.?.key = k;
            q.?.l_link.node = null;
            q.?.r_link = null;
            q.?.b = 0;

            // A6. [Adjust balance factors.]
            // Now the balance factors on nodes between s and q need to be changed from zero to ±1.
            const a: i2 = if (k < s.?.key) -1 else 1;
            p = if (a == -1) s.?.l_link.node else s.?.r_link;
            var r = p;
            while (p != q) {
                if (k < p.?.key) {
                    p.?.b = -1;
                    p = p.?.l_link.node;
                } else {
                    // k > p.key, otherwise we'd have broken the loop because p = q.
                    p.?.b = 1;
                    p = p.?.r_link;
                }
            }

            // A7. [Balancing act.]
            if (s.?.b == 0) {
                // The tree has grown higher.
                s.?.b = a;
                head.l_link.height += 1;
                return;
            } else if (s.?.b == -a) {
                // The tree has gotten more balanced.
                s.?.b = 0;
                return;
            } else {
                // s.b == a
                // The tree has gotten out of balance.
                if (r.?.b == a) {
                    // A8. [Single rotation.]
                    p = r;
                    if (a == -1) {
                        s.?.l_link.node = r.?.r_link;
                        r.?.r_link = s;
                    } else {
                        s.?.r_link = r.?.l_link.node;
                        r.?.l_link.node = s;
                    }
                    r.?.b = 0;
                    s.?.b = 0;
                } else {
                    // r.b == -a
                    // A9. [Double rotation.]
                    if (a == -1) {
                        p = r.?.r_link;
                        r.?.r_link = p.?.l_link.node;
                        p.?.l_link.node = r;
                        s.?.l_link.node = p.?.r_link;
                        p.?.r_link = s;
                    } else {
                        p = r.?.l_link.node;
                        r.?.l_link.node = p.?.r_link;
                        p.?.r_link = r;
                        s.?.r_link = p.?.l_link.node;
                        p.?.l_link.node = s;
                    }
                    s.?.b, r.?.b = if (p.?.b == a) .{ -a, 0 } else if (p.?.b == 0) .{ 0, 0 } else
                        // p.b == -a
                        .{ 0, a };
                    p.?.b = 0;
                }
            }

            // A10. [Finishing touch.]
            if (s == t.r_link) {
                t.r_link = p;
            } else {
                t.l_link.node = p;
            }
        }
    };
}

fn expectNonNegativeIntegers(comptime Key: type, gpa: std.mem.Allocator, head: *AvlTree(Key).Node) error{ OutOfMemory, TestExpectedEqual }!void {
    const Traversal = struct { expected: Key };
    var traversal = Traversal{ .expected = 0 };
    try AvlTree(Key).traverseInOrder(error{TestExpectedEqual}, gpa, head, &traversal, struct {
        fn f(ctx: *Traversal, key: usize) error{TestExpectedEqual}!void {
            try std.testing.expectEqual(ctx.expected, key);
            ctx.expected += 1;
        }
    }.f);
}

test "insert" {
    const gpa = std.testing.allocator;

    const T = AvlTree(usize);
    var buffer: [18]T.Node = undefined;
    buffer[0] = .{ .key = 0, .l_link = T.Node.LLinkHack{ .node = null }, .r_link = null, .b = 0 };
    var head = T.Node{ .l_link = T.Node.LLinkHack{ .height = 1 }, .r_link = &buffer[0], .key = undefined, .b = undefined };
    for (1..18) |i| {
        T.insert(
            &head,
            i,
            &buffer[i],
        );
    }
    try expectNonNegativeIntegers(usize, gpa, &head);
}

test "insert in reverse" {
    const gpa = std.testing.allocator;

    const T = AvlTree(usize);
    var buffer: [18]T.Node = undefined;
    buffer[0] = .{ .key = 17, .l_link = T.Node.LLinkHack{ .node = null }, .r_link = null, .b = 0 };
    var head = T.Node{ .l_link = T.Node.LLinkHack{ .height = 1 }, .r_link = &buffer[0], .key = undefined, .b = undefined };
    for (1..18) |i| {
        T.insert(
            &head,
            17 - i,
            &buffer[i],
        );
    }
    try expectNonNegativeIntegers(usize, gpa, &head);
}

test "insert evens then odds" {
    const gpa = std.testing.allocator;

    const T = AvlTree(usize);
    var buffer: [18]T.Node = undefined;
    buffer[0] = .{ .key = 0, .l_link = T.Node.LLinkHack{ .node = null }, .r_link = null, .b = 0 };
    var head = T.Node{ .l_link = T.Node.LLinkHack{ .height = 1 }, .r_link = &buffer[0], .key = undefined, .b = undefined };
    for (1..9) |i| {
        T.insert(
            &head,
            2 * i,
            &buffer[i],
        );
    }
    for (0..9) |i| {
        T.insert(
            &head,
            2 * i + 1,
            &buffer[9 + i],
        );
    }
    try expectNonNegativeIntegers(usize, gpa, &head);
}
