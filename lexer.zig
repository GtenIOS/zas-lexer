const std = @import("std");
const byte = @import("common").byte;
const Imm = @import("common").imm.Imm;
pub const TokenKind = enum {
    id,
    num,
    eql,
    deq,
    com,
    sem,
    col,
    quo,
    dqo,
    pls,
    min,
    mul,
    div,
    mod,
    til,
    not,
    at,
    str,
    chr,
    dot,
    dol,
    hsh,
    lbrk,
    rbrk,
    nl,
    const Self = @This();
    pub fn strval(self: Self) []const u8 {
        return switch (self) {
            TokenKind.id => "ID",
            TokenKind.num => "NUMBER",
            TokenKind.eql => "EQUAL",
            TokenKind.deq => "DOUBLE EQUAL",
            TokenKind.com => "COMMA",
            TokenKind.col => "COLON",
            TokenKind.sem => "SEMI COLON",
            TokenKind.quo => "QUOTE",
            TokenKind.dqo => "DOUBLE QUOTE",
            TokenKind.pls => "PLUS",
            TokenKind.min => "MINUS",
            TokenKind.mul => "MULTIPLY",
            TokenKind.div => "DIVISON",
            TokenKind.mod => "MODULUS",
            TokenKind.til => "TILDE",
            TokenKind.not => "NOT",
            TokenKind.at => "AT",
            TokenKind.str => "STRING",
            TokenKind.chr => "CHAR",
            TokenKind.dot => "DOT",
            TokenKind.dol => "DOLLAR",
            TokenKind.hsh => "HASH",
            TokenKind.lbrk => "LEFT BRACKET",
            TokenKind.rbrk => "RIGHT BRACKET",
            TokenKind.nl => "NEW LINE",
        };
    }

    pub fn isColon(self: Self) bool {
        switch (self) {
            .col => return true,
            else => return false,
        }
    }

    pub fn isId(self: Self) bool {
        switch (self) {
            .id => return true,
            else => return false,
        }
    }

    pub fn isNl(self: Self) bool {
        switch (self) {
            .nl => return true,
            else => return false,
        }
    }

    pub fn isArithOperator(self: Self) bool {
        switch (self) {
            .pls => return true,
            .min => return true,
            .mul => return true,
            .div => return true,
            .mod => return true,
            else => return false,
        }
    }
};

pub const TokenVal = union(enum) {
    str: []const u8,
    num: NumberVal,
    ch: u8,
    const Self = @This();

    pub fn strVal(self: Self) ![]const u8 {
        switch (self) {
            .str => |str_val| return str_val,
            else => return error.NotAStringValue,
        }
    }

    pub inline fn numVal(self: Self) !NumberVal {
        switch (self) {
            .num => |nval| return nval,
            .ch => |cval| return NumberVal{ .int = @intCast(isize, cval) },
            else => return error.ValueNotCastableToNumber,
        }
    }

    pub inline fn bytesVal(self: Self, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        switch (self) {
            .str => |sval| {
                var bytes = std.ArrayList(u8).init(allocator);
                errdefer bytes.deinit();
                try bytes.appendSlice(sval);
                return bytes;
            },
            .num => |nval| return nval.bytesVal(allocator),
            .ch => |cval| {
                var bytes = std.ArrayList(u8).init(allocator);
                errdefer bytes.deinit();
                try bytes.append(cval);
                return bytes;
            },
        }
    }
};

pub const NumberVal = union(enum) {
    int: isize,
    float: f64,
    const Self = @This();

    pub inline fn toImm(self: Self) Imm {
        switch (self) {
            .int => |ival| return Imm{ .imm = @intCast(u64, @bitCast(usize, ival)) },
            .float => |fval| return Imm{ .imm = @intCast(u64, @bitCast(usize, fval)) },
            // .int => |ival| return Imm.fromISize(ival),
            // .float => |fval| return Imm.fromFloat(fval),
        }
    }

    inline fn toImmForConstData(self: Self) Imm {
        switch (self) {
            .int => |ival| return Imm.fromISize(ival),
            .float => |fval| return Imm.fromFloat(fval),
        }
    }

    inline fn bytesVal(self: Self, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        return self.toImmForConstData().encode(allocator);
    }

    pub inline fn toISize(self: Self) !isize {
        switch (self) {
            .int => |ival| return ival,
            else => return error.InvalidNumberType,
        }
    }
};

pub const Token = struct {
    type: TokenKind,
    val: TokenVal,
    line: u32,
    offset: u32,
    const Self = @This();
    pub fn print(self: Self) void {
        const token_type = self.type.strval();
        switch (self.val) {
            .str => |string| std.log.info("[{s}]({d}:{d}): {s}", .{ token_type, self.line, self.offset, string }),
            .num => |number| {
                switch (number) {
                    .int => |inum| std.log.info("[{s}]({d}:{d}): {d}", .{ token_type, self.line, self.offset, inum }),
                    .float => |fnum| std.log.info("[{s}]({d}:{d}): {d}", .{ token_type, self.line, self.offset, fnum }),
                }
            },
            .ch => |char| std.log.info("[{s}]({d}:{d}): {c}", .{ token_type, self.line, self.offset, char }),
        }
    }

    pub fn bytesValue(self: Self, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        if (self.type == .num or self.type == .str or self.type == .chr) {} else {
            return error.InvalidTokenKind;
        }

        return self.val.bytesVal(allocator);
    }

    pub fn numVal(self: Self) !NumberVal {
        if (self.type == .num) {} else {
            return error.InvalidTokenKind;
        }

        return self.val.numVal();
    }
};

pub const Lexer = struct {
    lines: std.ArrayList([]const u8),
    line: u32,
    line_offset: u32,
    peekc: ?u8,

    const Self = @This();
    pub fn init(lines: std.ArrayList([]const u8)) Self {
        return Self{ .lines = lines, .line = 0, .line_offset = 0, .peekc = null };
    }

    pub inline fn currentLine(self: Self) []const u8 {
        return self.lines.items[self.line];
    }

    fn next(self: *Self) ?u8 {
        return if (self.peekc) |ch| blk: {
            self.peekc = null;
            break :blk ch;
        } else if (self.line_offset >= self.currentLine().len) {
            self.line_offset += 1;
            if (self.line >= self.lines.items.len - 1) return null else return '\n';
        } else blk: {
            const ch = self.currentLine()[self.line_offset];
            self.line_offset += 1;
            break :blk ch;
        };
    }

    inline fn incrementLine(self: *Self) void {
        self.line += 1;
        self.line_offset = 0;
    }

    inline fn isSpace(ch: u8) bool {
        return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
    }

    inline fn isId(ch: u8) bool {
        return (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z') or ch == '_';
    }

    inline fn isNum(ch: u8) bool {
        return (ch >= '0' and ch <= '9');
    }

    inline fn isHexLiteral(ch: u8) bool {
        return (ch >= 'A' and ch <= 'F') or (ch >= 'a' and ch <= 'f');
    }

    fn accept(self: *Self, ch: u8) !void {
        if (self.next()) |next_ch| {
            if (next_ch != ch) {
                self.peekc = next_ch;
                std.log.err("Expecting `{c}` after: line ({}: {})", .{ ch, self.line + 1, self.line_offset - 1 });
                return error.UnexpectedChar;
            }
        } else {
            std.log.err("Invalid `{c}` at the end of file", .{ch});
            return error.InvalidEOF;
        }
    }

    pub inline fn skipLine(self: *Self) void {
        if (self.line_offset == 0) return;
        while (self.next()) |ch| {
            if (ch == '\n') break;
        } else {
            return;
        }

        self.incrementLine();
    }

    fn skipWhitespaces(self: *Self) void {
        while (self.next()) |ch| {
            if (!isSpace(ch)) {
                self.peekc = ch;
                break;
            } else if (ch == '\n') {
                break;
            }
        }
    }

    fn fetchIdToken(self: *Self) Token {
        const start_ofst = self.line_offset - 1;
        const start_col = self.line_offset;
        while (self.next()) |ch| {
            if (isId(ch) == false and isNum(ch) == false) {
                self.peekc = ch;
                break;
            }
        }
        const end_ofst = self.line_offset - 1;
        return Token{ .type = TokenKind.id, .val = .{ .str = self.currentLine()[start_ofst..end_ofst] }, .line = self.line + 1, .offset = start_col };
    }

    fn fetchNumberToken(self: *Self, sign: bool) !Token {
        const start_ofst = if (sign) self.line_offset - 2 else self.line_offset - 1;
        const start_col = start_ofst + 1;
        var found_dot = false;
        var radix: u8 = 10;
        while (self.next()) |ch| {
            if (isNum(ch)) {
                continue;
            } else if (radix == 16 and isHexLiteral(ch)) {
                continue;
            } else if ((ch == 'x' or ch == 'o' or ch == 'b') and self.line_offset - start_ofst == 2) {
                // Found radix
                radix = if (ch == 'x') @as(u8, 16) else if (ch == 'o') @as(u8, 8) else @as(u8, 2);
                continue;
            } else if (ch == '.') {
                if (!found_dot) {
                    found_dot = true;
                } else {
                    return error.InvalidFormat;
                }
            } else {
                self.peekc = ch;
                break;
            }
        }
        const end_ofst = self.line_offset - 1;
        const num_val = if (found_dot) NumberVal{ .float = try std.fmt.parseFloat(f64, self.currentLine()[start_ofst..end_ofst]) } else NumberVal{ .int = try std.fmt.parseInt(isize, self.currentLine()[start_ofst..end_ofst], 0) };
        return Token{ .type = TokenKind.num, .val = .{ .num = num_val }, .line = self.line + 1, .offset = start_col };
    }

    fn fetchStringToken(self: *Self) !Token {
        const start_ofst = self.line_offset;
        const start_col = self.line_offset;

        while (self.next()) |ch| {
            if (ch == '"') break;
        } else {
            std.log.err("Invalid end of string (EOF)", .{});
            return error.InvalidEOF;
        }

        const end_ofst = self.line_offset - 1;
        return Token{ .type = TokenKind.str, .val = .{ .str = self.currentLine()[start_ofst..end_ofst] }, .line = self.line + 1, .offset = start_col };
    }

    inline fn lexChar(self: *Self) !?Token {
        if (self.next()) |next_ch| {
            try self.accept('\'');
            return Token{ .type = TokenKind.chr, .val = .{ .ch = next_ch }, .line = self.line + 1, .offset = self.line_offset - 2 };
        } else {
            std.log.err("Invalid end of file, expecting a char value", .{});
            return null;
        }
    }

    inline fn lexMinus(self: *Self) !Token {
        const ch = '-';
        if (self.next()) |next_ch| {
            if (isNum(next_ch)) {
                return try self.fetchNumberToken(true);
            } else {
                self.peekc = next_ch;
                return Token{ .type = TokenKind.min, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 };
            }
        } else {
            return Token{ .type = TokenKind.min, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 };
        }
    }

    pub fn fetchNextToken(self: *Self) ?Token {
        loop: while (self.next()) |ch| {
            if (isSpace(ch)) {
                if (ch == '\n') {
                    const nl_token = Token{ .type = TokenKind.nl, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 };
                    self.incrementLine();
                    return nl_token;
                }
                self.skipWhitespaces();
                continue :loop;
            } else if (isId(ch)) {
                return self.fetchIdToken();
            } else if (isNum(ch)) {
                return self.fetchNumberToken(false) catch |err| {
                    std.log.err("Invalid number: {}", .{err});
                    continue :loop;
                };
            } else {
                switch (ch) {
                    '-' => return self.lexMinus() catch |err| {
                        std.log.err("Invalid number: {}", .{err});
                        continue :loop;
                    },
                    '+' => return Token{ .type = TokenKind.pls, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '*' => return Token{ .type = TokenKind.mul, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '/' => return Token{ .type = TokenKind.div, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '%' => return Token{ .type = TokenKind.mod, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '.' => return Token{ .type = TokenKind.dot, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    ',' => return Token{ .type = TokenKind.com, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    ':' => return Token{ .type = TokenKind.col, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '$' => return Token{ .type = TokenKind.dol, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '#' => return Token{ .type = TokenKind.hsh, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '[' => return Token{ .type = TokenKind.lbrk, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    ']' => return Token{ .type = TokenKind.rbrk, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 },
                    '"' => return self.fetchStringToken() catch {
                        continue :loop;
                    },
                    '\'' => return self.lexChar() catch {
                        continue :loop;
                    },
                    ';' => {
                        // Single line comment
                        const nl_token = Token{ .type = TokenKind.nl, .val = .{ .ch = ch }, .line = self.line + 1, .offset = self.line_offset - 1 };
                        self.skipLine();
                        return nl_token;
                    },
                    else => {},
                }
            }
        } else {
            return null;
        }
    }
};
