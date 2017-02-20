/*
 * n = name
 * m = members
 * e = enumerals
 * w = wrapper
 * u = union
 * o = output
 * p = type parameters
 * g = groups
 * k = type level value constraint
 *
 */

/* Unit
 * U8, U16, U32, U64
 * I8, I16, I32, I64
 * F32, F64
 * Bool
 * Char
 * String
 */

var R = require('ramda');

function hasKeys(ls, o) {
    for (var i in ls) {
        if (!(ls[i] in o)) return false;
    }
    return true;
}

function notKeys(ls, o) {
    for (var i in ls) {
        if ((ls[i] in o)) return false;
    }
    return true;
}

function isEnum(v) {
    return hasKeys(['n', 'e'], v) && notKeys(['m', 'u', 'w'], v);
}

function Enum(name, params, enumerals, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.e = enumerals;
    this.o = output || "Unit";
    this.g = groups || [];
    this.d = description || [];
}

function toEnum(v) {
    return new Enum(v.n, v.p, v.e, v.o, v.g, v.d);
}

function isEnumeral(v) {
    return hasKeys(['n'], v) && notKeys(['m', 'e', 'u', 'w', 'g', 'd'], v);
}

function Enumeral(name, params) {
    this.n = name;
    this.p = params || [];
}

function toEnumeral(v) {
    return new Enumeral(v.n, v.p);
}

function isStruct(v) {
    return hasKeys(['n', 'm'], v) && notKeys(['e', 'u', 'w'], v);
}

function Struct(name, params, members, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.m = members;
    this.o = output || "Unit";
    this.g = groups || [];
    this.d = description || [];
}

function toStruct(v) {
    return new Struct(v.n, v.p, v.m, v.o, v.g, v.d);
}

function isWrapper(v) {
    return hasKeys(['n', 'w'], v) && notKeys(['e', 'm', 'u'], v);
}

function Wrapper(name, params, wrapper, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.w = wrapper;
    this.o = output || "Unit";
    this.g = groups || [];
    this.d = description || [];
}

function toWrapper(v) {
    return new Wrapper(v.n, v.p, v.w, v.o, v.g, v.d);
}

function isUnion(v) {
    return hasKeys(['n', 'u'], v) && notKeys(['e', 'm', 'w', 'o'], v);
}

function Union(name, params, union, groups, description) {
    this.n = name;
    this.p = params || [];
    this.u = union;
    this.g = groups || [];
    this.d = description || [];
}

var types = {
    "types": [
        { "n": "Suit", "e": [ "Hearts", "Diamonds", "Clubs", "Spades" ] },
        { "n": "Rank", "e": [ "Ace", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "Jack", "Queen", "King" ] },
        { "n": "D", "e": [ "X", "Y", "Z" ] },
        { "n": "E", "p": [ "a", "b" ], "e": [ { "n": "X", "p": [ { "n": "List", "p": [ "a" ] } ] }, { "n": "Y", "p": [ { "n": "List", "p": [ "b" ] } ] }, "Z" ] },
        { "n": "A", "m": [ [ "I32", "x" ] ], "g": [ "C" ] },
        { "n": "B", "p": [ "a" ], "m": [ [ "a", "x" ] ], "g": [ "C" ] },
        { "n": "Id", "p": [ { "n": "a", "k": "String" } ], "w": "I32" },
        { "n": "Card", "m": [ "Suit" , "Rank" ] },
        { "n": "Joker" },
        { "n": "AnyCard", "u": [ "Card", "Joker" ] }
    ]
};

function typeDeclTag(v) {
    if (isStruct(v)) return [ 'struct', v ];
    if (isEnum(v)) return [ 'enum', v ];
    if (isUnion(v)) return [ 'union', v ];
    if (isWrapper(v)) return [ 'wrapper', v ];
}

function typeDeclTags(v) {
    return v.types.map(function (t) { return typeDeclTag(t); } );
}

//

function hasCamelCaseCharacters(s) {
    for (var i in s) {
        if (!((s[i] >= 'a' && s[i] <= 'z') || (s[i] >= 'A' && s[i] <= 'Z') || (s[i] >= '0' &&  s[i] <= '9'))) return false;
    }
    return true;

}

function isLowerCamelCase(s) {
    if (!s.length) return false;
    if (!(s[0] >= 'a' && s[0] <= 'z')) return false;
    return hasCamelCaseCharacters(s);
}

function isUpperCamelCase(s) {
    if (!s.length) return false;
    if (!(s[0] >= 'A' && s[0] <= 'Z')) return false;
    return hasCamelCaseCharacters(s);
}

function isPrimitiveType(ty) {
    return ty === 'String';
}

function Errors() {
    this.list = [];
}

Errors.prototype = {
    assert: function(cond, error) {
        if (!cond) {
            this.list.push(error);
        }
        return cond;
    },
    orResult: function(f) {
        return !!this.list.length ? ['error', this.list] : ['ok', f()];
    }
};

function validateWrapper(f) {
    var errors = new Errors();
    errors.assert('n' in f, 'missing name') &&
        errors.assert(isUpperCamelCase(f.n), 'upper camel case name');
    errors.assert('o' in f, 'missing output');
    errors.assert('w' in f, 'missing wrapper');
    errors.assert('g' in f, 'missing groups');
    errors.assert('d' in f, 'missing description');
    return errors.orResult(() => f);
}

var relations = {
    "relations": [
        [ { "n": "Id", "p": [ "Person" ] }, "Person" ]
    ]
};

var services = {
    "rpc": [
        {
            "protocol": "http",
            "name": "Root",
            "address": "127.0.0.1",
            "path": "/",
            "format": "json",
            "port": 8886,
            "send": [ { "message": "Root", "error": "Unit" } ]
        },
        {
            "protocol": "http",
            "name": "Calculator",
            "address": "127.0.0.1",
            "path": "calculator",
            "format": "json",
            "port": 8887,
            "send": [ { "message": "Operation", "error": "Unit" } ]
        },
        {
            "protocol": "HTTP",
            "name": "Example",
            "address": "127.0.0.1",
            "path": "/example/example",
            "format": "JSON",
            "port": 8888,
            "send": [ { "message": "Combined", "error": "Unit" } ]
        }
    ],
    "stream": [
        {
            "protocol": "Websocket",
            "name": "Example",
            "address": "127.0.0.1",
            "path": "/event-stream",
            "format": "JSON",
            "port": 8888,
            "receive": [ "Event" ]
        }
    ]
};

module.exports = {
    parse: () => "",
    validateWrapper: validateWrapper
};
