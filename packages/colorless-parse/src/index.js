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
 * f = from (mappings)
 * t = to (mappings)
 * i = input (projections)
 * s = show (projections)
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

const hasKeys = (ls, o) => {
    for (var i in ls) {
        if (!(ls[i] in o)) return false;
    }
    return true;
};

const notKeys = (ls, o) => {
    for (var i in ls) {
        if ((ls[i] in o)) return false;
    }
    return true;
};

const isEnum = v => hasKeys(['n', 'e'], v) && notKeys(['m', 'u', 'w'], v);

function Enum(name, params, enumerals, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.e = enumerals.map(R.compose(toEnumeral, expandFromName));
    this.o = output || { n: "Unit" };
    this.g = groups || [];
    this.d = description || [];
}

const toEnum = v => new Enum(v.n, v.p, v.e, v.o, v.g, v.d);

const isEnumeral = v => hasKeys(['n'], v) && notKeys(['m', 'e', 'u', 'w', 'g', 'd'], v);

function Enumeral(name, params, members) {
    this.n = name;
    this.p = params || [];
    this.m = members || [];
}

const toEnumeral = v => new Enumeral(v.n, v.p, v.m);

const isStruct = v => hasKeys(['n', 'm'], v) && notKeys(['e', 'u', 'w'], v);

function Struct(name, params, members, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.m = members.map(R.map(expandFromName));
    this.o = output || { n: "Unit" };
    this.g = groups || [];
    this.d = description || [];
}

const toStruct = v => new Struct(v.n, v.p, v.m, v.o, v.g, v.d);

const isWrapper = v => hasKeys(['n', 'w'], v) && notKeys(['e', 'm', 'u'], v);

function Wrapper(name, params, wrapper, output, groups, description) {
    this.n = name;
    this.p = params || [];
    this.w = wrapper || { n: "Unit" };
    this.o = output || { n: "Unit" };
    this.g = groups || [];
    this.d = description || [];
}

const toWrapper = v => new Wrapper(v.n, v.p, v.w, v.o, v.g, v.d);

const isUnion = v => hasKeys(['n', 'u'], v) && notKeys(['e', 'm', 'w', 'o'], v);

function Union(name, params, union, groups, description) {
    this.n = name;
    this.p = params || [];
    this.u = union.map(R.compose(ensureParams, expandFromName));
    this.g = groups || [];
    this.d = description || [];
}

const toUnion = v => new Union(v.n, v.p, v.u, v.g, v.d);

var types = {
    "types": [
        { "n": "Suit", "e": [ "Hearts", "Diamonds", "Clubs", "Spades" ] },
        { "n": "Rank", "e": [ "Ace", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "Jack", "Queen", "King" ] },
        { "n": "D", "e": [ "X", "Y", "Z" ] },
        { "n": "E", "p": [ "a", "b" ], "e": [ { "n": "X", "p": [ { "n": "List", "p": [ "a" ] } ] }, { "n": "Y", "p": [ { "n": "List", "p": [ "b" ] } ] }, "Z" ] },
        { "n": "A", "m": [ {"x":"I32"} ], "g": [ "C" ] },
        { "n": "B", "p": [ "a" ], "m": [ {"x":"a"} ], "g": [ "C" ] },
        { "n": "Id", "p": [ { "n": "a", "k": "String" } ], "w": "I32" },
        { "n": "Card", "m": [ {"suit":"Suit"} , {"rank":"Rank"} ] },
        { "n": "Joker" },
        { "n": "AnyCard", "u": [ "Card", "Joker" ] }
    ]
};

const typeDeclTag = v => {
    if (isStruct(v)) return [ 'struct', v ];
    if (isEnum(v)) return [ 'enum', v ];
    if (isUnion(v)) return [ 'union', v ];
    if (isWrapper(v)) return [ 'wrapper', v ];
};

function typeDeclTags(v) {
    return v.types.map(function (t) { return typeDeclTag(t); } );
}

const expandFromName = v => (typeof v == 'string') ? { n: v } : v;

const ensureParams = v => Object.assign({ p: [] }, v);

const ensureMembers = v => Object.assign({ m: [] }, v);

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

function Checker() {
    this.errors = [];
    this.warnings = [];
}

Checker.prototype = {
    assert: function(cond, error) {
        if (!cond) {
            this.errors.push(error);
        }
        return cond;
    },
    suggest: function(cond, warning) {
        if (!cond) {
            this.warnings.push(warning);
        }
        return cond;
    },
    orResult: function(f) {
        return !!this.errors.length
            ?  {
                type: 'error',
                errors: this.errors,
                warnings: this.warnings
            } : {
                type: 'ok',
                value: f(),
                warnings: this.warnings
            };
    }
};

function validateWrapper(f) {
    var checker = new Checker();
    checker.assert('n' in f, 'missing name') &&
        checker.assert(isUpperCamelCase(f.n), 'upper camel case name');
    checker.assert('o' in f, 'missing output');
    checker.assert('w' in f, 'missing wrapper');
    checker.assert('g' in f, 'missing groups');
    checker.assert('d' in f, 'missing description');
    return checker.orResult(() => f);
}

var mappings = {
    "mappings": [
        { "f": { "n": "Id", "p": [ "Person" ], "t": "Person" }
    ]
};

var services = {
    "services": [
        {
            "direction": "Push",
            "protocol": "Http",
            "name": "Root",
            "address": "127.0.0.1",
            "path": "/",
            "format": "json",
            "port": 8886,
            "send": [ { "message": "Root", "error": "Unit" } ]
        },
        {
            "direction": "Push",
            "protocol": "Http",
            "name": "Calculator",
            "address": "127.0.0.1",
            "path": "calculator",
            "format": "json",
            "port": 8887,
            "send": [ { "message": "Operation", "error": "Unit" } ]
        },
        {
            "direction": "Push",
            "protocol": "Http",
            "name": "Example",
            "address": "127.0.0.1",
            "path": "/example/example",
            "format": "JSON",
            "port": 8888,
            "send": [ { "message": "Combined", "error": "Unit" } ]
        },
        {
            "direction": "Pull",
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

function generateRuntimeClient(spec) {
    var rpc = spec.rpc[0];
    return function () {
        fetch("https://" + rpc.address)
            .then(r => r.json())
            .then(console.log)
    }
}

module.exports = {
    parse: () => "",
    toEnum: toEnum,
    toEnumernal: toEnumeral,
    toStruct: toStruct,
    toWrapper: toWrapper,
    toUnion: toUnion,
    validateWrapper: validateWrapper,
    expandFromName: expandFromName,
    generateRuntimeClient: generateRuntimeClient
};
