/*
 * n = name
 * m = members
 * c = constructors
 * w = wrapper type
 * a = arguments
 * o = output type
 * p = type parameters
 * t = tags
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
    return hasKeys(['n', 'c'], v) && notKeys(['m', 'a', 'w', 'o'], v);
}

function Enum(name, params, ctors, tags, description) {
    this.n = name;
    this.p = params || [];
    this.c = ctors;
    this.t = tags || [];
    this.d = description || '';
}

function toEnum(v) {
    return new Enum(v.n, v.p, v.c, v.t, v.d);
}

function isConstructor(v) {
    return hasKeys(['n'], v) && notKeys(['m', 'a', 'c', 'w', 'o', 't'], v);
}

function Constructor(name, params) {
    this.n = name;
    this.p = params || [];
}

function toConstructor(v) {
    return new Constructor(v.n, v.p);
}

function isStruct(v) {
    return hasKeys(['n', 'm'], v) && notKeys(['c', 'a', 'w', 'o'], v);
}

function Struct(name, params, members, tags, description) {
    this.n = name;
    this.p = params || [];
    this.m = members;
    this.t = tags || [];
    this.d = description || '';
}

function toStruct(v) {
    return new Struct(v.n, v.p, v.m, v.t, v.d);
}

function isWrapper(v) {
    return hasKeys(['n', 'w'], v) && notKeys(['c', 'm', 'a', 'o'], v);
}

function Wrapper(name, params, wrapper, tags, description) {
    this.n = name;
    this.p = params || [];
    this.s = wrapper;
    this.t = tags || [];
    this.d = description || '';
}

function toWrapper(v) {
    return new Wrapper(v.n, v.p, v.s, v.t, v.d);
}

var types = {
    "enums": [
    //  { "n": "Suit", "c": [ "Hearts", "Diamonds", "Clubs", "Spades" ] },
        { "n": "Suit", "c": [ { "n": "Hearts" }, { "n": "Diamonds" }, { "n": "Clubs" }, { "n": "Spades" }] },
        { "n": "Rank", "c": [ { "n": "Ace" }, { "n": "R2" }, { "n": "R3" }, { "n": "R4" }, { "n": "R5" }, { "n": "R6" }, { "n": "R7" }, { "n": "R8" }, { "n": "R9" }, { "n": "R10" }, { "n": "Jack" }, { "n": "Queen" }, { "n": "King" } ] },
        { "n": "D", "c": [ { "n": "X" }, { "n": "Y" }, { "n": "Z" } ] },
        { "n": "E", "p": [ { "n": "a" }, { "n": "b" }], "c": [ { "n": "X", "p": [ { "n": "List", "p": [ {"n": "a" } ] } ] }, { "n": "Y", "p": [ { "n": "List", "p": [ {"n": "b" } ] } ] }, { "n": "Z" } ] }
    ],
    "structs": [
        { "n": "A", "m": { "x": { "n": "I32" } }, "t": [ "C" ] },
        { "n": "B", "p": [ { "n": "a" } ], "m": { "x": "a" }, "t": [ "C" ] },
    ],
    "wrappers": [
        { "n": "Id", "p": [ { "n": "a", "k": "String" } ], "w": { "n": "I32" } }
    ]
};

function typeDeclTag(v) {
    if (isStruct(v)) return [ 'struct', v ];
    if (isEnum(v)) return [ 'enum', v ];
    if (isWrapper(v)) return [ 'wrapper', v ];
}

function typeDeclTags(v) {
    return v.types.map(function (t) { return typeDeclTag(t); } );
}

//

var functions = {
    "functions": [
        { "n": "helloWorld", "o": "String" },
        { "n": "add", "a": [ {"x":"F32"}, {"y":"F32"} ], "o": "F32" }
    ]
};

function isFunctionDecl(f) {
    return 'n' in f && 'o' in f
}

function areFunctionDecls(v) {
    return v.functions.map(function (f) { return [f.n, isFunctionDecl(f)]; } );
}

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

function validateFunction(f) {
    var errors = new Errors();
    errors.assert('n' in f, 'missing name') &&
        errors.assert(isLowerCamelCase(f.n), 'lower camel case name');
    errors.assert('o' in f, 'missing output') &&
        errors.assert(isPrimitiveType(f.o), 'invalid primitive output');
    errors.assert('a' in f, 'missing arguments');
    errors.assert('t' in f, 'missing tags');
    errors.assert('d' in f, 'missing description');
    return errors.orResult(() => f);
}

var relations = {
    "relations": [
        [
            { "n": "Id", "p": [ { "n": "Person" } ] },
            { "n": "Person" }
        ],
    ]
};

var domains = {
    "domains": [
        {
            "name": "Calculator",
            "modules": [
                "Calculator.0.json"
            ],
            "domains": [
            ]
        },
        {
            "name": "Example",
            "modules": [
                "Imports.0.json",
                "Deck.0.json",
                "Spec.0.json"
            ],
            "domains": [
            ]
        },
        {
            "name": "Combined",
            "modules": [
            ],
            "domains": [
                "Example",
                "Calculator"
            ]
        }
    ]
};

var services = {
    "rpc": [
        [   "http",
            {
                "name": "Root",
                "address": "127.0.0.1",
                "path": "/",
                "format": "json",
                "port": 8888,
                "domain": "Combined",
                "error": { "n": "Error" }
            }
        ],
        [   
            "http",
            {
                "name": "Calculator",
                "address": "127.0.0.1",
                "path": "calculator",
                "format": "json",
                "port": 8888,
                "domain": "Calculator",
                "error": { "n": "Unit" }
            }
        ],
        [ 
            "http",
            {
                "name": "Example",
                "address": "127.0.0.1",
                "path": "/example/example",
                "format": "json",
                "port": 8888,
                "domain": "Example",
                "error": { "n": "Unit" }
            }
        ],
    ],
    "stream": [
        [
            "websocket",
            {
                "name": "Example",
                "address": "127.0.0.1",
                "path": "/event-stream",
                "format": "json",
                "port": 8888,
                "domain": "Example",
                "event": { "n": "Event" }
            }
        ]
    ]
};

module.exports = {
    parse: () => "",
    validateFunction: validateFunction
};
