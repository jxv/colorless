/*
 * n = name
 * f = fields
 * c = constructors
 * s = type synonym
 * a = arguments
 * o = output type
 * p = type parameters
 * t = tags
 * k = type level value constraint
 *
 */

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

function isSum(v) {
    return hasKeys(['n', 'c'], v) && notKeys(['f', 'a', 's', 'o'], v);
}

function Sum(name, params, ctors, tags, description) {
    this.n = name;
    this.p = params || [];
    this.c = ctors;
    this.t = tags || [];
    this.d = description || '';
}

function toSum(v) {
    return new Sum(v.n, v.p, v.c, v.t, v.d);
}

function isConstructor(v) {
    return hasKeys(['n'], v) && notKeys(['f', 'a', 'c', 's', 'o', 't'], v);
}

function Constructor(name, params) {
    this.n = name;
    this.p = params || [];
}

function toConstructor(v) {
    return new Constructor(v.n, v.p);
}

function isProduct(v) {
    return hasKeys(['n', 'f'], v) && notKeys(['c', 'a', 's', 'o'], v);
}

function Product(name, params, fields, tags, description) {
    this.n = name;
    this.p = params || [];
    this.f = fields;
    this.t = tags || [];
    this.d = description || '';
}

function toProduct(v) {
    return new Product(v.n, v.p, v.f, v.t, v.d);
}

function isSynonym(v) {
    return hasKeys(['n', 's'], v) && notKeys(['c', 'f', 'a', 'o'], v);
}

function Synonym(name, params, synonym, tags, description) {
    this.n = name;
    this.p = params || [];
    this.s = synonym;
    this.t = tags || [];
    this.d = description || '';
}

function toSynonym(v) {
    return new Synonym(v.n, v.p, v.s, v.t, v.d);
}

var types = {
    "types": [
     // { "n": "Suit", "c": [ "Hearts", "Diamonds", "Clubs", "Spades" ] },
        { "n": "Suit", "c": [ { "n": "Hearts" }, { "n": "Diamonds" }, { "n": "Clubs" }, { "n": "Spades" }] },
        { "n": "Rank", "c": [ { "n": "Ace" }, { "n": "R2" }, { "n": "R3" }, { "n": "R4" }, { "n": "R5" }, { "n": "R6" }, { "n": "R7" }, { "n": "R8" }, { "n": "R9" }, { "n": "R10" }, { "n": "Jack" }, { "n": "Queen" }, { "n": "King" } ] },
        { "n": "A", "f": [ ["x", { "n": "i32" }] ], "t": [ "C" ] },
        { "n": "A", "f": [ ["x", { "n": "i32" }] ], "t": [ "C" ] },
        { "n": "B", "p": [ { "n": "a" } ], "f": [["x","a"]], "t": [ "C" ] },
        { "n": "D", "c": [ { "n": "X" }, { "n": "Y" }, { "n": "Z" } ] },
        { "n": "E", "p": [ { "n": "a" }, { "n": "b" }], "c": [ { "n": "X", "p": [ { "n": "List", "p": [ {"n": "a" } ] } ] }, { "n": "Y", "p": [ { "n": "List", "p": [ {"n": "b" } ] } ] }, { "n": "Z" } ] },
        { "n": "Id", "p": [ { "n": "a", "k": "string" } ], "s": { "n": "i32" } }
    ]
};

function typeDeclTag(v) {
    if (isProduct(v)) return [ 'product', v ];
    if (isSum(v)) return [ 'sum', v ];
    if (isSynonym(v)) return [ 'synonym', v ];
}

function typeDeclTags(v) {
    return v.types.map(function (t) { return typeDeclTag(t); } );
}

//

var functions = {
    "functions": [
        { "n": "helloWorld", "o": "string" },
        { "n": "add", "a": [["x","f32"], ["y","f32"]], "o": "f32" }
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
    return ty === 'string';
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
                "error": { "n": "unit" }
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
                "error": { "n": "unit" }
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
