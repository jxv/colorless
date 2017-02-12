/*
 * _n = name
 * _f = fields
 * _c = constructors
 * _s = type synonym
 * _a = arguments
 * _o = output type
 * _p = type parameters
 * _t = tags
 * _k = type level value constraint
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
    return hasKeys(['_n', '_c'], v) && notKeys(['_f', '_a', '_s', '_o'], v);
}

function Sum(name, params, ctors, tags) {
    this._n = name;
    this._p = params || [];
    this._c = ctors;
    this._t = tags || [];
}

function toSum(v) {
    return new Sum(v._n, v._p, v._c, v._t);
}

function isConstructor(v) {
    return hasKeys(['_n'], v) && notKeys(['_f', '_a', '_c', '_s', '_o', '_t'], v);
}

function Constructor(name, params) {
    this._n = name;
    this._p = params || [];
}

function toConstructor(v) {
    return new Constructor(v._n, v._p);
}

function isProduct(v) {
    return hasKeys(['_n', '_f'], v) && notKeys(['_c', '_a', '_s', '_o'], v);
}

function Product(name, params, fields, tags) {
    this._n = name;
    this._p = params || [];
    this._f = fields;
    this._t = tags || [];
}

function toProduct(v) {
    return new Product(v._n, v._p, v._f, v._t);
}

function isSynonym(v) {
    return hasKeys(['_n', '_s'], v) && notKeys(['_c', '_f', '_a', '_o'], v);
}

function Synonym(name, params, synonym, tags) {
    this._n = name;
    this._p = params || [];
    this._s = synonym;
    this._t = tags || [];
}

function toSynonym(v) {
    return new Synonym(v._n, v._p, v._s, v._t);
}

var types = {
    "_types": [
        { "_n": "Suit", "_c": [ { "_n": "Hearts" }, { "_n": "Diamonds" }, { "_n": "Clubs" }, { "_n": "Spades" }] },
        { "_n": "Rank", "_c": [
                { "_n": "Ace" },
                { "_n": "R2" },
                { "_n": "R3" },
                { "_n": "R4" },
                { "_n": "R5" },
                { "_n": "R6" },
                { "_n": "R7" },
                { "_n": "R8" },
                { "_n": "R9" },
                { "_n": "R10" },
                { "_n": "Jack" },
                { "_n": "Queen" },
                { "_n": "King" }
            ]
        },
        { "_n": "A", "_f": { "x": { "_n": "i32" } }, "_t": [ "C" ] },
        { "_n": "A", "_f": { "x": { "_n": "i32" } }, "_t": [ "C" ] },
        { "_n": "B", "_p": [ { "_n": "a" } ], "_f": { "x": "a" }, "_t": [ "C" ] },
        { "_n": "D", "_c": [ { "_n": "X" }, { "_n": "Y" }, { "_n": "Z" } ] },
        { "_n": "E", "_p": [ { "_n": "a" }, { "_n": "b" }], "_c": [
                { "_n": "X", "_p": [ { "_n": "List", "_p": [ {"_n": "a" } ] } ] },
                { "_n": "Y", "_p": [ { "_n": "List", "_p": [ {"_n": "b" } ] } ] },
                { "_n": "Z" }
            ] 
        },
        { "_n": "Id", "_p": [ { "_n": "a", "_k": "str" } ], "_s": { "_n": "i32" } }
    ]
};

function typeDeclTag(v) {
    if (isProduct(v)) return [ 'product', v ];
    if (isSum(v)) return [ 'sum', v ];
    if (isSynonym(v)) return [ 'synonym', v ];
}

function typeDeclTags(v) {
    return v._types.map(function (t) { return  typeDeclTag(t); } );
}

//

var functions = {
    "_functions": [
        { "_n": "helloWorld", "_o": "str" },
        { "_n": "add", "_a": ["f32", "f32"], "_o": "f32" }
    ]
};

function isFunctionDecl(f) {
    return '_n' in f && '_o' in f
}

function areFunctionDecls(v) {
    return v._functions.map(function (f) { return [ f._n, isFunctionDecl(f) ]; } );
}

module.exports = {
    parse: () => ""
};
