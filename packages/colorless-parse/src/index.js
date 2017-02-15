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

function Sum(name, params, ctors, tags, description) {
    this._n = name;
    this._p = params || [];
    this._c = ctors;
    this._t = tags || [];
    this._d = description || '';
}

function toSum(v) {
    return new Sum(v._n, v._p, v._c, v._t, v._d);
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

function Product(name, params, fields, tags, description) {
    this._n = name;
    this._p = params || [];
    this._f = fields;
    this._t = tags || [];
    this._d = description || '';
}

function toProduct(v) {
    return new Product(v._n, v._p, v._f, v._t, v._d);
}

function isSynonym(v) {
    return hasKeys(['_n', '_s'], v) && notKeys(['_c', '_f', '_a', '_o'], v);
}

function Synonym(name, params, synonym, tags, description) {
    this._n = name;
    this._p = params || [];
    this._s = synonym;
    this._t = tags || [];
    this._d = description || '';
}

function toSynonym(v) {
    return new Synonym(v._n, v._p, v._s, v._t, v._d);
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
    return v._functions.map(function (f) { return [f._n, isFunctionDecl(f)]; } );
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
    errors.assert('_n' in f, 'missing name') &&
        errors.assert(isLowerCamelCase(f._n), 'lower camel case name');
    errors.assert('_o' in f, 'missing output') &&
        errors.assert(isPrimitiveType(f._o), 'invalid primitive output');
    errors.assert('_a' in f, 'missing arguments');
    errors.assert('_t' in f, 'missing tags');
    errors.assert('_d' in f, 'missing description');
    return errors.orResult(() => f);
}

module.exports = {
    parse: () => "",
    validateFunction: validateFunction
};
