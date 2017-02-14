const expect = require('chai').expect;

//const index = require('../src/index.js');
const { validateFunction, Error,  Ok } = require('../src/index.js');

describe("parsing validation", () => {
    const hello = { _n: 'hello', _a: [], _o: 'string', _t: [], _d: '' };
    it("no function", () => {
        expect(validateFunction({})).to.deep.equal(['error', ['missing name', 'missing output', 'missing arguments', 'missing tags', 'missing description']]);
    });
    it("simple function", () => {
        expect(validateFunction(hello)).to.deep.equal(['ok', hello]);
    });
    it("simple function - lower camel case error", () => {
        var hello2 = hello;
        hello2._n = 'Hello';
        expect(validateFunction(hello2)).to.deep.equal(['error', ['name must be lower camel case']]);
    });

});
