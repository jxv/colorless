const R = require('ramda');
const expect = require('chai').expect;

//const index = require('../src/index.js');
const { validateWrapper, Error,  Ok } = require('../src/index.js');

describe("parsing validation:", () => {
    const hello = { n: 'Hello', w: "Unit", o: 'String', g: [], d: [] };
    it("no wrapper", () => {
        expect(validateWrapper({})).to.deep.equal(['error', ['missing name', 'missing output', 'missing wrapper', 'missing groups', 'missing description']]);
    });
    it("simple wrapper", () => {
        expect(validateWrapper(hello)).to.deep.equal(['ok', hello]);
    });
    it("simple wrapper - upper camel case error", () => {
        var hello2 = R.clone(hello);
        hello2.n = 'hello';
        expect(validateWrapper(hello2)).to.deep.equal(['error', ['upper camel case name']]);
    });
});
