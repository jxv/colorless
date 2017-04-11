const R = require('ramda');
const expect = require('chai').expect;
const {
    validateWrapper,
    toWrapper,
    toUnion,
    toStruct,
    toEnum
} = require('../src/index.js');

describe('expand', () => {
    describe('wrapper', () => {
        it('toWrapper without inner type', () => {
            const actual = toWrapper({ n: 'MyType' });
            const expected = { n: 'MyType', p: [], d: [], g: [], o: { n: 'Unit' }, w: { n: 'Unit' } };
            expect(actual).to.deep.equal(expected);
        });
        it('toWrapper with inner type', () => {
            const actual = toWrapper({ n: 'MyType', w: { n: 'String' } });
            const expected = { n: 'MyType', p: [], d: [], g: [], o: { n: 'Unit' }, w: { n: 'String' } };
            expect(actual).to.deep.equal(expected);
        });
        it('toWrapper with output', () => {
            const actual = toWrapper({ n: 'MyType', w: { n: 'String' }, o: { n: 'String' } });
            const expected = { n: 'MyType', p: [], d: [], g: [], o: { n: 'String' }, w: { n: 'String' } };
            expect(actual).to.deep.equal(expected);
        });
    });
    describe('enum', () => {
        it('toEnum', () => {
            const actual = toEnum({ n: 'Color', e: [ 'Red', 'Green', 'Blue' ] });
            const expected = {
                n: 'Color',
                p: [],
                d: [],
                g: [],
                o: { n: 'Unit' },
                e: [
                    { n: 'Red', p: [], m: [] },
                    { n: 'Green', p: [], m: [] },
                    { n: 'Blue', p: [], m: [] }
                ]
            };
            expect(actual).to.deep.equal(expected);
        });
    });
    describe('struct', () => {
        it('toStruct', () => {
            const actual = toStruct({ n: 'Pos', m: [ {x:'F32'}, {y:'F32'} ] });
            const expected = {
                n: 'Pos',
                p: [],
                d: [],
                g: [],
                o: { n: 'Unit' },
                m: [
                    { x: { n: 'F32' } },
                    { y: { n: 'F32' } }
                ]
            };
            expect(actual).to.deep.equal(expected);
        });
    });
    describe('union', () => {
        it('toUnion', () => {
            const actual = toUnion({ n: 'Query', u: ['FindThing', 'GetThing'] });
            const expected = {
                n: 'Query',
                g: [],
                d: [],
                p: [],
                u: [
                    { n: 'FindThing', p: [] },
                    { n: 'GetThing', p: [] }
                ]
            };
            expect(actual).to.deep.equal(expected);
        });
    });
});

describe('parsing validation', () => {
    describe('wrapper', () => {
        const hello = { n: 'Hello', w: 'Unit', o: 'String', g: [], d: [] };
        it('no wrapper', () => {
            expect(validateWrapper({})).to.deep.equal({
                type: 'error',
                errors: ['missing name', 'missing output', 'missing wrapper', 'missing groups', 'missing description'],
                warnings: []
            });
        });
        it('simple wrapper', () => {
            expect(validateWrapper(hello)).to.deep.equal({ type: 'ok', value: hello, warnings: [] });
        });
        it('simple wrapper - upper camel case error', () => {
            var hello2 = R.clone(hello);
            hello2.n = 'hello';
            expect(validateWrapper(hello2)).to.deep.equal({ type: 'error', errors: ['upper camel case name'], warnings: [] });
        });
    });
});
