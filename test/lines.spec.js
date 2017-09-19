const Lines = require('../src/lines.js').Lines;
const expect = require('chai').expect;

describe('lines constructor', () => {
  it('without input', () => {
    var x = new Lines();
    expect(x.lines).to.eql([]);
  })
  it('with string input', () => {
    var x = new Lines('x');
    expect(x.lines).to.eql(['x']);
  })
  it('with Lines input', () => {
    var x = new Lines(new Lines('x'));
    expect(x.lines).to.eql(['x']);
  })
  it('with array string input', () => {
    var x = new Lines(['x','y']);
    expect(x.lines).to.eql(['x','y']);
  })
  it('with array of array string input', () => {
    var x = new Lines([['x','y'],['z']]);
    expect(x.lines).to.eql(['x','y','z']);
  })
})

describe('lines.add', () => {
  it('with Lines', () => {
    var x = new Lines('x');
    x.add(new Lines('y'))
    expect(x.lines).to.eql(['x','y'])
  })
  it('with string', () => {
    var x = new Lines('x');
    x.add('y')
    expect(x.lines).to.eql(['x','y'])
  })
  it('with array string', () => {
    var x = new Lines('x');
    x.add(['y','z'])
    expect(x.lines).to.eql(['x','y','z'])
  })
  it('with array of array string', () => {
    var x = new Lines('x');
    x.add([['y','z'],['a']])
    expect(x.lines).to.eql(['x','y','z','a'])
  })
})

describe('lines.collapse', () => {
  it('collapse', () => {
    var x = new Lines(['x','y','z'])
    expect(x.collapse()).to.eql('xyz')
  })
})
