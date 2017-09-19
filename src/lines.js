function Lines(x) {
  if (x instanceof Lines) {
    this.lines = x.lines;
  } else if (typeof x === 'string') {
    this.lines = [x];
  } else if (Array.isArray(x) && x.length && typeof x[0] === 'string') {
    this.lines = x;
  } else if (Array.isArray(x) && x.length && Array.isArray(x[0]) && x[0].length && typeof x[0][0] === 'string') { // nested arrays
    this.lines = [].concat.apply([], x);
  } else {
    this.lines = [];
  }
  this.collapse = function() {
    return this.lines.join('');
  }
  this.add = function(x) { // add nested arrays of strings OR flat array of strings
    if (x instanceof Lines) {
      this.lines = this.lines.concat(x.lines);
    } else if (typeof x === 'string') {
      this.lines = this.lines.concat([x]);
    } else if (Array.isArray(x) && x.length && typeof x[0] === 'string') {
      this.lines = this.lines.concat(x);
    } else if (Array.isArray(x) && x.length && Array.isArray(x[0]) && x[0].length && typeof x[0][0] === 'string') { // nested arrays
      this.lines = [].concat.apply(this.lines, x);
    }
  }
}

module.exports.Lines = Lines;
