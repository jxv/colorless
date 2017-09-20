var gen = require('./gen.js');
var spec = require('./spec.js');
var latest = require('./latest.js');

module.exports = {
  gen: gen.gen,
  spec: spec.spec,
  latest: latest.latest,
};
