const R = require('ramda');
var Lines = require('../../lines.js').Lines;

const gen = (s, addons) => {
  var lines = new Lines();
  lines.add(['require "json"\n', '\n']);
  lines.add(['module V', s.version.major, '\n']);
  lines.add(['  def spec\n', '    return JSON.parse(', JSON.stringify(JSON.stringify(s.original)), ')\n', '  end\n']);
  lines.add(['end']);
  return lines.collapse();
};

module.exports.gen = gen;