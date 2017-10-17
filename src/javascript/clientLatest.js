var R = require('ramda');
var Lines = require('../lines.js').Lines;

const clientLatest = (specs) => {
  const spec = specs[specs.length - 1];

  var lines = new Lines();

  lines.add([
    'import { assert } from \'../../fluid\';\n',
  ]);
  lines.add([
    '\n',
    'export const version = {\n',
    '  major: ', spec.version.major, ',\n',
    '  minor: ', spec.version.minor, ',\n',
    '};\n',
  ]);

  spec.hollow.forEach(hollow => {
    if (hollow.func) {
      lines.add([
        '\n',
        'export const ', hollow.name, ' = () => ({\n',
        '  n: \'', hollow.label,'\',\n',
        '});\n'
      ]);
    }
  });

  spec.wrap.forEach(wrap => {
    if (wrap.func) {
      lines.add([
        '\n',
        'export const ', wrap.name, ' = (w) => {\n',
        '  assert(w !== undefined, "`', wrap.name,'` missing value");\n',
      ]);
      lines.add([
        '  return {\n',
        '    n: \'', wrap.label,'\',\n',
        '    w: w,\n',
        '  };\n',
        '};\n'
      ]);
    }
  });

  spec.struct.forEach(struct => {
    if (struct.func) {
      lines.add([
        '\n',
        'export const ', struct.name, ' = (m) => {\n',
        '  assert(m !== undefined, "`', struct.name,'` missing members");\n',
      ]);
      struct.members.forEach(member => {
        lines.add(['  assert(m.', member.name, ' !== undefined, "`', struct.name,'` missing member `', member.name,'`");\n']);
      });
      lines.add([
        '  return {\n',
        '    n: \'', struct.label,'\',\n',
        '    m: {\n',
      ]);
      struct.members.forEach(member => {
        lines.add(['      \'', member.label, '\': m.', member.name, ',\n']);
      });
      lines.add([
        '    },\n',
        '  };\n',
        '};\n'
      ]);
    }
  });

  spec.enumeration.forEach(enumeration => {
    if (enumeration.func) {
      lines.add([
        '\n',
        'export const ', enumeration.name, ' = (e) => {\n',
        '  assert(e !== undefined, "`', enumeration.name,'` missing enumeral");\n',
        '  assert(e.tag !== undefined, "`', enumeration.name,'` missing tag");\n',
      ]);
      lines.add('  assert(');
      enumeration.enumerals.forEach(enumeral => {
        lines.add(['e.tag === \'', enumeral.tag,'\' || ']);
      });
      lines.add(['false, "`', enumeration.name,'` unrecognized tag, `" + e.tag + "`");\n']);
      enumeration.enumerals.forEach(enumeral => {
        lines.add(['  if (e.tag === \'', enumeral.tag,'\') {\n']);
        enumeral.members.forEach(member => {
          lines.add(['    assert(e.', member.name,' !== undefined, "`', enumeration.name,'` with tag `', enumeral.tag,'` missing member `', member.name,'`");\n']);
        });
        lines.add('  }\n');
      });
      lines.add([
        '  return {\n',
        '    n: \'', enumeration.label,'\',\n',
        '    e: e,\n',
        '  };\n',
        '};\n'
      ]);
    }
  });

  return lines.collapse();
};

module.exports = {
  clientLatest,
};
