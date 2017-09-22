#!/usr/bin/env node
'use strict';

const fs = require('fs');
const R = require('ramda');
const mkdirp = require('mkdirp');
const diff = require('./diff.js');
var program = require('commander');
const Haskell = require('./haskell/index.js');

program
  .version('0.0.0')
  .option('-s --src [type]', 'Directory of colorless specs')
  .option('-d --dest [type]', 'Directory to generate code')
  .option('-n --name [type]', 'Name of top level source file and directory')
  .option('-l --lang [type]', 'Language of code')
  .option('-m --prefix [type]', 'Prefix or module name')
  .option('-e --side [type]', '\'client\' or \'server\' side code', 'client')
  .parse(process.argv);

const hasJsonExtension = (name) => {
  const s = name.split('.');
  return s[s.length - 1] === 'json';
};

(function() {
  if (
      program.lang === 'haskell' &&
      program.side === 'server' &&
      program.name && program.name.length &&
      program.dest && program.dest.length &&
      program.src && program.src.length) {

    fs.readdir(program.src, function (err, items) {
      if (err) console.log(err)
      else {
        const files = items.filter(hasJsonExtension);
        const jsonSpecs = files.map(file => expandTypes(JSON.parse(fs.readFileSync(program.src + '/' + file, 'utf8'))));

        var diffs = [];
        for (var i = 0; i < jsonSpecs.length - 1; i++) {
          diffs.push(diff.diff(jsonSpecs[i], jsonSpecs[i + 1]));
        }

        var version = { major: 0, minor: 0 };
        var specs = [];
        for (var i = 0; i < jsonSpecs.length; i++) {
          specs.push(Haskell.spec(program.prefix, version, jsonSpecs[i]));
          if (i < diffs.length) {
            version = nextVersion(version, versionChange(diffs[i]));
          }
        }
        // TODO
        const spec = specs[specs.length - 1];
        const latest = Haskell.latest(spec);

        mkdirp(program.dest, function (err) {
          if (err) { console.error(err)
          } else {
            const path = program.dest + '/' + program.name;
            mkdirp(path, function (err) {
              if (err) { console.error(err)
              } else {
                fs.writeFile(path + '.hs', latest, function (err) {
                  if (err) { console.error(err)
                  } else {
                    writeCode(path, specs);
                  }
                });
              }
            });
          }
        });
      }
    });
    return;
  }

  console.log('Bad args');
})();

const writeCode = (path, specs) => {
  if (!specs.length) {
    return;
  }
  const code = Haskell.gen(specs[0]);
  const filePath = path + '/V' + specs[0].version.major + '.hs';
  fs.writeFile(filePath, code, function (err) {
    if (err) { console.error(err)
    } else {
      writeCode(path, specs.slice(1));
    }
  });
};

const versionChange = diff => {
  if (diff.removeType.length ||
      diff.modifyType.length ||
      diff.modifyWrap.length ||
      diff.modifyStruct.length ||
      diff.modifyEnumeration.filter(e => e.removeEnumerator.length || e.modifyEnumerator.length || !!e.removeOutput).length) {
    return 'major';
  }
  return 'minor';
};

const nextVersion = ({major, minor}, delta) => ({
  major: delta === 'major' ? major + 1 : major,
  minor: delta === 'minor' ? minor + 1 : (delta === 'major' ? 0 : minor),
});

// expand enumerations
const expandTypes = s => R.merge(s, {
  types: s.types.map(ty => ty.e
      ? R.merge(ty, { e: (ty.e.map(e => typeof e === 'string' ? { tag: e } : e)) })
      : ty
    )
});
