#!/usr/bin/env node
'use strict';

const fs = require('fs');
const R = require('ramda');
const mkdirp = require('mkdirp');
const diff = require('./diff.js');
var program = require('commander');

const Haskell = require('./haskell/index.js');
const JavaScript = require('./javascript/index.js');

program
  .version('0.0.0')
  .option('-s --src [type]', 'Directory of specs OR JSON containing array of specs')
  .option('-d --dest [type]', 'Directory to generate code')
  .option('-n --name [type]', 'Name of top level source file and directory')
  .option('-l --lang [type]', 'Language of code')
  .option('-m --prefix [type]', 'Prefix or module name')
  .option('-e --side [type]', '\'client\' or \'server\' side code', 'client')
  .option('-v --major [type]', 'Oldest supported major version', '0')
  .option('-a --addon [type]', 'Add-on code for client-side or server-side. May require additional dependencies.')
  .parse(process.argv);

const hasJsonExtension = (name) => {
  const s = name.split('.');
  return s[s.length - 1] === 'json';
};

const readSpecs = (path, items) => {
  const files = items.filter(hasJsonExtension);
  return files.map(file => expandTypes(JSON.parse(fs.readFileSync(path + '/' + file, 'utf8'))));
};

const diffSpecs = (specs) => {
  var diffs = [];
  for (var i = 0; i < specs.length - 1; i++) {
    diffs.push(diff.diff(specs[i], specs[i + 1]));
  }
  return diffs;
};

const supportedSpecs = (prefix, specForLang, diffs, jsonSpecs) => {
  // assert(diffs.length == jsonSpecs.length - 1)

 // by major version, extract the req specs and decide where to place the types
  var version = jsonSpecs[0].version || { major: 0, minor: 0 };
  var specs = [];
  var tyVers = [initTypeVersions(jsonSpecs[0].types.map(ty => ty.n), version)];

  for (var i = 0; i < jsonSpecs.length; i++) {
    const original = R.merge(jsonSpecs[i], {version});
    specs.push(R.merge(specForLang(prefix, version, jsonSpecs[i]), {original}));
    if (i < diffs.length) {
      const change = typeChanges(diffs[i]);
      version = jsonSpecs[i + 1].version || nextVersion(version, versionChange(change));
      tyVers.push(nextTypeVersion(tyVers[i], version, change));
    }
  }

  // attach the versioned type to each spec
  return attachTypeSources(specs, dropLowMinors(tyVers))
    .filter(({version}) => version.major >= +program.major);
};

const versionChange = changes => {
  if (changes.major.length) {
    return 'major';
  }
  if (changes.minor.length) {
    return 'minor';
  }
  return null;
};

const typeChanges = diff => ({
  major: R.uniq(R.flatten([
    diff.removeType
      .map(name => ({ name, action: 'remove' })),
    diff.modifyType
      .map(name => ({ name, action: 'modify' })),
    diff.modifyWrap
      .map(name => ({ name, action: 'modify' })),
    diff.modifyStruct
      .map(name => ({ name, action: 'modify' })),
    diff.modifyEnumeration
      .map(x => ({ name: x.name, action: 'modify' })),
  ])),
  minor: R.uniq(R.flatten([
    diff.addType
      .map(name => ({ name, action: 'add' })),
  ])),
});

const nextVersion = ({major, minor}, delta) => ({
  major: delta === 'major' ? major + 1 : major,
  minor: delta === 'minor' ? minor + 1 : (delta === 'major' ? 0 : minor),
});

const expandTypes = s => R.merge(s, {
  types: s.types.map(ty => ty.e
      ? R.merge(ty, { e: (ty.e.map(e => typeof e === 'string' ? { tag: e } : e)) })
      : ty
    )
});

const initTypeVersions = (types, version) => ({
  version: version,
  types: R.mergeAll(types.map(ty => ({ [ty]: version }))),
});

const nextTypeVersion = (typeVersion, version, change) => {
  const typeActions = R.concat(change.major, change.minor);
  const removeTypes = typeActions.filter(ty => ty.action === 'remove').map(ty => ty.name);
  const modifyTypes = typeActions.filter(ty => ty.action === 'modify').map(ty => ty.name);
  const addTypes = typeActions.filter(ty => ty.action === 'add').map(ty => ty.name);
  return {
    version: version,
    types:
      R.merge(
        R.reduce(R.dissoc, typeVersion.types, removeTypes),
        R.mergeAll(R.concat(addTypes, modifyTypes).map(name => ({ [name]: version })))
      ),
  };
};

// For each major version, collapse the types of non-greatest minor versions into the greatest minor version
const dropLowMinors = (tyVers) => {
  var lastTyVer = null;
  var dropped = [];
  R.reverse(tyVers).forEach(tyVer => {
    if (!!lastTyVer) {
      if (tyVer.version.major === lastTyVer.version.major) {
      } else {
        lastTyVer = { version: tyVer.version, typeSource: R.map(ver => ver.major, tyVer.types) };
        dropped.unshift(lastTyVer);
      }
    } else {
      lastTyVer = { version: tyVer.version, typeSource: R.map(ver => ver.major, tyVer.types) };
      dropped.unshift(lastTyVer);
    }
  });
  return dropped;
};

const attachTypeSources = (specs, typeSources) => {
  var attached = [];
  for (var i = 0; i < typeSources.length; i++) {
    const { version, typeSource } = typeSources[i];
    for (var j = 0; j < specs.length; j++) {
      if (R.equals(specs[j].version, version)) {
        attached.push(R.merge(specs[j], { typeSource }));
      }
    }
  }
  return attached;
};

const writeCode = (gen, pathBuilder, path, specs) => {
  if (!specs.length) {
    return;
  }
  const code = gen(specs[0]);
  const filePath = pathBuilder(path, specs[0].version.major);
  fs.writeFile(filePath, code, function (err) {
    if (err) { console.error(err)
    } else {
      writeCode(gen, pathBuilder, path, specs.slice(1));
    }
  });
};

const generateJavascriptClient = (program, jsonSpecs) => {
  if (!jsonSpecs.length) {
    console.log('No specs');
    return;
  }

  const diffs = diffSpecs(jsonSpecs);

  const specs = supportedSpecs(program.prefix, JavaScript.spec, diffs, jsonSpecs);
   if (!specs.length) {
    console.log('Spec support is too high')
    return;
  }

  const latest = JavaScript.clientLatest(specs);

  mkdirp(program.dest, function (err) {
    if (err) { console.error(err)
    } else {
      const path = program.dest + '/' + program.name;
      fs.writeFile(path + '.js', latest, function (err) {
        if (err) { console.error(err)
        } else {
        }
      });
    }
  });
};

const generateHaskellClient = (program, jsonSpecs) => {
  if (!jsonSpecs.length) {
    console.log('No specs');
    return;
  }

  const diffs = diffSpecs(jsonSpecs);

  const specs = supportedSpecs(program.prefix, Haskell.spec, diffs, jsonSpecs);
  if (!specs.length) {
    console.log('Spec support is too high')
    return;
  }

  const code = Haskell.client.gen(specs, program.addon ? program.addon.split(',') : []);

  mkdirp(program.dest, function (err) {
    if (err) { console.error(err)
    } else {
      const path = program.dest + '/' + program.name;
      fs.writeFile(path + '.hs', code, function (err) {
        if (err) { console.error(err)
        } else {
        }
      });
    }
  });
};

const generateHaskellServer = (program, jsonSpecs) => {
  if (!jsonSpecs.length) {
    console.log('No specs');
    return;
  }

  const diffs = diffSpecs(jsonSpecs);

  const specs = supportedSpecs(program.prefix, Haskell.spec, diffs, jsonSpecs);
  if (!specs.length) {
    console.log('Spec support is too high')
    return;
  }

  const latest = Haskell.server.latest(specs);

  const haskellPathBuilder = (path, major) => path + '/V' + major + '.hs';

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
              writeCode(Haskell.server.gen, haskellPathBuilder, path, specs);
            }
          });
        }
      });
    }
  });
};

const generate = (program, lang, side, gen) => {
  if (program.lang === lang && program.side === side &&
      program.name && program.name.length &&
      program.dest && program.dest.length &&
      program.src && program.src.length) {
    if (hasJsonExtension(program.src)) {
      var jsonSpecs = JSON.parse(fs.readFileSync(program.src, 'utf8'));
      if (!Array.isArray(jsonSpecs)) {
        console.log('JSON source is not an array of specs');
        return;
      }
      gen(program, jsonSpecs);
    } else {
      fs.readdir(program.src, function (err, items) {
        if (err) {
          console.log(err)
        } else {
          gen(program, readSpecs(program.src, items));
        }
      });
    }
    return true;
  }
  return false;
};

(function() {
  generate(program, 'javascript', 'client', generateJavascriptClient) ||
  generate(program, 'haskell', 'server', generateHaskellServer) ||
  generate(program, 'haskell', 'client', generateHaskellClient) ||
  (() => console.log('Bad args'))();
})();
