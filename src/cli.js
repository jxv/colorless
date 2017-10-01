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
  .option('-s --src [type]', 'Directory of colorless specs')
  .option('-d --dest [type]', 'Directory to generate code')
  .option('-n --name [type]', 'Name of top level source file and directory')
  .option('-l --lang [type]', 'Language of code')
  .option('-m --prefix [type]', 'Prefix or module name')
  .option('-e --side [type]', '\'client\' or \'server\' side code', 'client')
  .option('-v --major [type]', 'Oldest supported major version', '0')
  .parse(process.argv);

const hasJsonExtension = (name) => {
  const s = name.split('.');
  return s[s.length - 1] === 'json';
};

const generateJavascriptClient = (program, items) => {
  const files = items.filter(hasJsonExtension);
  const jsonSpecs = files.map(file => expandTypes(JSON.parse(fs.readFileSync(program.src + '/' + file, 'utf8'))));

  if (!jsonSpecs.length) {
    console.log('No specs');
    return;
  }

  // diff the specs
  var diffs = [];
  for (var i = 0; i < jsonSpecs.length - 1; i++) {
    diffs.push(diff.diff(jsonSpecs[i], jsonSpecs[i + 1]));
  }

  // by major version, extract the req specs and decide where to place the types
  var version = { major: 0, minor: 0 };
  var specs = [];
  var tyVers = [initTypeVersions(jsonSpecs[0].types.map(ty => ty.n))];

  for (var i = 0; i < jsonSpecs.length; i++) {
    specs.push(JavaScript.spec(version, jsonSpecs[i]));
    if (i < diffs.length) {
      const change = typeChanges(diffs[i]);
      version = nextVersion(version, versionChange(change));
      tyVers.push(nextTypeVersion(tyVers[i], version, change));
    }
  }

  const reqTyVers = dropLowMinors(tyVers);
  const reqSpecs = attachTypeSources(specs, reqTyVers);
  const supportedSpecs = reqSpecs.filter(({version}) => version.major >= +program.major);
  if (!supportedSpecs.length) {
    console.log('Spec support is too high')
    return;
  }
  const latest = JavaScript.clientLatest(supportedSpecs);

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

const generateHaskellServer = (program, items) => {
  const files = items.filter(hasJsonExtension);
  const jsonSpecs = files.map(file => expandTypes(JSON.parse(fs.readFileSync(program.src + '/' + file, 'utf8'))));

  if (!jsonSpecs.length) {
    console.log('No specs');
    return;
  }

  // diff the specs
  var diffs = [];
  for (var i = 0; i < jsonSpecs.length - 1; i++) {
    diffs.push(diff.diff(jsonSpecs[i], jsonSpecs[i + 1]));
  }

  // by major version, extract the req specs and decide where to place the types
  var version = { major: 0, minor: 0 };
  var specs = [];
  var tyVers = [initTypeVersions(jsonSpecs[0].types.map(ty => ty.n))];
  for (var i = 0; i < jsonSpecs.length; i++) {
    specs.push(Haskell.spec(program.prefix, version, jsonSpecs[i]));
    if (i < diffs.length) {
      const change = typeChanges(diffs[i]);
      version = nextVersion(version, versionChange(change));
      tyVers.push(nextTypeVersion(tyVers[i], version, change));
    }
  }

  const reqTyVers = dropLowMinors(tyVers);
  const reqSpecs = attachTypeSources(specs, reqTyVers);
  const supportedSpecs = reqSpecs.filter(({version}) => version.major >= +program.major);
  if (!supportedSpecs.length) {
    console.log('Spec support is too high')
    return;
  }
  const latest = Haskell.server.latest(supportedSpecs);

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
              writeCode(path, reqSpecs);
            }
          });
        }
      });
    }
  });
};

(function() {
  if (program.lang === 'javascript' && program.side === 'client' &&
      program.name && program.name.length &&
      program.dest && program.dest.length &&
      program.src && program.src.length) {
    fs.readdir(program.src, function (err, items) {
      if (err) {
        console.log(err)
      } else {
        generateJavascriptClient(program, items);
      }
    });
    return;
  }

  if (program.lang === 'haskell' && program.side === 'server' &&
      program.name && program.name.length &&
      program.dest && program.dest.length &&
      program.src && program.src.length) {
    fs.readdir(program.src, function (err, items) {
      if (err) {
        console.log(err)
      } else {
        generateHaskellServer(program, items);
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
  const code = Haskell.server.gen(specs[0]);
  const filePath = path + '/V' + specs[0].version.major + '.hs';
  fs.writeFile(filePath, code, function (err) {
    if (err) { console.error(err)
    } else {
      writeCode(path, specs.slice(1));
    }
  });
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

const initTypeVersions = types => ({
  version: { major: 0, minor: 0 },
  types: R.mergeAll(types.map(ty => ({ [ty]: { major: 0, minor: 0 } }))),
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
