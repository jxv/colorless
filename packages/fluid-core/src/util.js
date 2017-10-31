const hasJsonExtension = (name) => {
  const s = name.split('.');
  return s[s.length - 1] === 'json';
};

const diffSpecs = (specs) => {
  var diffs = [];
  for (var i = 0; i < specs.length - 1; i++) {
    diffs.push(diff.diff(specs[i], specs[i + 1]));
  }
  return diffs;
};

const supportedSpecs = (prefix, specForLang, diffs, jsonSpecs) => {
  // by major version, extract the req specs and decide where to place the types
  var version = jsonSpecs[0].version || { major: 0, minor: 0 };
  var specs = [];

  var tyVers = [initTypeVersions(jsonSpecs[0].types.map(ty => ty.n), version)];

  for (var i = 0; i < jsonSpecs.length; i++) {
    const original = R.dissoc('types', R.merge(jsonSpecs[i], {version}));
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

const expandTypes = s => {
  const types = R.values(
    R.mapObjIndexed((ty, n) =>
        R.merge({n}, (() => {
            if (typeof ty === 'string') {
              return { w: ty };
            }
            if (Array.isArray(ty)) {
              return { e: (ty.map(e => typeof e === 'string' ? { tag: e } : e)) };
            }
            if (ty.e) {
              return R.merge(ty, { e: (ty.e.map(e => typeof e === 'string' ? { tag: e } : e)) });
            }
            return ty;
          })()
        ),
      s.schema));
  if (!types) {
    console.log('types: null', s.schema)
  }
  return R.merge(s, { types });
};

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
