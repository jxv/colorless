const R = require('ramda');

// differences:
//   actions: add, modify, remove
//   target:
//     * type
//     * enumeration: tag and/or members, output
//     * struct: members, output
//     * hollow: output
//     * wrap: wrap, output


const getType = ty => {
  if (ty.w) {
    return 'wrap';
  }
  if (ty.m) {
    return 'struct';
  }
  if (ty.e) {
    return 'enumeration';
  }
  return 'hollow';
};

const hasOutput = ty => !!ty.o;

const diff = (prev, next) => {
  const prevTypes = R.mergeAll(prev.types.map(ty => ({ [ty.n]: ty })));
  const nextTypes = R.mergeAll(next.types.map(ty => ({ [ty.n]: ty })));
  const prevNames = R.keys(prevTypes);
  const nextNames = R.keys(nextTypes);

  const addType = R.without(prevNames, nextNames);
  const removeType = R.without(nextNames, prevNames);
  const sameType = R.intersection(prevNames, nextNames);

  var modifyType = [];
  var modifyWrap = [];
  var modifyStruct = [];
  var modifyEnumeration = [];
  sameType.forEach(name => {
    const prevType = getType(prevTypes[name]);
    const nextType = getType(nextTypes[name]);
    const ty = nextType;

    const prev = prevTypes[name];
    const next = nextTypes[name];

    if (prevType !== nextType) {
      modifyType.push(name);
    } else if (ty === 'wrap' && (!R.equals(prev.w, next.w) || (prev.o && !R.equals(prev.o, next.o)))) {
      modifyWrap.push(name);
    } else if (ty === 'struct' && (!R.equals(prev.m, next.m) || (prev.o && !R.equals(prev.o, next.o)))) {
      modifyStruct.push(name);
    } else if (ty === 'enumeration' && (!R.equals(prev.e, next.e) || (prev.o && !R.equals(prev.o, next.o)))) {

      const prevEnumerals = R.mergeAll(prev.e.map(e => ({ [e.tag]: e })));
      const nextEnumerals = R.mergeAll(next.e.map(e => ({ [e.tag]: e })));

      const prevTags = R.keys(prevEnumerals);
      const nextTags = R.keys(nextEnumerals);

      const addTags = R.without(prevTags, nextTags);
      const removeTags = R.without(nextTags, prevTags);
      const sameTags = R.intersection(prevTags, nextTags);

      var modifyEnumeral = [];
      sameTags.forEach(tag => {
        if (!R.equals(prevEnumerals[tag], nextEnumerals[tag])) {
          modifyEnumeral.push(tag);
        }
      });

      if (addTags.length || removeTags.length || modifyEnumeral.length) {
        const item = {
          name: name,
          addEnumeral: addTags,
          removeEnumeral: removeTags,
          modifyEnumeral: modifyEnumeral,
          modifyOutput: !!prev.o && !R.equals(prev.o, next.o),
        };
        modifyEnumeration.push(item);
      }
    }
  });

  return {
    addType: R.without(modifyType, addType),
    removeType,
    modifyType,
    modifyWrap,
    modifyStruct,
    modifyEnumeration,
  };
};

module.exports = {
  diff,
};
