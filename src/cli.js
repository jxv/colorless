#!/usr/bin/env node
'use strict';

const fs = require('fs');
const mkdirp = require('mkdirp');
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

(function() {
  if (
      program.lang === 'haskell' &&
      program.side === 'server' &&
      program.name && program.name.length &&
      program.dest && program.dest.length &&
      program.src && program.src.length) {
    const jsonSpec = JSON.parse(fs.readFileSync(program.src, 'utf8'));
    const spec = Haskell.spec(program.prefix, jsonSpec);

    const v0 = Haskell.gen(spec);
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
                fs.writeFile(path + '/V0.hs', v0, function (err) {
                  if (err) { console.error(err) } else { }
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

const nestedDirectories = deepest => {
  const separated = deepest.split('/');
  var list = [];
  for (var i = 0; i < separated.length; i++) {

    list.push();
  }
  return list;
};
