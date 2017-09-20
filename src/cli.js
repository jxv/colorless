#!/usr/bin/env node
'use strict';

const fs = require('fs');
var program = require('commander');
const Haskell = require('./haskell/index.js');

program
  .version('0.0.0')
  .option('-s --src [type]', 'Directory of colorless specs')
  .option('-d --dest [type]', 'Directory to generate code')
  .option('-l --lang [type]', 'Language of code')
  .option('-m --prefix [type]', 'Prefix or module name')
  .option('-e --side [type]', '\'client\' or \'server\' side code', 'client')
  .parse(process.argv);

(function() {
  if (program.lang === 'haskell' && program.side === 'server' && program.dest && program.src) {
    var jsonSpec = JSON.parse(fs.readFileSync(program.src, 'utf8'));
    var spec = Haskell.spec(program.prefix, jsonSpec);
    //console.log(Haskell.gen(spec);
    //console.log(Haskell.latest(spec));
    return;
  }

  console.log('Bad args');
})();
