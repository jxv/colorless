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
  .option('-m --prefix [type]', 'Module name/prefix')
  .option('-e --side [type]', 'Client or server side code', 'client')
  .parse(process.argv);

const src = program.src;
const dest = program.dest;
const lang = program.lang;
const side = program.side;
const prefix = program.prefix;

var spec = JSON.parse(fs.readFileSync(src, 'utf8'));

console.log(Haskell.gen(Haskell.spec(prefix, spec)));
