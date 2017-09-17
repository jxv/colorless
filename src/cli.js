#!/usr/bin/env node
'use strict';

const fs = require('fs');
var program = require('commander');
const Haskell = require('./haskell/gen.js');

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

console.log(Haskell.gen({
  module: 'Hello.World',
  version: { major: 0, minor: 0 },
  error: 'Goodbye',
  meta: 'User',
  name: 'Api',
  hollow: [
    { name: 'Goodbye', label: 'Goodbye', func: 'goodbye', output: '()' },
  ],
  struct: [
    { name: 'Hello',
      label: 'Hello',
      func: 'hello',
      output: 'T.Text',
      members: [
        { name: 'target', label: 'target', type:  'T.Text' },
      ]
    },
    { name: 'User',
      label: 'User',
      members: [
        { name: 'userId', label: 'userId', type: 'UUID'},
        { name: 'name', label: 'name', type: 'T.Text'},
        { name: 'email', label: 'email', type: 'Maybe Email'},
      ],
    },
  ],
  enumeration: [
    { name: 'Color',
      label: 'Color',
      enumerals: [
        { tag: 'Red', label: 'Red' },
        { tag: 'Blue', label: 'Blue' },
        { tag: 'Green', label: 'Green' },
        { tag: 'Custom', label: 'Custom',
          members: [
            { name: 'red', label: 'red', type: 'I.Word8' },
            { name: 'blue', label: 'blue', type: 'I.Word8' },
            { name: 'green', label: 'green', type: 'I.Word8' } ] }
      ],
    },
    { name: 'Move',
      label: 'Move',
      func: 'move',
      output: '()',
      enumerals: [
        { tag: 'Up', label: 'Up' },
        { tag: 'Down', label: 'Down' },
        { tag: 'Other', label: 'Other',
          members: [
            { name: 'angle', label: 'angle', type: 'Float' }
          ] },
      ],
    }
  ],
  wrap: [
    { name: 'Email',
      label: 'Email',
      type: 'T.Text',
      instances: {
        text: true,
        number: false
      },
    },
  ],
}));
