'use strict';

const fs = require('fs');
const R = require('ramda');
var diff = require('../src/diff.js');

var v0 = JSON.parse(fs.readFileSync('./examples/HelloWorld/0.json', 'utf8'));
var v1 = JSON.parse(fs.readFileSync('./examples/HelloWorld/1.json', 'utf8'));
var v2 = JSON.parse(fs.readFileSync('./examples/HelloWorld/2.json', 'utf8'));
var v3 = JSON.parse(fs.readFileSync('./examples/HelloWorld/3.json', 'utf8'));

describe('diff', () => {
})
