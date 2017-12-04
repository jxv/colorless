"use strict";

exports.showStatsObj = require("util").inspect;

exports.statsMethod = function (m, s) {
  return s[m]();
};
