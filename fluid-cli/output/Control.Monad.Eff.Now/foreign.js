"use strict";

exports.now = function () {
  return Date.now();
};

exports.nowOffset = function () {
  var dt = new Date();
  return dt.getTimezoneOffset();
};
