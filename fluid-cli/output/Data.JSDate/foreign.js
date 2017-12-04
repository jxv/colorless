/* global exports */
"use strict";

var createDate = function (y, m, d, h, mi, s, ms) {
  var date = new Date(Date.UTC(y, m, d, h, mi, s, ms));
  if (y >= 0 && y < 100) {
    date.setUTCFullYear(y);
  }
  return date;
};

var createLocalDate = function (y, m, d, h, mi, s, ms) {
  var date = new Date(y, m, d, h, mi, s, ms);
  if (y >= 0 && y < 100) {
    date.setFullYear(y);
  }
  return date;
};

exports.now = function () {
  return new Date();
};

exports.isValid = function (date) {
  return !isNaN(date.getTime());
};

exports.toInstantImpl = function (just) {
  return function (nothing) {
    return function (date) {
      var t = date.getTime();
      return isNaN(t) ? nothing : just(t);
    };
  };
};

exports.fromInstant = function (instant) {
  return new Date(instant);
};

exports.jsdate = function (parts) {
  return createDate(parts.year, parts.month, parts.day, parts.hour, parts.minute, parts.second, parts.millisecond);
};

exports.jsdateLocal = function (parts) {
  return function () {
    return createLocalDate(parts.year, parts.month, parts.day, parts.hour, parts.minute, parts.second, parts.millisecond);
  };
};

exports.dateMethod = function (method, date) {
  return date[method]();
};

exports.dateMethodEff = function (method, date) {
  return function () {
    return date[method]();
  };
};

exports.parse = function (dateString) {
  return function () {
    return new Date(dateString);
  };
};
