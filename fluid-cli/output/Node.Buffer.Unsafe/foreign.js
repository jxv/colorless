/* global exports */
/* global Buffer */
"use strict";

exports.slice = function (start) {
  return function (end) {
    return function (buff) {
      return buff.slice(start, end);
    };
  };
};
