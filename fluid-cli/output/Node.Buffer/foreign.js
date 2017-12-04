/* global exports */
/* global Buffer */
/* global require */
"use strict";

exports.showImpl = require('util').inspect;

exports.create = function (size) {
  return function() {
    return new Buffer(size);
  };
};

exports.fromArray = function (octets) {
  return function() {
    return new Buffer(octets);
  };
};

exports.fromStringImpl = function (str) {
  return function (encoding) {
    return function() {
      return new Buffer(str, encoding);
    };
  };
};

exports.readImpl = function (ty) {
  return function (offset) {
    return function (buf) {
      return function() {
        return buf['read' + ty](offset);
      };
    };
  };
};

exports.readStringImpl = function (enc) {
  return function (start) {
    return function (end) {
      return function (buff) {
        return function() {
          return buff.toString(enc, start, end);
        };
      };
    };
  };
};

exports.toStringImpl = function (enc) {
  return function (buff) {
    return function() {
      return buff.toString(enc);
    };
  };
};

exports.writeImpl = function (ty) {
  return function (value) {
    return function (offset) {
      return function (buf) {
        return function() {
          buf['write' + ty](value, offset);
          return {};
        }
      };
    };
  };
};

exports.writeStringImpl = function (encoding) {
  return function (offset) {
    return function (length) {
      return function (value) {
        return function (buff) {
          return function() {
            return buff.write(value, offset, length, encoding);
          }
        };
      };
    };
  };
};

exports.toArray = function (buff) {
  return function() {
    var json = buff.toJSON()
    return json.data || json;
  };
};

exports.getAtOffsetImpl = function (just) {
  return function (nothing) {
    return function (offset) {
      return function (buff) {
        return function() {
          var octet = buff[offset];
          return octet == null ? nothing
                               : just(octet);
        };
      };
    };
  };
};

exports.setAtOffset = function (value) {
  return function (offset) {
    return function (buff) {
      return function() {
        buff[offset] = value;
        return {};
      };
    };
  };
};

exports.size = function (buff) {
  return function() {
    return buff.length;
  };
};



exports.concat = function (buffs) {
  return function() {
    return Buffer.concat(buffs);
  };
};

exports["concat'"] = function (buffs) {
  return function (totalLength) {
    return function() {
      return Buffer.concat(buffs, totalLength);
    };
  };
};

exports.copy = function (srcStart) {
  return function (srcEnd) {
    return function (src) {
      return function (targStart) {
        return function (targ) {
          return function() {
            return src.copy(targ, targStart, srcStart, srcEnd);
          };
        };
      };
    };
  };
};

exports.fill = function (octet) {
  return function (start) {
    return function (end) {
      return function (buf) {
        return function() {
          buf.fill(octet, start, end);
          return {};
        };
      };
    };
  };
};
