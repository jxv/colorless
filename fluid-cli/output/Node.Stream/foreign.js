"use strict";

exports.undefined = undefined;

exports.setEncodingImpl = function (s) {
  return function (enc) {
    return function () {
      s.setEncoding(enc);
    };
  };
};

exports.readChunkImpl = function (Left) {
  return function (Right) {
    return function (chunk) {
      if (chunk instanceof Buffer) {
        return Right(chunk);
      } else if (typeof chunk === "string") {
        return Left(chunk);
      } else {
        throw new Error(
          "Node.Stream.readChunkImpl: Unrecognised " +
          "chunk type; expected String or Buffer, got: " +
          chunk);
      }
    };
  };
};

exports.onDataEitherImpl = function (readChunk) {
  return function (r) {
    return function (f) {
      return function () {
        r.on("data", function (data) {
          f(readChunk(data))();
        });
      };
    };
  };
};

exports.onEnd = function (s) {
  return function (f) {
    return function () {
      s.on("end", f);
    };
  };
};

exports.onFinish = function (s) {
  return function (f) {
    return function () {
      s.on("finish", f);
    };
  };
};

exports.onReadable = function (s) {
  return function (f) {
    return function () {
      s.on("readable", f);
    };
  };
};

exports.onError = function (s) {
  return function (f) {
    return function () {
      s.on("error", function (e) {
        f(e)();
      });
    };
  };
};

exports.onClose = function (s) {
  return function (f) {
    return function () {
      s.on("close", f);
    };
  };
};

exports.resume = function (s) {
  return function () {
    s.resume();
  };
};

exports.pause = function (s) {
  return function () {
    s.pause();
  };
};

exports.isPaused = function (s) {
  return function () {
    return s.isPaused();
  };
};

exports.pipe = function (r) {
  return function (w) {
    return function () {
      return r.pipe(w);
    };
  };
};

exports.readImpl = function (readChunk) {
  return function (Nothing) {
    return function (Just) {
      return function (r) {
        return function (s) {
          return function () {
            var v = r.read(s);
            if (v === null) {
              return Nothing;
            } else {
              return Just(readChunk(v));
            }
          };
        };
      };
    };
  };
};

exports.write = function (w) {
  return function (chunk) {
    return function (done) {
      return function () {
        return w.write(chunk, null, done);
      };
    };
  };
};

exports.writeStringImpl = function (w) {
  return function (enc) {
    return function (s) {
      return function (done) {
        return function () {
          return w.write(s, enc, done);
        };
      };
    };
  };
};

exports.cork = function (w) {
  return function () {
    return w.cork();
  };
};

exports.uncork = function (w) {
  return function () {
    return w.uncork();
  };
};

exports.setDefaultEncodingImpl = function (w) {
  return function (enc) {
    return function () {
      w.setDefaultEncoding(enc);
    };
  };
};

exports.end = function (w) {
  return function (done) {
    return function () {
      w.end(null, null, function () {
        done();
      });
    };
  };
};
