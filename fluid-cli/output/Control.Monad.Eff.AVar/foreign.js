/* globals exports, setTimeout */
/* jshint -W097 */

"use strict";

var AVar = function () {
  function MutableQueue () {
    this.head = null;
    this.last = null;
    this.size = 0;
  }

  function MutableCell (queue, value) {
    this.queue = queue;
    this.value = value;
    this.next  = null;
    this.prev  = null;
  }

  function AVar (value) {
    this.draining = false;
    this.error    = null;
    this.value    = value;
    this.takes    = new MutableQueue();
    this.reads    = new MutableQueue();
    this.puts     = new MutableQueue();
  }

  var EMPTY = {};

  function putLast (queue, value) {
    var cell = new MutableCell(queue, value);
    switch (queue.size) {
    case 0:
      queue.head = cell;
      break;
    case 1:
      cell.prev = queue.head;
      queue.head.next = cell;
      queue.last = cell;
      break;
    default:
      cell.prev = queue.last;
      queue.last.next = cell;
      queue.last = cell;
    }
    queue.size++;
    return cell;
  }

  function takeLast (queue) {
    var cell;
    switch (queue.size) {
    case 0:
      return null;
    case 1:
      cell = queue.head;
      queue.head = null;
      break;
    case 2:
      cell = queue.last;
      queue.head.next = null;
      queue.last = null;
      break;
    default:
      cell = queue.last;
      queue.last = cell.prev;
      queue.last.next = null;
    }
    cell.prev = null;
    cell.queue = null;
    queue.size--;
    return cell.value;
  }

  function takeHead (queue) {
    var cell;
    switch (queue.size) {
    case 0:
      return null;
    case 1:
      cell = queue.head;
      queue.head = null;
      break;
    case 2:
      cell = queue.head;
      queue.last.prev = null;
      queue.head = queue.last;
      queue.last = null;
      break;
    default:
      cell = queue.head;
      queue.head = cell.next;
      queue.head.prev = null;
    }
    cell.next = null;
    cell.queue = null;
    queue.size--;
    return cell.value;
  }

  function deleteCell (cell) {
    if (cell.queue === null) {
      return;
    }
    if (cell.queue.last === cell) {
      takeLast(cell.queue);
      return;
    }
    if (cell.queue.head === cell) {
      takeHead(cell.queue);
      return;
    }
    if (cell.prev) {
      cell.prev.next = cell.next;
    }
    if (cell.next) {
      cell.next.prev = cell.prev;
    }
    cell.queue.size--;
    cell.queue = null;
    cell.value = null;
    cell.next  = null;
    cell.prev  = null;
  }

  function drainVar (util, avar) {
    if (avar.draining) {
      return;
    }

    var ps = avar.puts;
    var ts = avar.takes;
    var rs = avar.reads;
    var p, r, t, value, rsize;

    avar.draining = true;

    /* jshint -W084 */
    while (1) {
      p = null;
      r = null;
      t = null;
      value = avar.value;
      rsize = rs.size;

      if (avar.error !== null) {
        value = util.left(avar.error);
        while (p = takeHead(ps)) {
          runEff(p.cb(value));
        }
        while (r = takeHead(rs)) {
          runEff(r(value));
        }
        while (t = takeHead(ts)) {
          runEff(t(value));
        }
        break;
      }

      // Process the next put. We do not immediately invoke the callback
      // because we want to preserve ordering. If there are takes/reads
      // we want to run those first.
      if (value === EMPTY && (p = takeHead(ps))) {
        avar.value = value = p.value;
      }

      if (value !== EMPTY) {
        // We go ahead and queue up the next take for the same reasons as
        // above. Invoking the read callbacks can affect the mutable queue.
        t = takeHead(ts);
        // We only want to process the reads queued up before running these
        // callbacks so we guard on rsize.
        while (rsize-- && (r = takeHead(rs))) {
          runEff(r(util.right(value)));
        }
        if (t !== null) {
          avar.value = EMPTY;
          runEff(t(util.right(value)));
        }
      }

      if (p !== null) {
        runEff(p.cb(util.right(void 0)));
      }

      // Callbacks could have queued up more items so we need to guard on the
      // actual mutable properties.
      if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) {
        break;
      }
    }
    /* jshint +W084 */

    avar.draining = false;
  }

  function runEff(eff) {
    try {
      eff();
    } catch (error) {
      setTimeout(function () {
        throw error;
      }, 0);
    }
  }

  AVar.EMPTY      = EMPTY;
  AVar.putLast    = putLast;
  AVar.takeLast   = takeLast;
  AVar.takeHead   = takeHead;
  AVar.deleteCell = deleteCell;
  AVar.drainVar   = drainVar;

  return AVar;
}();

exports.makeEmptyVar = function () {
  return new AVar(AVar.EMPTY);
};

exports.makeVar = function (value) {
  return function () {
    return new AVar(value);
  };
};

exports._killVar = function (util, error, avar) {
  return function () {
    if (avar.error === null) {
      avar.error = error;
      avar.value = AVar.EMPTY;
      AVar.drainVar(util, avar);
    }
  };
};

exports._putVar = function (util, value, avar, cb) {
  return function () {
    var cell = AVar.putLast(avar.puts, { cb: cb, value: value });
    AVar.drainVar(util, avar);
    return function () {
      AVar.deleteCell(cell);
    };
  };
};

exports._takeVar = function (util, avar, cb) {
  return function () {
    var cell = AVar.putLast(avar.takes, cb);
    AVar.drainVar(util, avar);
    return function () {
      AVar.deleteCell(cell);
    };
  };
};

exports._readVar = function (util, avar, cb) {
  return function () {
    var cell = AVar.putLast(avar.reads, cb);
    AVar.drainVar(util, avar);
    return function () {
      AVar.deleteCell(cell);
    };
  };
};

exports._tryPutVar = function (util, value, avar) {
  return function () {
    if (avar.value === AVar.EMPTY && avar.error === null) {
      avar.value = value;
      AVar.drainVar(util, avar);
      return true;
    } else {
      return false;
    }
  };
};

exports._tryTakeVar = function (util, avar) {
  return function () {
    var value = avar.value;
    if (value === AVar.EMPTY) {
      return util.nothing;
    } else {
      avar.value = AVar.EMPTY;
      AVar.drainVar(util, avar);
      return util.just(value);
    }
  };
};

exports._tryReadVar = function (util, avar) {
  return function () {
    if (avar.value === AVar.EMPTY) {
      return util.nothing;
    } else {
      return util.just(avar.value);
    }
  };
};

exports._status = function (util, avar) {
  return function () {
    if (avar.error) {
      return util.killed(avar.error);
    }
    if (avar.value === AVar.EMPTY) {
      return util.empty;
    }
    return util.filled(avar.value);
  };
};

