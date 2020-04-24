var ebisu = require('ebisu-js');

exports._defaultModel = ebisu.defaultModel;
exports._predictRecall = function(model) {
  return function(elapsed) {
    return ebisu.predictRecall(model, elapsed);
  }
}
exports._updateRecall = ebisu.updateRecall;

exports.binLowest = function(it, binMaxs, f) {
    binMaxs = binMaxs.slice().sort((a, b) => a - b);
    if (binMaxs[binMaxs.length - 1] !== Infinity) {
        binMaxs.push(Infinity);
    }
    const hits = Array.from(Array(binMaxs.length), _ => []);
    let hitsIdx = binMaxs.length - 1;
    for (const x of it) {
        const y = f(x);
        if (y < binMaxs[hitsIdx]) {
            hitsIdx = binMaxs.findIndex(max => y < max);
            hits[hitsIdx].push(x);
        }
    }
    return hits[hitsIdx];
}
exports.partialSort = function(it) {
  return function(K) {
    return function(f) {
      if (it instanceof Array) {
        it = it.values();
      }
      const ret = [];
      if (K <= 0) {
        return ret;
      }
      // fill the bag with the first N items from the iterator
      for (let i = 0; i < K; i++) {
        const { value, done } = it.next();
        ret.push({ x: value, y: f(value) });
        if (done) {
          return ret;
        }
      }
      // Loop through the rest of the iterator, adding it to the bag and kicking out the previous largest if it's smaller
      // than the largest
      const cmp = (a, b) => a.y - b.y;
      ret.sort(cmp);
      const last = K - 1;
      for (const x of it) {
        const y = f(x);
        if (y < ret[last].y) {
          ret[last] = { x, y };
          ret.sort(cmp);
        }
      }
      return ret;
    }
  }
}
