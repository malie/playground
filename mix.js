/* Javascript object system base supporting mixins.
 * See the tests below.
 *
 * Written by Markus Liedl, `markusliedl' on twitter.
 */

function arrayDo(xs, f) {
  for (var i = 0; i < xs.length; i++)
    f(xs[i]);
}
function arrayDoWithIndices(array, f) {
  for (var i = 0; i < array.length; i++)
    f(array[i], i);
}
function arrayMap(xs, f) {
  var res = [];
  for (var i = 0; i < xs.length; i++)
    res.push(f(xs[i]));
  return res;
}


/* The global variable `______supr' holds the super method during the
 * activity of any method that overrides another method. When the
 * overriding method calls super the method stored there gets
 * activated.  Dynamically scoped.
 */
var ______supr = null;

/*
 * Produces a function to wrap any overriding method in such a way
 * that `______supr' is bound to the super method
 */
function withSuperBoundTo(sup, meth) {
  return function() {
    var outer_super = ______supr;
    ______supr = sup;
    var res = meth.apply(this, arguments);
    ______supr = outer_super;
    return res;
  }
}

/* `combineBehaviors' takes the `name' of the class to be built, and a
 * ordered list of objects, containing the behavioral aspects. All
 * those get combinded, overriding methods get detected and wrapped
 * via `withSuperBoundTo'. The result is constructor producing a flat
 * object, passing its arguments along the method `initialize', if
 * there is one.
 */
function combineBehaviors(name, ps) {
  // constructor for all objects
  var res = function () { }

  // one method available to all objects: `supr' for activating the
  // super method; shortcut;
  res.prototype.supr = function () { return ______supr.apply(this, arguments) }

  for (var i = 0; i < ps.length; i++) {
    var parent = ps[i];
    for (var slotname in parent) {
      var prev = res.prototype[slotname];
      var val = parent[slotname];
      if (prev != undefined) {
	// method name exist in some super class
	val = withSuperBoundTo(prev, val);
      } else {
	if (/this\.supr/.test(val.toString())) {
	  // the method may not call super if there is no super implementation.
	  throw 'unbound *supr* in method of `'+name +'\': ' + val;
	}
      }
      res.prototype[slotname] = val;
    }
  }
  if (res.prototype.initialize)
    return function () {
      var x = new res();
      x.initialize.apply(x, arguments);
      return x }
  else
    return function () { return new res(); }
}

// object system base
var baseObjectDescription = {
  type: 'class',
  name: 'baseObject',
  supers: [],
  methods: {
    initialize: function () { }
  },
  toString: function () { return '<class of base object>' }
};

function parentsLinearized(desc) {
  function generatePairs(d0) {
    var pairs = [];
    function pushPair(a,b) {
      pairs.push({
	left: a,
	right: b,
	toString: function () { return '(' + this.left + ', ' + this.right + ')'}})}
    function rec(d) {
      arrayDo(d.supers, function (sup) { pushPair(d, sup) });
      for (var i = 0; i < d.supers.length; i++) {
	var i1 = i+1;
	pushPair(d.supers[i], i1==d.supers.length ? null : d.supers[i1]);
      }
      arrayDo(d.supers, rec);
    }
    pushPair(d0, null);
    rec(d0);
    return pairs;
  }
  
  function printPairs() {
    console.log('----------------------------------------');
    for (var p in pairs) {
      console.log(pairs[p].toString())}}

  function isPreceeded(pairs, cand) {
    for (var i in pairs) {
      if (pairs[i].right == cand)
	return true}
    return false}
  function findUnpreceeded(pairs) {
    for (var i in pairs) {
      var candidate = pairs[i].left;
      if (!isPreceeded(pairs, candidate))
	return candidate}
    return null}
  function removePairs(pairs, par) {
    var res = [];
    for (var i in pairs) {
      var pair = pairs[i];
      if (pair.left != par && pair.right != par)
	res.push(pair)}
    return res}

  var pairs = generatePairs(desc)
  var parents = [];
  while (pairs.length > 0) {
    // printPairs();
    var par = findUnpreceeded(pairs);
    if (par == null)
      throw 'inconsistent behavior hierarchy! ' + pairs;
    parents.push(par);
    pairs = removePairs(pairs, par);
  }
  console.log('   pl: ' + parents);
  return parents;
}


function defmixin(name, behavior) {
  return {
    type: 'mixin',
    name: name,
    supers: [baseObjectDescription],
    methods: behavior,
    toString: function () { return '<mixin ' + name + '>' }
  };
}

function defclass(name, supers, behavior) {
  return {
    type: 'class',
    name: name,
    supers: supers,
    methods: behavior,
    toString: function () { return '<class ' + name + '>' },
    instantiate: function () {
      var parents = parentsLinearized(this);
      var bs = [];
      for (var i = parents.length-1; i >= 0; i--)
	bs.push(parents[i].methods);
      this.instantiate = combineBehaviors(this.name, bs);
      return this.instantiate.apply(this, arguments);
    }
  };
}




if (this.runTests) {
(function () {
function equalityForTests (a,b) {
  if (a instanceof Array && b instanceof Array) {
    if (a.length != b.length)
      return false;
    for (var i in a)
      if (!equalityForTests(a[i], b[i]))
	return false;
    return true
  }
  return a == b;
}

var numFailures = 0;
function testEqual(nm, expectedResult, realResult) {
  var msg = null;
  if (!equalityForTests(expectedResult, realResult)) {
    msg = 'FAILURE: ' + nm + '\n   expected:' + expectedResult + '\n   but got: ' + realResult;
    numFailures++;
  } else msg = 'ok: ' + nm;
  if (msg != null)
    console.log(msg);
}


function bench(f) {
  function run(msg) {
    var start = new Date();
    f();
    var end = new Date();
    var elapsed = end.getTime() - start.getTime();
    console.log(msg + ' spent ' + elapsed + ' for ' + f);
  }
  run('first');
  run('second');
  run('third');
}


// basics
testEqual('apply1', 23, (function () { return (function (a,b) { return a*10+b}).apply(null, arguments)})(2,3));


var parents = [
  { initialize: function () { this.init1 = 3 },
    _nm: 'parent 1',
    toString: function () { return 'parent 1'},
    m1: function () { return 234 },
    m4: function () { return 77 },
    getIvar2: function () { return this.ivar1; },
    getIvar3: function () { return this.ivar1; },
  },
  { initialize: function () { this.supr(); this.init2 = 4 },
    _nm: 'parent 2',
    toString: function () { return 'parent 2'},
    m2: function () { return 33 },
    m3: function () { return 88 },
    m4: function () { return this.supr() },
    setIvar1: function (x) { this.ivar1 = x; },
    getIvar1: function () { return this.ivar1; },
    getIvar3: function () { return this.supr(); }
  },
];


// inheritance
var x = combineBehaviors('x', parents)();
testEqual('init1', 3, x.init1);
testEqual('init2', 4, x.init2);

testEqual('m1', 234, x.m1());
testEqual('m2', 33, x.m2());
testEqual('m3', 88, x.m3());
testEqual('m4', 77, x.m4());

// instance variables
x.setIvar1(34);
testEqual('iv1', 34, x.getIvar1());
testEqual('iv2', 34, x.getIvar2());
testEqual('iv3', 34, x.getIvar3());

// initialize with arguments
var p2 = combineBehaviors('p2',
  [{ initialize: function (a) { this.a = a; },
     threeA: function () { return 3*this.a; } },
   { initialize: function (a,b,c) { this.supr(a); this.b = b; this.c = c; },
     foo: function () { return 100*this.a + 10*this.b + this.c; } }]);
var x = p2(5,6,7);
testEqual('x.a', 5, x.a);
testEqual('x.b', 6, x.b);
testEqual('x.c', 7, x.c);
testEqual('x.threeA()', 15, x.threeA());
testEqual('x.foo()', 567, x.foo());



// mixins
var compare = defmixin('compare',
  { lte: function (o) { return this.lt(o) || this.equals(o) },
    gt: function (o) { return !this.lte(o) },
    gte: function (o) { return !this.lt(o) },
  });
var n = defclass('n', [compare], 
  { initialize: function (x) { this.x = x; },
    lt: function (o) { return this.x < o.x },
    equals: function (o) { return this.x == o.x }});

var mx = defclass('mx', [compare], {});
var my = defclass('my', [], {});
var nmx = defclass('nmx', [n,mx], { });
var nmy = defclass('nmy', [n,compare,my], {});

testEqual('pl(n)', [n, compare, baseObjectDescription], parentsLinearized(n));
testEqual('pl(nmx)', [nmx, n, mx, compare, baseObjectDescription], parentsLinearized(nmx));
testEqual('pl(nmy)', [nmy, n, compare, my, baseObjectDescription], parentsLinearized(nmy));

testEqual('3 < 5', true, n.instantiate(3).lt(n.instantiate(5)));
testEqual('3 <= 5', true, n.instantiate(3).lte(n.instantiate(5)));
testEqual('3 <= 3', true, n.instantiate(3).lte(n.instantiate(3)));
testEqual('3 < 5', false, n.instantiate(3).gt(n.instantiate(5)));


var gtepar = defmixin('gtepar', {
    gte: function (o) { return '((' + this.supr(o) + '))' }
});

var gtetxt = defmixin('gtetxt', {
    gte: function (o) { return 'gte of super: ' + this.supr(o) }
});

var pt = defclass('pt', [gtepar, gtetxt, n], {});
var tp = defclass('tp', [gtetxt, gtepar, n], {});


testEqual('pt 3 >= 5', '((gte of super: false))', pt.instantiate(3).gte(pt.instantiate(5)));
testEqual('tp 3 >= 5', 'gte of super: ((false))', tp.instantiate(3).gte(tp.instantiate(5)));


if (numFailures > 0)
  console.error('failed tests: ' + numFailures);
})()}
