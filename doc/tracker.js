/* if the global is not initliazed, initialize it */
try {
	$t
} catch (e) {
	/* The helper object */
	$t={
		/* Encode base64 VLQ integer */
		encode: function () {
			var termset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
			var nonterm = "ghijklmnopqrstuvwxyz0123456789+/"
			var last = 0
			var absolute = true
			/* generate an unsigned value */
			function unsigned(value) {
				var res = ""
				for(;value >= 32; value >>= 5) res += nonterm[value % 32]
				res += termset[value]
				return res
			}
			/* generate a signed VLQ value */
			function signed(value) {
				if(value < 0) return unsigned(-value * 2 + 1)
				else return unsigned(value * 2)
			}
			/* The log format have two kinds of integer, absolute value and differnt. And
			 * Dot(.) is the signal to toggle the two different format. At the begining of 
			 * the log, it's in absolute mode
			 * Each log item contains a tag id, a value id and a list of dependency, and the
			 * dependency ends with the two same tag:
			 * 10,1,2,3,4,4 ==> tag id = 10; value id = 1, deps = 2,3,4
			 */
			return function (value) {
				var d = value - last
				var dl = signed(d)
				var al = unsigned(value)
				var ret = ""
				if(absolute && dl.length + 1 < al.length) {
					absolute = false
					ret += "."
				}
				if(!absolute && al.length + 1 < dl.length) {
					absolute = true
					ret += "."
				}
				last = value
				if(absolute) return ret + al
				else return ret + dl
			}
		}(),
		/* the value store */
		values: function () {
			var v2t = {}
			var t2v = []
			var nextToken = 0
			return {
				/* get a token by its value, because the number of primitive are limitied
				 * So it's a good idea to create a primitive pool, and each log item refer
				 * the value from the pool */
				getTokenByValue: function(value) {
					return v2t[value]
				},
				/* get the value by it token */
				getValueByToken: function(token) {
					return t2v[token]
				},
				/* put the value to store, if value exist return the existing token */
				putValue: function(value) {
					if(value in v2t) return v2t[value]
					else {
						var token = nextToken ++
						v2t[value] = token
						t2v[token] = value
						return token
					}
				}
			}
		}(),
		/* The log utilies */
		log: function() {
			var log = ""
			var nextId = function () {
				var nextId = 1
				return function () {
					return nextId ++
				}
			}();
			return {
				/* Emit a new log item, basically recording what the value is and the dependencies
				 * It's a primitive emit when a new value has been created,
				 * If it's an object emit when the prototype or data structure is changed. The source of this change should be :
				 *   Object creation: it's prototype // affcted by the constructor 
				 *   Adding a new key   // affected by the key 
				 *   delete a key       // N/A
				 */
				emit: function(value, deps) {
					var tag = nextId()
					var valueId = $t.values.putValue(value)
					log += $t.encode(tag) + $t.encode(valueId)
					if(deps === null) log += $t.encode(0)
					else
					{
						for(var i = 0; i < valueId.length; i ++)
							if(deps[i] !== undefined) log += _t.encode(deps[i])
					}
					log += $t.encode(0)
					//console.debug(tag + "\t" + value + "\t" + deps)
					switch(typeof value) {
						case "string":
						case "boolean":
						case "number":
						case "undefined":
							return new $t.__primitive__(value, tag)
						default:
							if(value === null) return new $t.__primitive__(value, tag)
							value.__tracking_tag__ = tag
							return value
					}
				},
				get: function() {
					return log
				}
			}
		}(),
		__primitive__: function () {
			function constructor(what, tag) {
				this.value = what;
				this.__tracking_tag__ = tag
			}
			constructor.prototype = {
				value: null,
				__trackiong_tag__: null,
				unpack: function () {
					return this.value
				},
				tostring: function() {
					return "[Primitive " + this.value + "tag:" + this.__tracking_tag__ + "]"
				}
			}
			return constructor
		}(),
		unpack: function(what) {
			if(what instanceof this.__primitive__) return what.unpack()
			else return what
		},
		/* TODO export the value to external code, otherwise return the value itself */
		exportIfNeeded: function(what, target) {

		}
	}
}

function fib(n) {
	if(/* n <= 0 */$t.unpack(function() {
		var __read_1 = n
		var __result = $t.log.emit($t.unpack(__read_1) <= 0, [__read_1.__tracking_tag__])
		return __result   /* TODO if the arguments.callee.caller is cross the boundary */
	}())) return (/* 1 */function () {
		var __result = $t.log.emit(1)
		return __result
	}())
	else return (/* fib(n - 1) + fib(n - 2) */ function () {
		var __apply_1 = fib(/* n - 1 */function () {
			var __read_1 = n
			var __result = $t.log.emit($t.unpack(n) - 1, [__read_1.__tracking_tag__])
			return __result
		}())
		var __apply_2 = fib(/* n - 2 */ function () {
			var __read_1 = n
			var __result = $t.log.emit($t.unpack(n) - 2, [__read_1.__tracking_tag__])
			return __result
		}())
		var __result =  $t.log.emit($t.unpack(__apply_1) + $t.unpack(__apply_2), [__apply_1.__tracking_tag__, __apply_2.__tracking_tag__])
		return __result
	})()
}

function fibloop(n) {
	var a = function () {
		var __result = $t.log.emit(1)
		return __result
	}();
	var b = function () {
		var __result = $t.log.emit(1)
		return __result
	}();
	for(var i = function () {
		var __result = $t.log.emit(0)
		return __result
	}(); $t.unpack(function () {
		var __read_1 = i
		var __read_2 = n
		var __result = $t.log.emit($t.unpack(__read_1) < $t.unpack(__read_2) + 1, [__read_1.__tracking_tag__, __read_2.__tracking_tag__])
		return __result
	}()); $t.unpack(function () {
		var __read_1 = i;
		var __result = $t.log.emit($t.unpack(__read_1) + 1, [__read_1.__tracking_tag__])
		i = __result
		return __result
	}())) {
		var tmp = a;
		(function () {
			var __read_1 = a;
			var __read_2 = b;
			var __result = $t.log.emit($t.unpack(__read_1) + $t.unpack(__read_2), [__read_1.__tracking_tag__, __read_2.__tracking_tag__])
			a = __result
			return __result
		}());
		(function () {
			var __result = tmp;
			b = __result;
			return __result
		}());
	}
	return (function () {
		var __result = tmp;
		return __result
	}())
}
console.debug(fibloop(15))
