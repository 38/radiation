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
			function raw(value) {
				var res = ""
				for(;value >= 32; value >>= 5) res += nonterm[value % 32]
				res += termset[value]
				return res
			}
			function vlq(value) {
				if(value < 0) return raw(-value * 2 + 1)
				else return raw(value * 2)
			}
			return function (value) {
				var d = value - last
				var dl = vlq(d)
				var al = raw(value)
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
		writeLog: function(tag, deps) {
			
		},
		nextId: function() {
			var nextId = 0
			return function () {
				return nextId ++
			}
		}(),
		wrap: function(value) {
			var deps = arguments
			return deps
		}
	}
}
