_$_next_tag_$_ = 0
function _$_pack_$_(what, deps) {
	console.debug(_$_next_tag_$_ + " << " + deps)
	return {
		_$_packed_$_: true,
		value: what,
		tag: _$_next_tag_$_ ++
	}
}
function _$_unpacked_$_(what) {
	return what.value
}
function _$_unpack_if_packed_$_(what) {
	if(what._$_packed_$_ == true) 
		return what.value
	return what
}

/** examples **/
function fib(n) {
	if(_$_unpack_if_packed_$_(n) < 1) return _$_pack_$_(1, [])
	return function () {
		var x = fib(n - 1)
		var y = fib(n - 2)
		var deps = [x.tag, y.tag]
		return _$_pack_$_(_$_unpack_if_packed_$_(x) + _$_unpack_if_packed_$_(y), deps)
	}()
}
