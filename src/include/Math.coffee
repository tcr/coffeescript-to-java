Math: {
	E: `new CSNumber(java.lang.Math.E)`
	LN10: `new CSNumber(java.lang.Math.LN10)`
	LN2: `new CSNumber(java.lang.Math.LN2)`
	LOG2E: `new CSNumber(java.lang.Math.LOG2E)`
	LOG10E: `new CSNumber(java.lang.Math.LOG10E)`
	PI: `new CSNumber(java.lang.Math.PI)`
	SQRT1_2: `new CSNumber(java.lang.Math.SQRT1_2)`
	SQRT2: `new CSNumber(java.lang.Math.SQRT2)`

	abs: (x) ->
		`return new CSNumber(java.lang.Math.abs(asNumber(${x})));`
	acos: (x) ->
		`return new CSNumber(java.lang.Math.acos(asNumber(${x})));`
	asin: (x) ->
		`return new CSNumber(java.lang.Math.asin(asNumber(${x})));`
	atan: (x) ->
		`return new CSNumber(java.lang.Math.atan(asNumber(${x})));`
	atan2: (x) ->
		`return new CSNumber(java.lang.Math.atan2(asNumber(${x})));`
	ceil: (x) ->
		`return new CSNumber(java.lang.Math.ceil(asNumber(${x})));`
	cos: (x) ->
		`return new CSNumber(java.lang.Math.cos(asNumber(${x})));`
	exp: (x) ->
		`return new CSNumber(java.lang.Math.exp(asNumber(${x})));`
	floor: (x) ->
		`return new CSNumber(java.lang.Math.floor(asNumber(${x})));`
	log: (x) ->
		`return new CSNumber(java.lang.Math.log(asNumber(${x})));`
	max: (args...) ->
		`double max = Number.NEGATIVE_INFINITY;`
		for arg in args
			`max = java.lang.Math.max(asNumber(${arg}));`
		`return new CSNumber(max);`
	min: (args...) ->
		`double min = Number.POSITIVE_INFINITY;`
		for arg in args
			`min = java.lang.Math.min(asNumber(${arg}));`
		`return new CSNumber(min);`
	pow: (x, y) ->
		`return new CSNumber(java.lang.Math.pow(asNumber(${x}), asNumber(${y})));`
	random: ->
		`return new CSNumber(java.lang.Math.random());`
	round: (x) ->
		`return new CSNumber(java.lang.Math.round(asNumber(${x})));`
	sin: (x) ->
		`return new CSNumber(java.lang.Math.sin(asNumber(${x})));`
	sqrt: (x) ->
		`return new CSNumber(java.lang.Math.sqrt(asNumber(${x})));`
	tan: (x) ->
		`return new CSNumber(java.lang.Math.tan(asNumber(${x})));`
}